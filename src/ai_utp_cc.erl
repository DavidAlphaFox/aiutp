-module(ai_utp_cc).
-include("ai_utp.hrl").
-export([cc/7]).

%% 对Peer的计算
update_peer_ledbat(Net,0)-> Net;
update_peer_ledbat(#utp_net{ peer_ledbat = none } = Net, Sample) ->
  Net#utp_net{ peer_ledbat = ai_utp_ledbat:new(Sample) };
update_peer_ledbat(#utp_net{ peer_ledbat = Ledbat } = Net, Sample) ->
  Net#utp_net{ peer_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.


%% if their new delay base is less than their previous one
%% we should shift our delay base in the other direction in order
%% to take the clock skew into account
update_clock_skew(#utp_net{ peer_ledbat = none }, NW) -> NW;
update_clock_skew(#utp_net{peer_ledbat = OldPeers },
           #utp_net{peer_ledbat = Peers,
                    our_ledbat   = Ours
                   } = NW) ->
  OldDelayBase = ai_utp_ledbat:base_delay(OldPeers),
  DelayBase = ai_utp_ledbat:base_delay(Peers),
  Diff = OldDelayBase - DelayBase,
  IsLess = ai_utp_ledbat:compare_less(DelayBase,OldDelayBase),
  % never adjust more than 10 milliseconds
  if IsLess == true andalso Diff < 10000 ->
      NW#utp_net{ our_ledbat = ai_utp_ledbat:shift(Ours, Diff) };
     true -> NW
  end.

update_reply_micro(Net,Now,TS)->
  ReplyMicro =
    if TS > 0 -> ai_utp_util:bit32(Now - TS);
       true -> 0
    end,
  Net0 = update_peer_ledbat(Net#utp_net{reply_micro = ReplyMicro},ReplyMicro),
  update_clock_skew(Net, Net0).


%% 对自己的计算
update_our_ledbat(Net,0)-> Net;
update_our_ledbat(#utp_net{ our_ledbat = none } = Net, Sample) ->
  Net#utp_net{ our_ledbat = ai_utp_ledbat:new(Sample) };
update_our_ledbat(#utp_net{ our_ledbat = Ledbat } = Net, Sample) ->
  Net#utp_net{ our_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.


%% if the delay estimate exceeds the RTT, adjust the base_delay to
%% compensate
update_estimate_exceed(#utp_net{our_ledbat = Ours} = NW,MinRTT) ->
  OurDelay = ai_utp_ledbat:get_value(Ours),
  Diff = OurDelay - MinRTT,
  if
    Diff > 0 -> NW#utp_net{our_ledbat = ai_utp_ledbat:shift(Ours, Diff) };
    true-> NW
  end.

congestion_control(#utp_net{our_ledbat = OurLedbat,max_window = MaxWindow,
                            opt_sndbuf = OptSndBuf,
                            last_maxed_out_window = LastMaxedOutWindow }=Net,
                   AckedBytes, NowMS,MinRTT)->
  OurDelay = min(MinRTT,ai_utp_ledbat:get_value(OurLedbat)),
  TargetDelay = ?CONGESTION_CONTROL_TARGET,

  TargetOffset = TargetDelay - OurDelay,
  %% this is the same as:
  %%
  %%    (min(off_target, target) / target) * (bytes_acked / max_window) * MAX_CWND_INCREASE_BYTES_PER_RTT
  %%
  %% so, it's scaling the max increase by the fraction of the window this ack represents, and the fraction
  %% of the target delay the current delay represents.
  %% The min() around off_target protects against crazy values of our_delay, which may happen when th
  %% timestamps wraps, or by just having a malicious peer sending garbage. This caps the increase
  %% of the window size to MAX_CWND_INCREASE_BYTES_PER_RTT per rtt.
  %% as for large negative numbers, this direction is already capped at the min packet size further down
  %% the min around the bytes_acked protects against the case where the window size was recently
  %% shrunk and the number of acked bytes exceeds that. This is considered no more than one full
  %% window, in order to keep the gain within sane boundries.
  WindowFactor = min(AckedBytes, MaxWindow) / max(MaxWindow, AckedBytes),

  %% The delay factor is how much we are off the target:
  DelayFactor = TargetOffset / TargetDelay,
  ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,
  ScaledGain0 =
    if (NowMS - LastMaxedOutWindow) > 1000 andalso ScaledGain > 0 -> 0;
       true -> ScaledGain
    end,
  LedbatCwnd = ai_utp_util:clamp(MaxWindow + ScaledGain0,?MIN_WINDOW_SIZE,OptSndBuf),
  Net#utp_net{max_window = LedbatCwnd}.

cc(#utp_net{cur_window = CurWindow} = Net,
   TS,TSDiff,Now,MinRTT,AckBytes,WndSize)->
  TSDiff0 = ai_utp_util:bit32(TSDiff),
  ActualDelay =
    if TSDiff0 == ?TS_DIFF_MAX -> 0;
       true -> TSDiff0
    end,
  Net0 = update_reply_micro(Net, Now, TS),
  Net1 = update_our_ledbat(Net0, ActualDelay),
  Net2 = update_estimate_exceed(Net1, MinRTT),
  Net3 =
    if ActualDelay > 0 andalso AckBytes > 0 ->
        congestion_control(Net2,AckBytes,Now / 1000,MinRTT);
       true -> Net2
    end,
  CurWindow0 = CurWindow - AckBytes,
  CurWindow1 =
    if CurWindow0 >= 0 -> CurWindow0;
       true -> 0
    end,
  Net3#utp_net{max_peer_window = WndSize,
               cur_window = CurWindow1 }.
