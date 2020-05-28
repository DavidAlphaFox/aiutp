-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/4]).

schedule_ack(Type,SeqNR,Actions)->
  if SeqNR >= ?REORDER_BUFFER_MAX_SIZE ->
      Max = ai_utp_util:bit16((?SEQ_NO_MASK + 1) - ?REORDER_BUFFER_MAX_SIZE),
      if SeqNR >= Max andalso Type == st_state ->
          [schedule_ack|Actions];
         true -> Actions
      end;
     true -> Actions
  end.

acked_size(AckNo,SeqNR,CurWindowPackets)->
  Acks = ai_utp_util:bit16(CurWindowPackets - (SeqNR - 1 - AckNo)),
  if Acks > CurWindowPackets -> 0;
     true -> Acks
  end.

acked_packet(OutBuf,Seq,Now,RTT,Bytes)->
  case array:get(Seq,OutBuf) of
    undefined -> {RTT,Bytes};
    WrapPacket ->
      if WrapPacket#utp_packet_wrap.transmissions > 0 ->
          Bytes0 = Bytes + WrapPacket#utp_packet_wrap.payload,
          SendTime = WrapPacket#utp_packet_wrap.send_time,
          RTT0 =
            if SendTime < Now -> min(RTT, Now - SendTime );
               true -> min(RTT,50000)
            end,
          {RTT0,Bytes0};
         true -> {RTT,Bytes}
      end
  end.
%% 常规的ack
acked_bytes(_,_,_,RTT,-1,Bytes)-> {RTT,Bytes};
acked_bytes(OutBuf,WindowStart,Now,RTT,Acks,Bytes) ->
  Seq = ai_utp_util:bit16(WindowStart + Acks),
  {RTT0,Bytes0}  = acked_packet(OutBuf,Seq, Now, RTT, Bytes),
  acked_bytes(OutBuf,WindowStart,Now,RTT0,Acks - 1,Bytes0).


acked(Bits,Len)->
  Pos = Len bsr 3,
  Mask = 1 bsr (Len band 7),
  if
    Pos > 0 ->
      Ignore = (Pos - 1) * 8,
      <<_:Ignore,Bit/big-integer,_/binary>> = Bits,
      Bit;
    true ->
      <<Bit/big-integer,_/binary>> = Bits,
      Bit
  end,
  Bit band Mask.

%% SeqNR -1  - (AckNo + 2 + Len)  >= cur_window_packets - 1
%% selective ack
sacked(_,_,_,_,_,-1,_,RTT,Bytes)->{RTT,Bytes};
sacked(OutBuf,SeqNR,CurWindowPackets,Base,Bits,Len,Now,RTT,Bytes)->
  Index = Base + Len,
  Count = ai_utp_util:bit16(SeqNR - 1 - Index),
  if Count >= CurWindowPackets - 1 -> {RTT,Bytes};
     true ->
      Acked = acked(Bits,Len),
      if
        Acked == 0 ->
          sacked(OutBuf,SeqNR,CurWindowPackets,Base,Bits,
                 Len-1,Now,RTT,Bytes);
        true ->
          {RTT0,Bytes0}  = acked_packet(OutBuf,Index, Now, RTT, Bytes),
          sacked(OutBuf,SeqNR,CurWindowPackets,Base,Bits,
                 Len-1,Now,RTT0,Bytes0)
      end
  end.
sacked_bytes(_,_,_,_,_,RTT,[],Bytes) -> {RTT,Bytes};
sacked_bytes(OutBuf,SeqNR,CurWindowPackets,
             Base,Now,RTT,[H|T],Bytes) ->
  case H of
    {sack,Bits}->
      Len = erlang:byte_size(Bits) * 8 - 1,
      sacked(OutBuf,SeqNR,CurWindowPackets,Base,
               Bits,Len,Now,RTT,Bytes);
    _ ->
      sacked_bytes(OutBuf,SeqNR,CurWindowPackets,
                   Base,Now,RTT,T, Bytes)
  end.

acked_bytes(OutBuf,SeqNR,CurWindowPackets,AckNo,Now,RTT,Acks,SAcks)->
  WindowStart = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  {MinRTT,AckedBytes} = acked_bytes(OutBuf,WindowStart,Now,RTT,Acks - 1,0),
  AckNo0 = ai_utp_util:bit16(AckNo + 2),
  sacked_bytes(OutBuf,SeqNR,CurWindowPackets,AckNo0,
               Now,MinRTT,SAcks,AckedBytes).


update_our_ledbat(Net,0)-> Net;
update_our_ledbat(#utp_net{ our_ledbat = none } = Net, Sample) ->
  Net#utp_net{ our_ledbat = ai_utp_ledbat:new(Sample) };
update_our_ledbat(#utp_net{ our_ledbat = Ledbat } = Net, Sample) ->
  Net#utp_net{ our_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.

update_peer_ledbat(Net,0)-> Net;
update_peer_ledbat(#utp_net{ peer_ledbat = none } = Net, Sample) ->
  Net#utp_net{ peer_ledbat = ai_utp_ledbat:new(Sample) };
update_peer_ledbat(#utp_net{ peer_ledbat = Ledbat } = Net, Sample) ->
  Net#utp_net{ peer_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.

update_reply_micro(Net,Now,TS)->
  ReplyMicro =
    if TS > 0 -> ai_utp_util:bit32(Now - TS);
       true -> 0
    end,
  update_peer_ledbat(Net#utp_net{reply_micro = ReplyMicro},ReplyMicro).


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
                   AckedBytes, ActualDelay, NowMS,MinRTT)->
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


update_fast_resend_seq_nr(#utp_net{fast_resend_seq_nr = FastResendSeqNR} = Net,AckNo)->
  NextAckNo = ai_utp_util:bit16(AckNo + 1),
  IsLess = ai_utp_util:wrapping_compare_less(
             FastResendSeqNR,NextAckNo,?ACK_NO_MASK),
  if
    IsLess == true ->
      Net#utp_net{fast_resend_seq_nr = NextAckNo};
    true -> Net
  end.


update_net('SYN_SENT',Net,#utp_packet{seq_no = SeqNo})->
  Net#utp_net{ack_nr = ai_utp_util:bit16(SeqNo-1)};
update_net(_,Net,_) -> Net.

pure_packet(#utp_net{outbuf = OutBuf,rtt = RTT, rtt_ledbat = RTTLedbat,
                     cur_window = CurWindow,
                     cur_window_packets = CurWindowPackets} = Net,Seq,Now)->
  case array:get(Seq,OutBuf) of
    undefined -> {continue,
                  Net#utp_net{cur_window_packets = CurWindowPackets -1}};
    WrapPacket ->
      if
        WrapPacket#utp_packet_wrap.transmissions == 0 ->
          {stop,Net};
        true ->
          Net0 =
            if WrapPacket#utp_packet_wrap.transmissions == 1 ->
                TimeSent = WrapPacket#utp_packet_wrap.send_time,
                {ok, _NewRTO, NewRTT, NewHistory} =
                  ai_utp_rtt:ack(RTTLedbat,RTT,TimeSent,Now),
                Net#utp_net{rtt = NewRTT,rtt_ledbat = NewHistory};
               true -> Net
            end,
          Net1 =
            if WrapPacket#utp_packet_wrap.need_resend == true -> Net0;
               true ->
                Net0#utp_net{cur_window = CurWindow - WrapPacket#utp_packet.payload}
            end,
          {continue,Net1#utp_net{cur_window_packets = CurWindowPackets -1,
                       retransmit_count = 0,
                       outbuf = array:set(Seq,undefined,OutBuf)}}
      end
  end.

pure_acked(Net,_,-1)-> Net;
pure_acked(#utp_net{cur_window_packets = CurWindowPackets,
                    seq_nr = SeqNR} = Net,Now,Acks)->
  Seq = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  case pure_packet(Net, Seq, Now) of
    {stop,Net0} -> Net0;
    {continue,Net0} ->
      pure_acked(Net0,Now,Acks - 1)
  end.

sacked_resend(Index,FastResendSeqNR,Count,Acc)->
  Out = ai_utp_util:bit16(Index - FastResendSeqNR),
  if Out =< ?OUTGOING_BUFFER_MAX_SIZE andalso
     Count >= ?DUPLICATE_ACKS_BEFORE_RESEND ->
      [Index|Acc];
     true  -> Acc
  end.

pure_sacked(Net,_,_,_,-1,Acc)-> {Net,Acc};
pure_sacked(#utp_net{cur_window_packets = CurWindowPackets,
                     seq_nr = SeqNR,fast_resend_seq_nr = FastResendSeqNR
                    }=Net,Now,Base,
            Bits,Len,{Count,Acc}) ->
  Index = ai_utp_util:bit16(Base + Len),
  Count = ai_utp_util:bit16(SeqNR - 1 - Index),
  if Count >= CurWindowPackets - 1 ->  Net;
     true ->
      Acked = acked(Bits,Len),
      if Acked == true->
          {_,Net0} = pure_packet(Net, Index, Now),
          pure_sacked(Net0,Now,Base,Bits,Len -1,{Count + 1,Acc});
         true ->
          Acc0 = sacked_resend(Index,FastResendSeqNR,Count,Acc),
          pure_sacked(Net,Now,Base,Bits,Len - 1,{Count,Acc0 })
      end
  end.


pure_sacked(Net,_,Base,[],Acc) -> {Net,Acc};
pure_sacked(Net,Now,Base,[H|T],Acc) ->
  case H of
    {sack,Bits}->
      Len = erlang:byte_size(Bits) * 8 - 1,
      {Net0,{Count,Acc0}} = pure_sacked(Net,Now,Base,Bits,Len,{0,Acc}),
      #utp_net{fast_resend_seq_nr = FastResendSeqNR} = Net0,
      Out = ai_utp_util:bit16(Base - 1),
      Acc1 = sacked_resend(Out, FastResendSeqNR, Count, Acc0),
      {Net0#utp_net{duplicate_ack = Count},Acc1};
    _ -> pure_sacked(Net,Now,Base,T,Acc)
  end.
nagle_send(#utp_net{outbuf = OutBuf,seq_nr = SeqNR,
                    cur_window_packets = CurWindowPackets},Actions)->
  if CurWindowPackets == 1 -> Actions;
     true ->
      case array:get(SeqNR - 1,OutBuf) of
        undefined -> Actions;
        WrapPacket ->
          if WrapPacket#utp_packet_wrap.transmissions == 0 ->
              [{transmist,SeqNR - 1}|Actions];
             true -> Actions
          end
      end
  end.
fast_timeout(#utp_net{fast_timeout = false} = Net,Actions) ->
  {Net,Actions};
fast_timeout(#utp_net{seq_nr = SeqNR, cur_window_packets = CurWindowPackets,
                      outbuf = OutBuf,fast_resend_seq_nr = FastResendSeqNR} = Net,
             Actions) ->
  Oldest = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  if Oldest /= FastResendSeqNR ->
      {Net#utp_net{fast_timeout = false},Actions};
     true ->
      case array:get(Oldest,OutBuf) of
        undefined -> {Net,Actions};
        WrapPacket ->
          if WrapPacket#utp_packet_wrap.transmissions > 0 ->
              {Net,[{transmist,Oldest}|Actions]};
             true -> {Net,Actions}
          end
      end
  end.


process_incoming(State,NetBase,
                 #utp_packet{type = Type,seq_no = SeqNo,
                             ack_no = AckNo,wnd_size = WndSize,
                             extension = Ext} = Packet,
                 {TS,TSDiff,Now} = Timing) ->
  Net = update_net(State,NetBase,Packet),
  #utp_net{ack_nr = AckNR,seq_nr = SeqNR,
           cur_window_packets = CurWindowPackets,
           outbuf = OutBuf,fin_sent = FinSent} = Net,
  NowMS = ai_utp_util:millisecond(),
  SeqWindow = ai_utp_util:bit16(SeqNo - AckNR - 1),
  Actions = schedule_ack(Type, SeqWindow, []),
  Acks = acked_size(AckNo,SeqNR,CurWindowPackets),
  {MinRTT,AckedBytes} = acked_bytes(OutBuf,SeqNR, CurWindowPackets,AckNo,
                           Now,?RTT_MAX,Acks,Ext),
  Net0 = update_reply_micro(Net, Now, TS),
  Net1 = update_clock_skew(Net, Net0),
  TSDiff0 = ai_utp_util:bit32(TSDiff),
  ActualDelay =
    if TSDiff == ?TS_DIFF_MAX -> 0;
       true -> TSDiff0
    end,
  Net2 = update_our_ledbat(Net1, ActualDelay),
  Net3 = update_estimate_exceed(Net2, MinRTT),
  Net4 =
    if ActualDelay > 0 andalso AckedBytes > 0 ->
        congestion_control(Net3,AckedBytes,ActualDelay,NowMS,MinRTT);
       true -> Net3
    end,
  Net5 = Net4#utp_net{max_peer_window = WndSize},
  Actions0 =
    if WndSize == 0->
        [{max_peer_window,NowMS}|Actions];
       true -> Actions
    end,
  Actions1 =
    if State == 'SYN_RECV' andalso Type == st_data ->
        [{next_state,'CONNECTED'}|Actions0];
       true -> Actions0
    end,
  Actions2 =
    if State == 'SYN_SENT' andalso Type == st_state ->
        [{next_state,'CONNECTED'}|Actions1];
       FinSent == true andalso Acks == CurWindowPackets ->
        [{next_state,'DESTROY'}|proplists:delete(next_state, Actions1)];
       true -> Actions1
    end,
  Net6 = update_fast_resend_seq_nr(Net5, AckNo),
  Net7 = pure_acked(Net6,Now,Acks),
  Actions3 = nagle_send(Net7,Actions2),
  {Net8,Actions4} = fast_timeout(Net7, Actions3),
  AckNo0 = ai_utp_util:bit16(AckNo + 2),
  {Net9,NeedResend} = pure_sacked(Net8,Now,AckNo0,Ext,[]),
  
  


%%recv_packet(State,Net,Packet,Timing)->
