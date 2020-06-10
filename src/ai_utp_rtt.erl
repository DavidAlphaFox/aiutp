-module(ai_utp_rtt).

-export([rto/1,ack/4,lost/2]).

-define(MAX_WINDOW_INCREASE, 3000).
-define(DEFAULT_RTT_TIMEOUT, 200).

-record(ai_utp_rtt, {
                     rtt = 500 :: integer(),
                     var = 800 :: integer(),
                     delay = 1.5
                    }).


%% Every packet that is ACKed, either by falling in the range
%% (last_ack_nr, ack_nr] or by explicitly being acked by a Selective
%% ACK message, should be used to update an rtt (round trip time) and
%% rtt_var (rtt variance) measurement. last_ack_nr here is the last
%% ack_nr received on the socket before the current packet, and ack_nr
%% is the field in the currently received packet.

%% The rtt and rtt_var is only updated for packets that were sent only
%% once. This avoids problems with figuring out which packet was
%% acked, the first or the second one.

%% rtt and rtt_var are calculated by the following formula, every time
%% a packet is ACKed:
lost(none,_) -> #ai_utp_rtt{};
lost(#ai_utp_rtt{delay = Delay} = RTT,LostCount)->
  Delay0 =
    if LostCount > 0 -> Delay * 1.5;
       true  -> Delay / 2
    end,
  if Delay0 > 8 -> RTT#ai_utp_rtt{delay = 8};
     Delay0 < 2 -> RTT#ai_utp_rtt{delay = 1.5};
     true -> RTT#ai_utp_rtt{delay = Delay}
  end.

update(Estimate,#ai_utp_rtt { rtt = LastRTT, var = Var} ) ->
  Delta = LastRTT - Estimate,
  #ai_utp_rtt {
     rtt = round(LastRTT - LastRTT/8 + Estimate/8),
     var = round(Var + (abs(Delta) - Var) / 4) };

update(Estimate,none)->
  #ai_utp_rtt {
     rtt = round(Estimate),
     var = round(Estimate / 2)}.

%% 计算重新传输超时
%% The default timeout for packets associated with the socket is also
%% updated every time rtt and rtt_var is updated. It is set to:
rto(none) -> ?DEFAULT_RTT_TIMEOUT;
rto(#ai_utp_rtt { rtt = RTT, var = Var}) ->
  RTO = erlang:max(RTT + Var * 4, ?DEFAULT_RTT_TIMEOUT),
  RTO.


%% ACKnowledge an incoming packet
ack(RTT, TimeSent, TSDiff, TimeAcked) ->
  if
    TimeAcked >= TimeSent->
      Estimate = ai_utp_util:bit32(TimeAcked - TimeSent + TSDiff) div 1000,
      NewRTT = update(Estimate, RTT),
      NewRTO = rto(NewRTT),
      {ok, NewRTO, NewRTT};
    true ->
      RTO = rto(RTT),
      {ok,RTO,RTT}
  end.

%% Every time a socket sends or receives a packet, it updates its
%% timeout counter. If no packet has arrived within timeout number of
%% milliseconds from the last timeout counter reset, the socket
%% triggers a timeout. It will set its packet_size and max_window to
%% the smallest packet size (150 bytes). This allows it to send one
%% more packet, and this is how the socket gets started again if the
%% window size goes down to zero.

%% The initial timeout is set to 1000 milliseconds, and later updated
%% according to the formula above. For every packet consecutive
%% subsequent packet that times out, the timeout is doubled.

