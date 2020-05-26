-module(ai_utp_stream).
-include("ai_utp.hrl").

-export([new/2,new/3]).
-export([recv_packet/5,send_packet/4,send_packet/5]).


-define(INITIAL_CWND, 3000).
% ms, we can only decay the window at this time
-define(MAX_WINDOW_DECAY, 100).

% ms, perhaps we should run this in us
-define(CONGESTION_CONTROL_TARGET, 100).
%% 每个来回最大可以增加3000 bytes的窗口
% bytes
-define(MAX_CWND_INCREASE_BYTES_PER_RTT, 3000).
%% 最小的传输窗口是3000 bytes 
% bytes
-define(MIN_WINDOW_SIZE, 3000).

%% The default RecvBuf size: 8K
-define(OPT_RECV_BUF, 8192).
-define(REORDER_BUFFER_MAX_SIZE, 511).

-define(OUTGOING_BUFFER_MAX_SIZE, 511).
-define(PACKET_SIZE, 350).
-define(OPT_SEND_BUF, ?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE).

%% The deafult delays of acks
-define(DELAYED_ACK_BYTE_THRESHOLD, 2400). % bytes
-define(DELAYED_ACK_TIME_THRESHOLD, 100).  % ms

-record(packet_wrap, {
                      packet,
                      transmissions = 0,
                      send_time = 0,
                      need_resend = false
                     }).
-record(options,{
                 recv_buf_size,
                 send_buf_size,
                 %% Current packet size. We can alter the packet size if we want, but we
                 %% cannot repacketize.
                 packet_size :: integer()
                }).

-record(buffer,{
                recv_buf = queue:new(),
                reorder_buf = [],
                retrans_queue = [],
                expected_seq_no = 1,
                seq_no = 1,
                fin = none
               }).

-record(network,{
                 peer_conn_id,
                 %% Size of the window the Peer advertises to us.
                 peer_window = 4096 :: integer(),
                 %% The current window size in the send direction, in bytes.
                 window :: integer(),
                 %% Maximal window size int the send direction, in bytes.
                 %% Also known as the current congestion window
                 cwnd :: integer(),

                 
                 %% Current value to reply back to the other end
                 reply_micro :: integer(),

                 %% Round trip time measurements and LEDBAT
                 min_rtt = 30000000 :: integer(),
                 round_trip = none,
                 rtt_ledbat = none,
                 our_ledbat = none,
                 peer_ledbat = none,
                 %% Timeouts,
                 %% --------------------
                 %% Set when we update the zero window to 0 so we can reset it to 1.
                 zero_window_timeout :: none | {set, reference()},

                 %% Timestamps
                 %% --------------------
                 %% When was the window last totally full (in send direction)
                 last_maxed_out_window :: integer(),
                 last_window_decay :: integer()
                }).

-record(state,{
               options,
               buffer,
               network
              }).

new_send_buf_size(undefined,undefined) -> ?OPT_SEND_BUF;
new_send_buf_size(PacketSize,undefined) ->
  PacketSize * ?OUTGOING_BUFFER_MAX_SIZE;
new_send_buf_size(_,SendBufSize) -> SendBufSize.
new_recv_buf_size(undefined) -> ?OPT_RECV_BUF;
new_recv_buf_size(RecvBufSize) -> RecvBufSize.
new_packet_size(undefined) -> ?PACKET_SIZE;
new_packet_size(PacketSize) -> PacketSize.

new(PeerConnID,InitialSeqNo)->
  new(PeerConnID,InitialSeqNo,[]).

new(PeerConnID,InitialSeqNo,Options)->
  PacketSize = proplists:get_value(packet_size, Options),
  RecvBufSize = proplists:get_value(recv_buf_size, Options),
  SendBufSize = proplists:get_value(send_buf_size, Options),
  Now = ai_utp_util:millisecond(),
  Options = #options{
               recv_buf_size = new_recv_buf_size(RecvBufSize),
               send_buf_size = new_send_buf_size(PacketSize,SendBufSize),
               packet_size = new_packet_size(PacketSize)
              },
  Network = #network {
               peer_conn_id = PeerConnID,
               reply_micro = 0,
               round_trip = none,
               cwnd = ?INITIAL_CWND,
               last_maxed_out_window = Now - 300,
               last_window_decay     = Now
              },
  #state{
     options = Options,
     network = Network,
     buffer = #buffer{seq_no = InitialSeqNo}
    }.


validate_seq_no(SeqNo, #buffer{ expected_seq_no = Expected }) ->
  Diff = ai_utp_util:bit16(SeqNo - Expected),
  DiffMinusOne   = ai_utp_util:bit16(SeqNo - (Expected - 1)),
  case Diff of
    _SeqAhead when DiffMinusOne == 0 ->
      {ok, no_data};
    SeqAhead when SeqAhead >= ?REORDER_BUFFER_SIZE ->
      {error, is_far_in_future};
    SeqAhead -> {ok, SeqAhead}
  end.

is_fin(st_fin,SeqNo,Buf)-> Buf#buffer { fin = {got_fin, SeqNo} };
is_fin(_,_,Buf) -> Buf.



%% @doc Update the Receive Buffer with Payload
%% There are essentially two cases: Either the packet is the next
%% packet in sequence, so we can simply push it directly to the
%% receive buffer right away. Then we can check the reorder buffer to
%% see if we can satisfy more packets from it. If it is not in
%% sequence, it should go into the reorder buffer in the right spot.
%% @end
%% 已经收到fin了
update_recv_buffer(SeqNo, <<>>,
                   #buffer { fin = {got_fin, SeqNo},
                             expected_seq_no = SeqNo } = Buffer) ->
  {got_fin, Buffer#buffer { expected_seq_no = ai_utp_util:bit16(SeqNo+1)}};
%% 空包，可能是状态包
update_recv_buffer(_SeqNo, <<>>, Buffer) -> {ok, Buffer};
%% 已经收到fin包，并且新包也是fin
update_recv_buffer(SeqNo, Payload,
                   #buffer { fin = {got_fin, SeqNo},
                             expected_seq_no = SeqNo } = Buffer) ->
  Buffer0 = recv_buffer_enqueue(Payload, Buffer),
  {got_fin, Buffer0#buffer {expected_seq_no = ai_utp_util:bit16(SeqNo+1)}};
update_recv_buffer(SeqNo, Payload,
                   #buffer { expected_seq_no = SeqNo } = Buffer) ->
    Buffer0 = recv_buffer_enqueue(Payload, Buffer),
    satisfy_from_reorder_buffer(
      Buffer0#buffer { expected_seq_no = ai_utp_util:bit16(SeqNo+1) });
update_recv_buffer(SeqNo, Payload, Buffer) when is_integer(SeqNo) ->
  reorder_buffer_in(SeqNo, Payload,Buffer).

recv_buffer_enqueue(Payload,
                    #buffer { recv_buf = Q } = Buffer) ->
    Buffer#buffer { recv_buf = queue:in(Payload, Q) }.
%% @doc Try to satisfy the next_expected_seq_no directly from the reorder buffer.
%% @end
satisfy_from_reorder_buffer(#buffer{ reorder_buf = [] } = Buffer) ->
  {ok, Buffer};
satisfy_from_reorder_buffer(#buffer {
                               expected_seq_no = AckNo,
                               fin = {got_fin, AckNo},
                               reorder_buf = [{AckNo, PL} | R]} = Buffer) ->
  Buffer0 = recv_buffer_enqueue(PL, Buffer),
  {got_fin, Buffer0#buffer {expected_seq_no = ai_utp_util:bit16(AckNo+1),
                            reorder_buf = R}};
satisfy_from_reorder_buffer(#buffer{
                               expected_seq_no = AckNo,
                               reorder_buf = [{AckNo, PL} | R]} = Buffer) ->
    Buffer0 = recv_buffer_enqueue(PL, Buffer),
    satisfy_from_reorder_buffer(
      Buffer0#buffer {expected_seq_no = ai_utp_util:bit16(AckNo+1),
                     reorder_buf = R});
satisfy_from_reorder_buffer(Buffer) ->{ok, Buffer}.

%% @doc Enter the packet into the reorder buffer, watching out for duplicates
%% @end
reorder_buffer_in(SeqNo, Payload,
                  #buffer { reorder_buf = OD } = Buffer) ->
  case orddict:is_key(SeqNo, OD) of
    true -> duplicate;
    false ->
      {ok, Buffer#buffer { reorder_buf = orddict:store(SeqNo, Payload, OD) }}
  end.


consider_send_ack(#buffer { reorder_buf = RB1,
                            expected_seq_no = Seq1 },
                  #buffer { reorder_buf = RB2,
                            expected_seq_no = Seq2})
  when RB1 =/= RB2 orelse Seq1 =/= Seq2 ->
  [{send_ack, true}];
consider_send_ack(_, _) -> [].
       
handle_receive(SeqNo, Payload, Buffer) ->
  case update_recv_buffer(SeqNo, Payload, Buffer) of
    %% Force an ACK out in this case
    duplicate -> {Buffer, [{send_ack, true}]};
    {ok,Buffer0} -> {Buffer0, consider_send_ack(Buffer,Buffer0)};
    {got_fin, Buffer0} ->
        %% *Always* ACK the FIN packet!
        {Buffer0, [{got_fin, true},
                  {send_ack, true}]}
    end.

handle_incoming(SeqNo,Payload,Buffer)->
  case validate_seq_no(SeqNo, Buffer) of
    {ok,no_data} -> no_data;
    {ok,_} -> {ok,handle_receive(SeqNo,Payload,Buffer)};
    Error -> throw(Error)
  end.

send_window(#buffer { retrans_queue = RQ }) -> length(RQ).
%% @doc View the state of the Ack
%% Given the `AckNo' and when the `WindowStart' started, we scrutinize the Ack
%% for correctness according to age. If the ACK is old, tell the caller.
%% @end
view_ack_no(AckNo, WindowStart, WindowSize) ->
  case ai_utp_util:bit16(AckNo - WindowStart) of
    %% The ack number is old, so do essentially nothing in the next part
    N when N > WindowSize -> {ack_is_old, N};
    N when is_integer(N) -> {ok, N}
  end.
view_ack_state(0, _PB) -> [];
view_ack_state(N, PB) when is_integer(N) ->
  case has_inflight(PB) of
    true ->[{data_inflight, true}];
    false ->[{all_acked, true}]
  end.

%% 是否有没ack的数据
has_inflight(#buffer { retrans_queue = [] }) -> false;
has_inflight(#buffer { retrans_queue = [_|_] }) -> true.

contains_fin([]) -> false;
contains_fin([#packet_wrap {packet = #packet { type = st_fin }} | _]) ->
    true;
contains_fin([_ | R]) -> contains_fin(R).
%% @doc Prune the retransmission queue for ACK'ed packets.
%% Prune out all packets from `WindowStart' and `AcksAhead' in. Return a new packet
%% buffer where the retransmission queue has been updated.
%% @todo All this AcksAhead business, why? We could as well just work directly on
%%       the ack_no I think.
%% @end
prune_acked(AckAhead, WindowStart,
            #buffer { retrans_queue = RQ } = Buffer) ->
  {AckedPs, NewRQ} =
    lists:partition(fun(#packet_wrap{packet = #packet { seq_no = SeqNo } }) ->
                        Distance = ai_utp_util:bit16(SeqNo - WindowStart),
                        Distance =< AckAhead
                    end,RQ),

  RetState =
    case contains_fin(AckedPs) of
      true -> fin_sent_acked;
      false -> ok
    end,
  {RetState, AckedPs, Buffer#buffer { retrans_queue = NewRQ }}.

update_send_buffer(AckNo, #buffer { seq_no = NextSeqNo } = Buffer) ->
  %% 最后一个包的序号
  SeqNo = ai_utp_util:bit16(NextSeqNo - 1),
  %% 还有多少包没有发送
  WindowSize = send_window(Buffer),
  %% 计算出第一个还没有被ack的包的序号
  WindowStart = ai_utp_util:bit16(SeqNo - WindowSize),
  case view_ack_no(AckNo, WindowStart, WindowSize) of
    {ok, AcksAhead} ->
      {Ret, AckedPs, Buffer0} = prune_acked(AcksAhead, WindowStart, Buffer),
      FinState =
        case Ret of
          ok -> [];
          fin_sent_acked -> [fin_sent_acked]
        end,
      {ok, FinState ++ view_ack_state(length(AckedPs), Buffer0),
       AckedPs,Buffer0};
    {ack_is_old, _AcksAhead} ->
      {ok, [{old_ack, true}], [], Buffer}
  end.

update_peer_window(PKI, WindowSize) -> PKI#network { peer_window = WindowSize }.
update_reply_micro(#network { peer_ledbat = TL } = SockInfo, RU) ->
  SockInfo#network { reply_micro = RU,
                     peer_ledbat = ai_utp_ledbat:add_sample(TL, RU) }.



handle_clock_skew(#network { peer_ledbat = none }, NW) -> NW;
handle_clock_skew(#network {peer_ledbat = OldPeers },
                  #network {peer_ledbat = Peers,
                            our_ledbat   = Ours
                           } = NW) ->
  OldDelayBase = ai_utp_ledbat:base_delay(OldPeers),
  DelayBase = ai_utp_ledbat:base_delay(Peers),
  Diff = OldDelayBase - DelayBase,
  case ai_utp_ledbat:compare_less(DelayBase,OldDelayBase) of
    true when Diff < 10000 ->
      NW#network { our_ledbat = ai_utp_ledbat:shift(Ours, Diff) };
    true -> NW;
    false -> NW
  end.
update_our_ledbat(#network { our_ledbat = none } = SockInfo, Sample) ->
  SockInfo#network { our_ledbat = ai_utp_ledbat:new(Sample) };
update_our_ledbat(#network { our_ledbat = Ledbat } = SockInfo, Sample) ->
  SockInfo#network { our_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.
handle_estimate_exceed(#network { min_rtt = MinRtt,
                                  our_ledbat = Ours
                                } = NW) ->
  OurDelay = ai_utp_ledbat:get_value(Ours),
  Diff = OurDelay - MinRtt,
  if
    Diff > 0 ->
      NW#network {our_ledbat = ai_utp_ledbat:shift(Ours, Diff) };
    true-> NW
  end.

extract_rtt(Packets) ->
  [TS || #packet_wrap { send_time = TS} = P <- Packets,
         P#packet_wrap.transmissions == 1].

extract_payload_size(Packets) ->
    lists:sum([byte_size(Pl) ||
                #packet_wrap{ packet = #packet { payload = Pl } } <- Packets]).
ack_packet_rtt(#network { round_trip = RTT,
                          min_rtt    = MinRTT,
                          rtt_ledbat = LedbatHistory } = NW,
               TimeSent, TimeAcked) ->
  {ok, _NewRTO, NewRTT, NewHistory} =
    ai_utp_rtt:ack(LedbatHistory,RTT,TimeSent,TimeAcked),
  NW#network { round_trip = NewRTT,
               min_rtt = min(TimeAcked - TimeSent, MinRTT),
               rtt_ledbat     = NewHistory}.

consider_last_maxed_window(LastMaxedOutTime) ->
  Now = ai_utp_util:millisecond(),
  %% 300毫秒内跑满了自己的窗口
  case Now - LastMaxedOutTime < 300 of
    true -> too_soon;
    false -> ok
  end.
congestion_control(_,NW, 0) ->
  NW; %% Nothing acked, so skip maintaining the congestion control
congestion_control(Opts,#network {cwnd = Cwnd,our_ledbat = OurHistory,
                             min_rtt = MinRtt,
                             last_maxed_out_window = LastMaxedOutTime } = Network,
                   BytesAcked) ->
  if
    MinRtt > 0 -> ok;
    true -> error({min_rtt_violated,MinRtt})
  end,
  OurDelay =
    case min(MinRtt, ai_utp_ledbat:get_value(OurHistory)) of
      O when O >= 0 ->O;
      Otherwise ->
        error({our_delay_violated, Otherwise})
    end,
  TargetDelay = ?CONGESTION_CONTROL_TARGET,

  TargetOffset = TargetDelay - OurDelay,
  %% Compute the Window Factor. The window might have shrunk since
  %% last time, so take the minimum of the bytes acked and the
  %% window maximum.  Divide by the maximal value of the Windows and
  %% the bytes acked. This will yield a ratio which tells us how
  %% full the window is. If the window is 30K and we just acked 10K,
  %% then this value will be 10/30 = 1/3 meaning we have just acked
  %% 1/3 of the window. If the window has shrunk, the same holds,
  %% but opposite. We must make sure that only the size of the
  %% window is considered, so we track the minimum. that is, if the
  %% window has shrunk from 30 to 10, we only allow an update of the
  %% size 1/3 because this is the factor we can safely consider.
  WindowFactor = min(BytesAcked, Cwnd) / max(Cwnd, BytesAcked),

  %% The delay factor is how much we are off the target:
  DelayFactor = TargetOffset / TargetDelay,

  %% How much is the scaled gain?
  ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,

  case ScaledGain =< 1 + ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor of
    true -> ok;
    false ->
      error({scale_gain_violation, ScaledGain, BytesAcked, Cwnd})
  end,

  Alteration =
    case consider_last_maxed_window(LastMaxedOutTime) of
      too_soon -> 0;
      ok -> ScaledGain
    end,
  NewCwnd = clamp(Cwnd + Alteration,?MIN_WINDOW_SIZE,Opts#options.send_buf_size),
  Network#network {cwnd = round(NewCwnd)}.

clamp(Val, Min, _Max) when Val < Min -> Min;
clamp(Val, _Min, Max) when Val > Max -> Max;
clamp(Val, _Min, _Max) -> Val.

update_window(_,Network,_,undefined)-> Network;
update_window(Options,Network, TimeAcked,Packets) ->
  Eligible = extract_rtt(Packets),
  N = lists:foldl(
        fun(TimeSent, Acc) ->
            ack_packet_rtt(Acc,TimeSent,TimeAcked)
        end,Network,Eligible),
  BytesAcked = extract_payload_size(Packets),
  congestion_control(Options,N, BytesAcked).

update_network(Options,Network,ReplyMicro,TimeAcked,TSDiff,WindowSize,Acked)->
  Network0 = update_reply_micro(Network, ReplyMicro),
  Network1 = handle_clock_skew(Network,  Network0),
  Network2 = update_our_ledbat(Network1, TSDiff),
  Network3 = handle_estimate_exceed(Network2),
  Network4 = update_peer_window(Network3, WindowSize),
  update_window(Options,Network4, TimeAcked,Acked).

recv_packet(#packet{seq_no = SeqNo,ack_no = AckNo,payload = Payload,
                        win_sz = WindowSize,type = Type},
                ReplyMicro,TimeAcked,TSDiff,
               #state{network = Network,buffer = Buffer,options = Opts} = State)->
  Buffer0 = is_fin(Type, SeqNo, Buffer),
  case handle_incoming(SeqNo, Payload, Buffer0) of
    {ok,{Buffer1,Messages}} ->
      {ok, SendMessages, Acked, Buffer2} =
                update_send_buffer(AckNo,Buffer1),
      Network0 = update_network(Opts,Network, ReplyMicro, TimeAcked,
                                TSDiff, WindowSize,Acked),
      {ok,State#state{buffer = Buffer2,network = Network0},
       SendMessages ++ Messages};
     no_data when Type == st_state orelse Type == st_data->
      {ok, SendMessages, _Acked, Buffer1} =
                update_send_buffer(AckNo,Buffer0),
      Network0 = update_network(Opts,Network, ReplyMicro, TimeAcked,
                                TSDiff,WindowSize,undefined),
      {ok,State#state{buffer = Buffer1,network = Network0},
       SendMessages}
  end.

%% @doc Return the size of the receive buffer
%% @end
recv_buf_size(Q) ->
  L = queue:to_list(Q),
  lists:sum([byte_size(Payload) || Payload <- L]).

%% @doc Calculate the advertised window to use
%% @end
advertised_window(#options{recv_buf_size = RecvBufSize},
                  #buffer {recv_buf = Q})->
  FillValue = recv_buf_size(Q),
  WindowSize = RecvBufSize - FillValue,
  if
    WindowSize > 0 -> WindowSize;
    true -> 0
  end.

send_packet(Socket,Remote,Packet,State)->
  send_packet(Socket,Remote,Packet,undefined,State).
send_packet(Socket,Remote,Packet,ConnID,#state{
                       network = Network,
                       buffer = Buffer,
                       options = Opts})->
  WindowSize = advertised_window(Opts, Buffer),
  send_packet(Socket,Remote,Network,WindowSize,ConnID,Packet).

send_packet(Socket,Remote,
            #network{reply_micro = TSDiff,peer_conn_id = PeerConnID},
            WindowSize,ConnID,Packet)->
  ConnID0 =
    if
      ConnID == undefined -> PeerConnID;
      true -> ConnID
    end,
  send(Socket,Remote,
       Packet#packet{conn_id = ConnID0,win_sz = WindowSize},TSDiff).

send(Socket,Remote,Packet,TSDiff)->
  send_aux(1,Socket,Remote,ai_utp_protocol:encode(Packet, TSDiff)).
send_aux(0,Socket,Remote,Payload)->
  gen_udp:send(Socket,Remote,Payload);
send_aux(N,Socket,Remote,Payload) ->
  case gen_udp:send(Socket,Remote,Payload) of
    ok -> {ok,ai_utp_util:microsecond()};
    {error,enobufs}->
      timer:sleep(150), % Wait a bit for the queue to clear
      send_aux(N-1, Socket, Remote, Payload);
    Error -> Error
  end.
