-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/4]).
-export([connect/2,accept/3]).
-export([state/1,do_send/2,do_read/1]).
-export([on_tick/3,net_error/1]).

ack_bytes(AckPackets,Now)->
  lists:foldl(
    fun(WrapPacket,{RTT,Times,Bytes})->
        Trans = WrapPacket#utp_packet_wrap.transmissions,
        SendTime = WrapPacket#utp_packet_wrap.send_time,
        Times0 =
          if Trans == 1 -> [SendTime|Times];
             true -> Times
          end,
        if Trans > 0 ->
            Bytes0 = Bytes + WrapPacket#utp_packet_wrap.payload,
            RTT0 =
              if SendTime < Now -> min(RTT, Now - SendTime );
                 true -> min(RTT,50000)
              end,
            {RTT0,Times0,Bytes0};
           true -> {RTT,Times0,Bytes}
        end
    end, {?RTT_MAX,[],0}, AckPackets).

window_size(#utp_net{opt_recvbuf = OptRecvBuf,
                     recvbuf_size = RecvBufSize,
                     reorder_size = RSize}) ->
  if RSize < ?REORDER_BUFFER_MAX_SIZE ->
      if OptRecvBuf > RecvBufSize -> OptRecvBuf - RecvBufSize;
         true -> 0
      end;
     true -> 0
  end.

sndbuf_remain(#utp_net{opt_sndbuf = OptSndBuf,
                          sndbuf_size = SndBufSize})->
  if OptSndBuf > SndBufSize ->
      OptSndBuf - SndBufSize;
     true -> 0
  end.
max_send_bytes(#utp_net{max_window = MaxWindow,
                        max_peer_window = MaxPeerWindow,
                        cur_window_packets = CurWindowPackets,
                        cur_window = CurWindow})->
  if CurWindowPackets >= ?REORDER_BUFFER_MAX_SIZE -> 0;
     true->
      AllowPacketSize = (?REORDER_BUFFER_MAX_SIZE - CurWindowPackets) * ?PACKET_SIZE,
      SendBytes = erlang:min(MaxWindow - CurWindow,AllowPacketSize),
      if SendBytes >= ?PACKET_SIZE -> erlang:min(MaxPeerWindow,SendBytes);
         true -> 0
      end
  end.

%% 最后阶段计算并清理所有被Ack过的包
ack(#utp_net{last_ack = LastAck} =Net,
    #utp_packet{ack_no = AckNo,
                win_sz = WndSize,extension = Ext},
    {_,_,Now} = Timing)->
  LastAck0 =
    if LastAck == undefined -> AckNo;
       true -> LastAck
    end,
  Less = ai_utp_util:wrapping_compare_less(LastAck0,AckNo,?ACK_NO_MASK),
  %% 只更新了reorder
  if (Less == true) orelse (LastAck0 == AckNo) ->
      SAcks = proplists:get_value(sack, Ext,undefined),
      {Lost,AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
      {MinRTT,Times,AckBytes} = ack_bytes(AckPackets,Now),
      ai_utp_cc:cc(Net1#utp_net{last_ack = AckNo},Timing, MinRTT,
                   AckBytes,Lost,lists:reverse(Times),WndSize);
     true -> Net
  end.
        


send_ack(#utp_net{ack_nr = AckNR,seq_nr = SeqNR,
                  peer_conn_id = PeerConnID} = Net)->
  Bits = ai_utp_buffer:sack(ai_utp_util:bit16(AckNR + 1),Net),
  AckNo = ai_utp_util:bit16(AckNR -1),
  SeqNo = ai_utp_util:bit16(SeqNR -1),
  Packet =
    case Bits of
      undefined -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo);
      _ -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo, [{sack,Bits}])
    end,
  Packet#utp_packet{win_sz = window_size(Net),
                    conn_id = PeerConnID}.

send_ack(#utp_net{ack_nr = AckNR,reorder_size = RSize},
         #utp_net{ack_nr = AckNR0,reorder_size = RSize0} = Net1)->
  if (AckNR /= AckNR0) orelse
     (RSize /= RSize0) -> send_ack(Net1);
     true -> none
  end.


fast_resend(#utp_net{outbuf = OutBuf,
                     ack_nr = AckNR} = Net,Now)->
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> {Net,[]};
     true ->
      AckNo = ai_utp_util:bit16(AckNR - 1),
      WinSize = window_size(Net),
      {_,Packets0,OutBuf0} =
        lists:foldl(fun(#utp_packet_wrap{
                           packet = Packet,
                           transmissions = Trans,
                           need_resend = Resend
                          } = Wrap,{Count,Packets,Out})->

                        if Count > 10 ->
                            Wrap0 = Wrap#utp_packet_wrap{need_resend = false},
                            {Count,Packets,queue:in(Wrap0,Out)};
                           true->
                            if Resend == true ->
                                Packet0 = Packet#utp_packet{ack_no = AckNo,
                                                            win_sz = WinSize},
                                Wrap0 = Wrap#utp_packet_wrap{
                                          need_resend = false,
                                          packet = Packet0,
                                          transmissions = Trans + 1,
                                          send_time = Now},
                                {Count+1,[Packet0|Packets],queue:in(Wrap0,Out)};
                               true ->
                                Wrap0 = Wrap#utp_packet_wrap{need_resend = false},
                                {Count +1,Packets,queue:in(Wrap0,Out)}
                            end
                        end
                    end,{0,[],queue:new()},queue:to_list(OutBuf)),
          {Net#utp_net{outbuf = OutBuf0},
           lists:reverse(Packets0)}
  end.
process_incoming(#utp_net{state = State} = Net,
                 #utp_packet{type = Type} = Packet,
                 Timing,Proc) ->
  {Net0,Packets} =
    case process_incoming(Type,State,Net,Packet,Timing) of
      {_,_} = R -> R;
      N -> {N,[]}
    end,
  AckPacket =
    if Type == st_data -> send_ack(Net,Net0);
       true -> none
    end,
  Now = ai_utp_util:microsecond(),
  {Net1,Packets0} = fast_resend(Net0,Now),
  
  Packets1 =
    if AckPacket == none -> Packets ++ Packets0;
       true -> [AckPacket|Packets] ++ Packets0
    end,
  Net2 =
    if erlang:length(Packets1) > 0 ->
        Net1#utp_net{last_send = Now,last_recv = Now};
       true -> Net1#utp_net{last_recv = Now}
    end,
  {{Net3,Packets2,_,ReplyMicro},Proc0} = do_send(Net2,Proc,true),
  {{Net3,Packets1 ++ Packets2,Now,ReplyMicro},Proc0}.

process_incoming(st_state, ?SYN_RECEIVE,
                 #utp_net{seq_nr = SeqNR} = Net,
                 #utp_packet{ack_no = AckNo} = Packet,Timing) ->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 -> ack(Net#utp_net{state = ?ESTABLISHED},Packet,Timing);
     true -> Net
  end;
process_incoming(st_syn, ?SYN_RECEIVE,
                 #utp_net{seq_nr = SeqNR,max_window = MaxWindow,
                         peer_conn_id = PeerConnID} = Net,
                 #utp_packet{seq_no = AckNo,conn_id = PeerConnID,
                             win_sz = PeerWinSize},_) ->
  Packet = ai_utp_protocol:make_ack_packet(ai_utp_util:bit16(SeqNR -1), AckNo),
  {Net#utp_net{
     max_peer_window = PeerWinSize,
     ack_nr = ai_utp_util:bit16(AckNo + 1)},
   [Packet#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID}]};
process_incoming(st_data,?SYN_RECEIVE,
                 #utp_net{seq_nr = SeqNR,ack_nr = PeerSeqNo} = Net,
                 #utp_packet{ack_no = AckNo,seq_no = PeerSeqNo,
                            win_sz = WndSize} =  Packet,Timing) ->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 ->
      process_incoming(st_data,?ESTABLISHED,
                       Net#utp_net{state = ?ESTABLISHED,
                                   max_peer_window = WndSize},
                       Packet,Timing);
     true -> Net
  end;
process_incoming(st_data,?ESTABLISHED,Net,
                 #utp_packet{seq_no = SeqNo,payload = Payload
                            }=Packet,Timing) ->
  case ai_utp_buffer:in(SeqNo,Payload,Net) of
    duplicate ->
      %% 强制发送ACK
      AckPacket = send_ack(Net),
      {Net,[AckPacket]};
    {_,Net1} -> ack(Net1,Packet,Timing)
  end;
process_incoming(st_state,?ESTABLISHED,Net,
                 Packet,Timing) ->
  ack(Net,Packet,Timing);
process_incoming(st_state,?SYN_SEND,
                 #utp_net{seq_nr = SeqNR} = Net,
                 #utp_packet{ack_no = AckNo,seq_no = SeqNo} = Packet,
                 Timing) ->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 ->
      Net1 = Net#utp_net{state = ?ESTABLISHED,
                         ack_nr = ai_utp_util:bit16(SeqNo + 1)},
      ack(Net1,Packet,Timing);
     true -> Net
  end;
process_incoming(st_reset,_,Net,_,_) ->
  Net#utp_net{state = ?CLOSED,error=econnrefused}.

connect(#utp_net{max_window = MaxWindow} = Net,ConnID)->
  SeqNo = ai_utp_util:bit16_random(),
  Packet = ai_utp_protocol:make_syn_packet(SeqNo),
  Now =  ai_utp_util:microsecond(),
  {Net#utp_net{
     last_send = Now,
     last_recv = Now,
     conn_id = ConnID,
     peer_conn_id = ai_utp_util:bit16(ConnID + 1),
     seq_nr = ai_utp_util:bit16(SeqNo + 1),
     last_decay_win = Now /1000,
     state = ?SYN_SEND},
   Packet#utp_packet{conn_id = ConnID,
                     win_sz = MaxWindow}}.
accept(#utp_net{max_window = MaxWindow} = Net,
       #utp_packet{
          conn_id = PeerConnID,
          seq_no = AckNo,
          win_sz = PeerWinSize},
       {TS,_,Now})->
  SeqNo = ai_utp_util:bit16_random(),
  Res = ai_utp_protocol:make_ack_packet(SeqNo, AckNo),
  ConnID = ai_utp_util:bit16(PeerConnID + 1),
  Net0 = Net#utp_net{
           last_send = Now,
           last_recv = Now,
           max_peer_window = PeerWinSize,
           conn_id = ConnID,
           peer_conn_id = PeerConnID,
           ack_nr = ai_utp_util:bit16(AckNo + 1),
           seq_nr = ai_utp_util:bit16(SeqNo + 1) ,
           last_decay_win = Now /1000,
           state  = ?SYN_RECEIVE},
  {Net0,Res#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID},
   ConnID,Now - TS}.

state(#utp_net{state = State})-> State.
rto(#utp_net{rtt = RTT})-> ai_utp_rtt:rto(RTT).

fill_buffer(Net,0,Proc)-> {Net,Proc};
fill_buffer(#utp_net{sndbuf = SndBuf,
                     sndbuf_size = SndBufSize} = Net,Bytes,Proc) ->
  case ai_utp_process:fill_send_window(Bytes, Proc) of
    {filled,Bin,Proc0}-> {Net#utp_net{sndbuf = queue:in({Bytes,Bin},SndBuf),
                                      sndbuf_size = SndBufSize + Bytes},Proc0};
    {partial,Bin,Proc0}->
      Size = erlang:byte_size(Bin),
      {Net#utp_net{sndbuf = queue:in({Size,Bin},SndBuf),
                   sndbuf_size = SndBufSize + Size },Proc0};
    zero -> {Net,Proc}
  end.

fill_tx_queue(_,Net,0) -> {Net,[]};
fill_tx_queue(Now,Net,Bytes) ->
  fill_tx_queue(Now,Net,Bytes,queue:new()).

fill_tx_queue(_,Net,0,TxQ) -> {Net,queue:to_list(TxQ)};
fill_tx_queue(_,#utp_net{sndbuf_size = 0}= Net,_,TxQ) ->
  {Net,queue:to_list(TxQ)};
fill_tx_queue(Now,#utp_net{
                     seq_nr = SeqNo,
                     ack_nr = AckNR,
                     cur_window = CurWindow,
                     cur_window_packets = CurWindowPackets,
                     outbuf = OutBuf,
                     peer_conn_id = PeerConnID,
                     sndbuf = SndBuf,
                     sndbuf_size = SndBufSize} = Net,Bytes,TxQ) ->
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WinSize = window_size(Net),
  ToFill =
    if Bytes =< ?PACKET_SIZE -> Bytes;
       true -> ?PACKET_SIZE
    end,
  case fill_from_buffer(ToFill,SndBuf,SndBufSize) of
    {filled,Bin,SndBuf0} ->
      Packet = ai_utp_protocol:make_data_packet(SeqNo, AckNo),
      Packet0 = Packet#utp_packet{payload = Bin,
                                  win_sz = WinSize,
                                  conn_id = PeerConnID},
      WrapPacket = #utp_packet_wrap{
                      packet = Packet0,
                      transmissions = 1,
                      payload = ToFill,
                      send_time = Now},
      Net0 = Net#utp_net{
               last_send = Now,
               seq_nr = ai_utp_util:bit16(SeqNo + 1),
               cur_window = CurWindow + ToFill,
               cur_window_packets = CurWindowPackets + 1,
               outbuf = queue:in(WrapPacket,OutBuf),
               sndbuf_size = SndBufSize - ToFill,
               sndbuf = SndBuf0
              },
      fill_tx_queue(Now, Net0,Bytes - ToFill,queue:in(Packet0,TxQ));
    {Filled,Bin,SndBuf0}->
      Packet = ai_utp_protocol:make_data_packet(SeqNo, AckNo),
      Packet0 = Packet#utp_packet{payload = Bin,
                                  win_sz = WinSize,
                                  conn_id = PeerConnID},
      WrapPacket = #utp_packet_wrap{
                      packet = Packet0,
                      transmissions = 1,
                      payload = Filled,
                      send_time = Now},
      Net0 = Net#utp_net{
               last_send = Now,
               seq_nr = ai_utp_util:bit16(SeqNo + 1),
               cur_window = CurWindow + Filled,
               cur_window_packets = CurWindowPackets + 1,
               outbuf = queue:in(WrapPacket,OutBuf),
               sndbuf_size = SndBufSize - Filled,
               sndbuf = SndBuf0
              },
      {Net0,queue:to_list(queue:in(Packet0,TxQ))};
    zero -> {Net,queue:to_list(TxQ)}
  end.

fill_from_buffer(_,_,0) -> zero;
fill_from_buffer(ToFill,SndBuf,SndBufSize)
  when ToFill >= SndBufSize ->
  Bin = lists:foldl(
          fun({_,Bin},Acc)-> <<Acc/binary,Bin/binary>> end,
          <<>>,queue:to_list(SndBuf)),
  {SndBufSize,Bin,queue:new()};
fill_from_buffer(ToFill,SndBuf,_) -> dequeue_sndbuf(ToFill,SndBuf,<<>>).
  
dequeue_sndbuf(0,SndBuf,Acc)-> {filled,Acc,SndBuf};
dequeue_sndbuf(ToFill,SndBuf,Acc)->
  {{value,{Size,Payload}},SndBuf0} = queue:out(SndBuf),
  if Size > ToFill ->
      Size0  = Size - ToFill,
      <<ToSend:ToFill/binary,Rest/binary>> = Payload,
      {filled,<<Acc/binary,ToSend/binary>>,
       queue:in_r({Size0,Rest}, SndBuf0)};
     true->
      dequeue_sndbuf(ToFill - Size,SndBuf0,
                     <<Acc/binary,Payload/binary>>)
  end.


%% try to full fill one package
do_send(Net,Proc)-> do_send(Net,Proc,false).


do_send(Net,Proc,Quick)->
  MaxBufSize = sndbuf_remain(Net),
  MaxSendBytes = max_send_bytes(Net),
  {Net0,Proc0} = fill_buffer(Net,MaxBufSize + MaxSendBytes,Proc),
  Now = ai_utp_util:microsecond(),
  if (Quick == true) orelse
     (Net0#utp_net.sndbuf_size >= ?PACKET_SIZE)->
      {Net1,TxQ} = fill_tx_queue(Now,Net0,MaxSendBytes),
      {{Net1,TxQ,Now,Net1#utp_net.reply_micro},Proc0};
     true ->
      {{Net0,[],Now,Net0#utp_net.reply_micro},Proc0}
  end.

do_read(#utp_net{
           recvbuf_size = RecvBufSize,
           recvbuf = RecvBuf} = Net)->
  if RecvBufSize > 0 ->
      InBuf = lists:foldl(
                fun({_,Payload},Acc)-> <<Acc/binary,Payload/binary>> end,
                <<>>, queue:to_list(RecvBuf)),
      {Net#utp_net{recvbuf_size = 0, recvbuf = queue:new()},InBuf};
     true -> Net
  end.


expire_resend(#utp_net{ack_nr = AckNR,
                       last_ack = LastAck,
                       outbuf = OutBuf} = Net,Now,RTO)->
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WinSize = window_size(Net),
  {Packets,OutBuf0} =
    lists:foldl(fun(#utp_packet_wrap{
                       packet = Packet,
                       transmissions = Trans,
                       send_time = SendTime
                      } = WrapPacket,{Packets,Out})->
                    Diff = (Now - SendTime) / 1000,
                    #utp_packet{seq_no = SeqNo} = Packet,
                    Distance = ai_utp_util:bit16(SeqNo - LastAck),
                    if (Diff > RTO) andalso (Trans > 0)
                       andalso (Distance < ?OUTGOING_BUFFER_MAX_SIZE)->
                        {[Packet#utp_packet{
                                      ack_no = AckNo,
                                      win_sz = WinSize}|Packets],
                         queue:in(WrapPacket#utp_packet_wrap{
                                   transmissions = Trans + 1,
                                   send_time = Now},Out)};
                       true-> {Packets,queue:in(WrapPacket, Out)}
                    end
                end,{[],queue:new()},queue:to_list(OutBuf)),
  {Net#utp_net{outbuf = OutBuf0},lists:reverse(Packets)}.

force_state(State,#utp_net{last_send = LastSend } = Net,
            Packets,Now,RTO)->
  Diff = Now - LastSend,
  Packets0 =
    if ((State == ?ESTABLISHED) orelse (State == ?CLOSING))
       andalso (erlang:length(Packets) == 0)
       andalso (Diff > RTO)-> [send_ack(Net)];
       true  -> Packets
    end,
  if erlang:length(Packets0) > 0 ->
      {Net#utp_net{last_send = Now},Packets0};
     true ->{ Net,Packets0 }
  end.


on_tick(?CLOSED,Net,Proc)->
  Now = ai_utp_util:microsecond(),
  {{Net,[],Now,Net#utp_net.reply_micro},Proc};

on_tick(State,#utp_net{last_recv = LastReceived} =  Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastReceived,
  if Diff >= ?MAX_RECV_IDLE_TIME ->
      {{Net#utp_net{state = ?CLOSED,error = econnaborted},
        [],Now,Net#utp_net.reply_micro},Proc};
     true ->
      RTO = rto(Net),
      {Net0,Packets0} = expire_resend(Net,Now,RTO),
      {{Net2,Packets1,_,ReplyMicro},Proc0} = do_send(Net0,Proc,true),
      {Net3,Packets} = force_state(State,Net2,Packets0 ++ Packets1,Now,RTO),
      {{Net3,Packets,Now,ReplyMicro},Proc0}
  end.

net_error(#utp_net{error = Error})->
  Error.
