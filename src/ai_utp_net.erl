-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/3]).
-export([connect/2,accept/3]).
-export([state/1,rto/1,do_send/2,do_read/1]).
-export([on_rto/1]).

ack_bytes(AckPackets,Now)->
  lists:foldl(
    fun(WrapPacket,{RTT,Bytes})->
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
    end, {?RTT_MAX,0}, AckPackets).

window_size(#utp_net{maxed_out_window = true}) -> 0;
window_size(#utp_net{cur_window = CurWindow,
                     max_window = MaxWindow})->
  if MaxWindow > CurWindow ->
      MaxWindow - CurWindow;
     true -> 0
  end.
max_send_bytes(#utp_net{
                  max_window = MaxWindow,
                  max_peer_window = MaxPeerWindow,
                  cur_window = CurWindow,
                  cur_window_packets = CurWindowPackets})->
  if
    CurWindowPackets > ?REORDER_BUFFER_SIZE -> 0;
    true ->
      SendBytes = MaxWindow - CurWindow,
      if SendBytes >= ?PACKET_SIZE ->
          erlang:min(MaxPeerWindow,SendBytes);
         true -> 0
      end
  end.

%% 最后阶段计算并清理所有被Ack过的包
ack(Net,#utp_packet{ack_no = AckNo,
                    win_sz = WndSize,extension = Ext},
    {TS,TSDiff,Now})->
  SAcks = proplists:get_value(sack, Ext,undefined),
  {AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
  {MinRTT,AckBytes} = ack_bytes(AckPackets,Now),
  ai_utp_cc:cc(Net1, TS, TSDiff, Now, MinRTT, AckBytes,WndSize).

send_ack(#utp_net{ack_nr = AckNR,seq_nr = SeqNR,
                  peer_conn_id = PeerConnID} = Net)->
  Bits = ai_utp_buffer:sack(AckNR + 1,Net),
  AckNo = ai_utp_util:bit16(AckNR -1),
  SeqNo = ai_utp_util:bit16(SeqNR -1),
  Packet =
    case Bits of
      undefined -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo);
      _ -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo, [{sack,Bits}])
    end,
  Packet#utp_packet{win_sz = window_size(Net),
                    conn_id = PeerConnID}.

send_ack(#utp_net{ack_nr = AckNR,reorder = Reorder},
         #utp_net{ack_nr = AckNR0,reorder = Reorder0} = Net1)->
  if (AckNR /= AckNR0) orelse
     (Reorder /= Reorder0) -> send_ack(Net1);
     true -> none
  end.




send(_,_,_,_,_,_,[],Acc)-> Acc;
%% 超时5次，基本上就可以认定是断开链接了
send(true,_,_,_,_,_,
     [#utp_packet_wrap{transmissions = Trans} | _],_) when Trans > 5->
  error;
send(Timer,Now,SeqNo,AckNo,MaxBytes,
     SenderWindow,[WrapPacket|T] = Out,Acc)->
  {SendBytes,InFlight,Packets,Buf} = Acc,
  #utp_packet_wrap{
     packet = Packet,
     transmissions = Trans,
     payload = Payload
    } = WrapPacket,
  #utp_packet{seq_no = PacketSeqNo} = Packet,
  if MaxBytes - SendBytes >= Payload ->
      Resend =
        ai_utp_util:wrapping_compare_less(PacketSeqNo,
                                          SeqNo, ?SEQ_NO_MASK),
      Acc0 =
        if (Resend == true) orelse
         (SeqNo == PacketSeqNo) ->
          %% 重新传输的包不记入窗口
          WinSize = SenderWindow - SendBytes,
          {SendBytes,InFlight,
           [Packet#utp_packet{ack_no = AckNo,
                              win_sz = WinSize}|Packets],
           queue:in(WrapPacket#utp_packet_wrap{
                      transmissions = Trans + 1,
                      send_time = Now},Buf)};
         true ->
          WinSize = SenderWindow - SendBytes - Payload,
          {SendBytes + Payload,InFlight + 1,
           [Packet#utp_packet{ack_no = AckNo,
                              win_sz = WinSize}|Packets],
           queue:in(WrapPacket#utp_packet_wrap{
                      transmissions = Trans + 1,
                      send_time = Now},Buf)}
      end,
      send(Timer,Now,SeqNo,AckNo,MaxBytes,
           SenderWindow,T,Acc0);
     true ->
      {SendBytes,InFlight,Packets,
       lists:foldl(fun queue:in/2,Buf, Out)}
  end.

      
send(Net,0,_,_)->
  {Net#utp_net{ maxed_out_window = true,
                last_maxed_out_window = ai_utp_util:millisecond()},[]};
send(#utp_net{seq_nr = SeqNR, ack_nr = AckNR, outbuf = OutBuf,
              cur_window = CurWindow,
              cur_window_packets = CurWindowPackets} = Net,
     MaxBytes,Now,Timer) ->
  SeqNo = ai_utp_util:bit16(SeqNR - 1),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  SenderWindow = window_size(Net),
  case send(Timer,Now,SeqNo,AckNo,MaxBytes,SenderWindow,
            queue:to_list(OutBuf),{0,0,[],queue:new()}) of
    error -> error;
    {Bytes,InFlight,Packets,OutBuf0}->
      {Net#utp_net{
        outbuf = OutBuf0,
        cur_window = CurWindow + Bytes,
        cur_window_packets = CurWindowPackets + InFlight,
        seq_nr = SeqNR + InFlight
       },lists:reverse(Packets)}
  end.

send(#utp_net{outbuf = OutBuf} = Net,Now,Timer)->
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> {Net,[]};
     true ->
      SendBytes = max_send_bytes(Net),
      case send(Net,SendBytes,Now,Timer) of
        error -> {Net#utp_net{state = {?ERROR,epipe}},[]};
        R -> R
      end
  end.


process_incoming(#utp_net{state = State }= Net,
                 #utp_packet{type = Type} = Packet,
                 Timing) ->
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
  {Net1,Packets0} = send(Net0,Now,false),
  Packets1 =
    if AckPacket == none -> Packets ++ Packets0;
       true -> Packets ++ [AckPacket|Packets0]
    end,
  {Net1,Packets1,Now,Net1#utp_net.reply_micro}.


process_incoming(st_syn, ?SYN_RECEIVE,
                 #utp_net{seq_nr = SeqNR,max_window = MaxWindow,
                         peer_conn_id = PeerConnID} = Net,
                 #utp_packet{seq_no = AckNo,conn_id = PeerConnID,
                             win_sz = PeerWinSize},_) ->
  Packet = ai_utp_protocol:make_ack_packet(ai_utp_util:bit16(SeqNR -1), AckNo),
  {Net#utp_net{
     max_peer_window = PeerWinSize,
     ack_nr = ai_utp_util:bit16(AckNo + 1)},
   [Packet#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID}],
  #{}};
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
      Packet = send_ack(Net),
      {Net,[Packet]};
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
  Net#utp_net{state = {?ERROR,econnrefused}}.

connect(#utp_net{max_window = MaxWindow} = Net,ConnID)->
  Packet = ai_utp_protocol:make_syn_packet(),
  {Net#utp_net{
     conn_id = ConnID,
     peer_conn_id = ai_utp_util:bit16(ConnID + 1),
     seq_nr = 2,
     state = ?SYN_SEND},
   Packet#utp_packet{conn_id = ConnID,
                        win_sz = MaxWindow}}.
accept(#utp_net{max_window = MaxWindow} = Net,
       #utp_packet{
          conn_id = PeerConnID,
          seq_no = AckNo,
          win_sz = PeerWinSize} = Packet, Timing)->

  SeqNo = ai_utp_util:bit16_random(),
  Res = ai_utp_protocol:make_ack_packet(SeqNo, AckNo),
  ConnID = ai_utp_util:bit16(PeerConnID + 1),
  Net0 = Net#utp_net{
     max_peer_window = PeerWinSize,
     conn_id = ConnID,
     peer_conn_id = PeerConnID,
     ack_nr = ai_utp_util:bit16(AckNo + 1),
     seq_nr = ai_utp_util:bit16(SeqNo + 1) ,
     state  = ?SYN_RECEIVE},
  Net1 = ack(Net0, Packet, Timing),
  {Net1,Res#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID},
   ConnID,Net1#utp_net.reply_micro}.
state(#utp_net{state = State})-> State.
rto(#utp_net{rtt = RTT,outbuf = OutBuf})->
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> undefined;
     true -> ai_utp_rtt:rto(RTT)
  end.


fill_from_proc(Net,Bytes,Proc)->
  TxQ = queue:new(),
  fill_from_proc(Net,Bytes,Proc,TxQ).

fill_from_proc(Net, 0, Proc,TxQ)->
  {Net#utp_net{
     last_maxed_out_window = ai_utp_util:millisecond(),
     maxed_out_window = true
    },Proc,TxQ};
fill_from_proc(Net,Bytes,Proc,TxQ) ->
  ToFill =
    if Bytes =< ?PACKET_SIZE -> Bytes;
       true -> ? PACKET_SIZE
    end,
  case ai_utp_process:fill_send_window(ToFill, Proc) of
    {filled,Bin,Proc1}->
      fill_from_proc(Net, Bytes - ToFill,Proc1,
                     queue:in(Bin,TxQ));
    {partial,Bin,Proc1}-> {Net,Proc1,
                           queue:in(Bin,TxQ)};
    zero -> {Net,Proc,TxQ}
  end.

send_tx_queue(#utp_net{ack_nr = AckNR,
                       seq_nr = SeqNR,
                       max_window = MaxWindow,
                       cur_window = CurWindow,
                       cur_window_packets = CurWindowPackets,
                       outbuf = OutBuf,
                       peer_conn_id = PeerConnID,
                       reply_micro = ReplyMicro
                      }= Net,TxQ)->
  L = queue:to_list(TxQ),
  Now = ai_utp_util:microsecond(),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  {SeqNR0,Packets,CurWindow0,
   CurWindowPackets0,OutBuf0} =
    lists:foldl(fun(Bin,{SeqNo,Acc,CurWindowAcc,
                         CurWindowPacketsAcc,WarpAcc})->
                    Packet = ai_utp_protocol:make_data_packet(SeqNo, AckNo),
                    Packet0 = Packet#utp_packet{payload = Bin,
                                                win_sz = MaxWindow - CurWindowAcc,
                                                conn_id = PeerConnID},
                    Size = erlang:byte_size(Bin),
                    WrapPacket = #utp_packet_wrap{
                                    packet = Packet0,
                                    transmissions = 1,
                                    payload = Size,
                                    send_time = Now
                                   },
                    {ai_utp_util:bit16(SeqNo+1),[Packet0|Acc],
                     CurWindowAcc + Size,CurWindowPacketsAcc +1,
                     queue:in(WrapPacket, WarpAcc)}
                end,{SeqNR,[],CurWindow,CurWindowPackets,OutBuf},L),
  {Net#utp_net{
     cur_window_packets = CurWindowPackets0,
     cur_window = CurWindow0,
     outbuf = OutBuf0,
     seq_nr = SeqNR0
    },lists:reverse(Packets),Now,ReplyMicro}.

do_send(Net,Proc)->
  MaxSendBytes = max_send_bytes(Net),
  {Net0,Proc0,TxQ} = fill_from_proc(Net, MaxSendBytes, Proc),
  Result = send_tx_queue(Net0, TxQ),
  {Result,Proc0}.

do_read(#utp_net{inbuf = InBuf} = Net)->
  Size = erlang:byte_size(InBuf),
  if Size > 0 ->
      {Net#utp_net{inbuf = <<>>},InBuf};
     true -> Net
  end.

on_rto(Net)->
  Now = ai_utp_util:microsecond(),
  {Net0,Packets0} = send(Net,Now,true),
  {Net0,Packets0,Now,Net0#utp_net.reply_micro}.
