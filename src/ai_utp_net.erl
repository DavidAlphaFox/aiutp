-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/3]).
-export([connect/2,accept/3]).
-export([state/1,do_send/2,do_read/1]).
-export([on_tick/3]).

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
    CurWindowPackets > ?REORDER_BUFFER_MAX_SIZE -> 0;
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
    {_,_,Now} = Timing)->
  SAcks = proplists:get_value(sack, Ext,undefined),
  {AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
  {MinRTT,Times,AckBytes} = ack_bytes(AckPackets,Now),
  ai_utp_cc:cc(Net1,Timing, MinRTT,
               AckBytes,lists:reverse(Times),WndSize).

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




do_resend(#utp_net{ack_nr = AckNR,outbuf = OutBuf} = Net,Now)->
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WinSize = window_size(Net),
  {Packets,OutBuf0} =
    lists:foldl(fun(#utp_packet_wrap{
                       packet = Packet,
                       transmissions = Trans,
                       need_resend = Resend
                      } = WrapPacket,{Packets,Out})->
                    if Resend == true  ->
                        {[Packet#utp_packet{ack_no = AckNo,
                                           win_sz = WinSize}|Packets],
                        queue:in(WrapPacket#utp_packet_wrap{
                                   need_resend = false,
                                   transmissions = Trans + 1,
                                   send_time = Now},Out)};
                       true-> {Packets,queue:in(WrapPacket, Out)}
                    end
                end,{[],queue:new()},queue:to_list(OutBuf)),
  {Net#utp_net{outbuf = OutBuf0},lists:reverse(Packets)}.

resend(#utp_net{outbuf = OutBuf} = Net,Now)->
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> {Net,[]};
     true -> do_resend(Net,Now)
  end.


process_incoming(#utp_net{state = State,last_recv = LastReceived,last_send = LastSend }= Net,
                 #utp_packet{type = Type,win_sz = PeerWinSize } = Packet,
                 Timing) ->
  WinSize = window_size(Net),
  io:format("Peer WinSize: ~p WinSize:~p~n",[PeerWinSize,WinSize]),
  io:format("LastSend:~p LastReceived:~p~n",[LastSend,LastReceived]),
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
  {Net1,Packets0} = resend(Net0,Now),
  Packets1 =
    if AckPacket == none -> Packets ++ Packets0;
       true -> Packets ++ [AckPacket|Packets0]
    end,
  Net2 =
    if erlang:length(Packets1) > 0 ->
        Net1#utp_net{last_send = Now,last_recv = Now};
       true -> Net1#utp_net{last_recv = Now}
    end,
  {Net2,Packets1,Now,Net1#utp_net.reply_micro}.

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
  Net#utp_net{state = {?ERROR,econnrefused}}.

connect(#utp_net{max_window = MaxWindow} = Net,ConnID)->
  Packet = ai_utp_protocol:make_syn_packet(),
  Now =  ai_utp_util:microsecond(),
  {Net#utp_net{
     last_send = Now,
     last_recv = Now,
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
          win_sz = PeerWinSize} = Packet,
       {_,_,Now} = Timing)->

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
           state  = ?SYN_RECEIVE},
  Net1 = ack(Net0, Packet, Timing),
  {Net1,Res#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID},
   ConnID,Net1#utp_net.reply_micro}.

state(#utp_net{state = State})-> State.
rto(#utp_net{rtt = RTT})-> ai_utp_rtt:rto(RTT).

fill_from_proc(Net,Bytes,Proc)->
  TxQ = queue:new(),
  fill_from_proc(Net,Bytes,Proc,TxQ).

fill_from_proc(Net, 0, Proc,TxQ)-> {Net,Proc,TxQ};
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

transmit(#utp_net{ack_nr = AckNR,
                       seq_nr = SeqNR,
                       max_window = MaxWindow,
                       cur_window = CurWindow,
                       cur_window_packets = CurWindowPackets,
                       outbuf = OutBuf,
                       peer_conn_id = PeerConnID
                      }= Net,TxQ,Now)->
  AckNo = ai_utp_util:bit16(AckNR - 1),
  {SeqNR0,Packets,CurWindow0,CurWindowPackets0,OutBuf0} =
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
                                    send_time = Now},
                    {ai_utp_util:bit16(SeqNo+1),[Packet0|Acc],
                     CurWindowAcc + Size,CurWindowPacketsAcc +1,
                     queue:in(WrapPacket, WarpAcc)}
                end,{SeqNR,[],CurWindow,CurWindowPackets,OutBuf},TxQ),
  {Net#utp_net{
     cur_window_packets = CurWindowPackets0,
     cur_window = CurWindow0,
     outbuf = OutBuf0,
     seq_nr = SeqNR0
    },lists:reverse(Packets)}.

do_send(Net,Proc)->
  MaxSendBytes = max_send_bytes(Net),
  {Net0,Proc0,TxQ} = fill_from_proc(Net, MaxSendBytes, Proc),
  TxQ0 = queue:to_list(TxQ),
  Now = ai_utp_util:microsecond(),
  {Net1,Packets} =
    if erlang:length(TxQ0) > 0 ->
        transmit(Net0#utp_net{last_send = Now}, TxQ0,Now);
       true ->{Net0,[]}
    end,
  {{Net1,Packets,Now,Net1#utp_net.reply_micro},Proc0}.

do_read(#utp_net{inbuf = InBuf} = Net)->
  Size = erlang:byte_size(InBuf),
  if Size > 0 ->
      {Net#utp_net{inbuf = <<>>},InBuf};
     true -> Net
  end.


expire_resend(#utp_net{ack_nr = AckNR,
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
                    if (Diff > RTO) andalso (Trans > 0) ->
                        {[Packet#utp_packet{ack_no = AckNo,
                                           win_sz = WinSize}|Packets],
                        queue:in(WrapPacket#utp_packet_wrap{
                                   transmissions = Trans + 1,
                                   send_time = Now},Out)};
                       true-> {Packets,queue:in(WrapPacket, Out)}
                    end
                end,{[],queue:new()},queue:to_list(OutBuf)),
  {Net#utp_net{outbuf = OutBuf0},lists:reverse(Packets)}.

force_state(State,#utp_net{last_send = LastSend} = Net,Packets,Now)->
  Diff = Now - LastSend,
  Packets0 =
    if ((State == ?ESTABLISHED) orelse (State == ?CLOSING)) andalso
       ((Diff > ?MAX_SEND_IDLE_TIME) andalso erlang:length(Packets) == 0) ->
        [send_ack(Net)];
       true  -> Packets
    end,
  if erlang:length(Packets0) > 0 ->
      {Net#utp_net{last_send = Now},Packets0};
     true ->{ Net,Packets0 }
  end.

on_tick({?ERROR,_},Net,Proc)->
  Now = ai_utp_util:microsecond(),
  {{Net,[],Now,Net#utp_net.reply_micro},Proc};
on_tick(?CLOSED,Net,Proc)->
  Now = ai_utp_util:microsecond(),
  {{Net,[],Now,Net#utp_net.reply_micro},Proc};
on_tick(State,#utp_net{last_recv = LastReceived} =  Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastReceived,
  if Diff >= ?MAX_RECV_IDLE_TIME ->
      {{Net#utp_net{state = {?ERROR,econnaborted}},
       [],Now,Net#utp_net.reply_micro},Proc};
     true ->
      RTO = rto(Net),
      {Net0,Packets0} = expire_resend(Net,Now,RTO),
      {{Net1,Packets1,_,ReplyMicro},Proc0} = do_send(Net0,Proc),
      {Net2,Packets} = force_state(State,Net1,Packets0 ++ Packets1,Now),
      {{Net2,Packets,Now,ReplyMicro},Proc0}
  end.
