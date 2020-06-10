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
                     inbuf_size = RSize}) ->
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
  %% 只更新了reorder 或者收到重复包
  if (Less == true) orelse (LastAck0 == AckNo) ->
      SAcks = proplists:get_value(sack, Ext,undefined),
      {Lost,AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
      {MinRTT,Times,AckBytes} = ack_bytes(AckPackets,Now),
      ai_utp_cc:cc(Net1#utp_net{last_ack = AckNo},Timing, MinRTT,
                   AckBytes,Lost,lists:reverse(Times),WndSize);
     true -> Net
  end.
        


send_ack(#utp_net{ack_nr = AckNR,seq_nr = SeqNR,
                  reply_micro = ReplyMicro,
                  peer_conn_id = PeerConnID} = Net)->
  Bits = ai_utp_buffer:sack(ai_utp_util:bit16(AckNR + 1),Net),
  AckNo = ai_utp_util:bit16(AckNR -1),
  SeqNo = ai_utp_util:bit16(SeqNR -1),
  Packet =
    case Bits of
      undefined -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo);
      _ -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo, [{sack,Bits}])
    end,
  Packet0 = Packet#utp_packet{win_sz = window_size(Net),
                              conn_id = PeerConnID},
  case send(Net,Packet0,ReplyMicro) of
    {ok,SendTimeNow}-> Net#utp_net{last_send = SendTimeNow};
    true -> Net
  end.
send_ack(#utp_net{ack_nr = AckNR,inbuf_size = RSize},
         #utp_net{ack_nr = AckNR0,inbuf_size = RSize0} = Net1)->
  if (AckNR /= AckNR0) orelse
     (RSize /= RSize0) -> send_ack(Net1);
     true -> Net1
  end.

fast_resend(Net,0,OutBuf,Resend)->
  {false,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}};
fast_resend(#utp_net{reply_micro = ReplyMicro,
                     cur_window_packets = CurWindowPackets,
                     cur_window = CurWindow} = Net,
            ResendCount,OutBuf,Resend) ->
  case queue:out(OutBuf) of
    {empty,_} -> {true,Net#utp_net{outbuf = Resend}};
    {{value,Wrap},OutBuf0}->
      #utp_packet_wrap{
         packet = Packet,
         transmissions = Trans,
         need_resend = Resend,
         payload = Payload
        } = Wrap,
      if Resend == true ->
          case send(Net,Packet,ReplyMicro) of
            {ok,SendTimeNow} ->
              Resend0 = queue:in(Wrap#utp_packet_wrap{
                                   need_resend = false,
                                   transmissions = Trans + 1,
                                   send_time = SendTimeNow},Resend),
              fast_resend(Net#utp_net{ last_send = SendTimeNow},
                          ResendCount - 1,OutBuf0,Resend0);
            _ -> {false,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}}
          end;
         true ->
          %% 说明是新的包，继续传输直到队列满了
          if Trans == 0 ->
              case send(Net,Packet,ReplyMicro) of
                {ok,SendTimeNow} ->
                  Resend0 = queue:in(Wrap#utp_packet_wrap{
                                       need_resend = false,
                                       transmissions = Trans + 1,
                                       send_time = SendTimeNow},Resend),
                  fast_resend(Net#utp_net{
                                last_send = SendTimeNow,
                                cur_window = CurWindow + Payload,
                                cur_window_packets = CurWindowPackets + 1},
                              ResendCount - 1,OutBuf0,Resend0);
                _ -> {false,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}}
              end;
             true -> {false,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}}
          end
      end
  end.

fast_resend(#utp_net{outbuf = OutBuf} = Net)->
  %% 快速重发前10个数据包
  fast_resend(Net, 10, OutBuf, queue:new()).

process_incoming(#utp_net{state = State,ack_nr = AckNR,
                          seq_nr = SeqNR,inbuf_size = RSize} = Net,
                 #utp_packet{type = Type,seq_no= SeqNo} = Packet,
                 Timing,Proc) ->
  io:format("Type: ~p seqNo: ~p ackNR:~p seqNR: ~p ReorderSize: ~p~n",
            [Type,SeqNo,AckNR,SeqNR,RSize]),
  Net0 = process_incoming(Type,State,
                          Net#utp_net{last_recv = ai_utp_util:millisecond() },
                          Packet,Timing),
  Net1 =
    if Type == st_data -> send_ack(Net,Net0);
       true -> Net0
    end,
  {SendNew,Net2} = fast_resend(Net1),
  if SendNew == false -> {Net2,Proc};
     true -> do_send(Net2, Proc)
  end.
  
process_incoming(st_state, ?SYN_RECEIVE,
                 #utp_net{seq_nr = SeqNR} = Net,
                 #utp_packet{ack_no = AckNo} = Packet,Timing) ->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 -> ack(Net#utp_net{state = ?ESTABLISHED},Packet,Timing);
     true -> Net
  end;
process_incoming(st_syn, ?SYN_RECEIVE,
                 #utp_net{seq_nr = SeqNR,max_window = MaxWindow,
                          peer_conn_id = PeerConnID,
                          reply_micro = ReplyMicro} = Net,
                 #utp_packet{seq_no = AckNo,conn_id = PeerConnID,
                             win_sz = PeerWinSize},_) ->
  Packet = ai_utp_protocol:make_ack_packet(ai_utp_util:bit16(SeqNR -1), AckNo),
  Net0 = Net#utp_net{max_peer_window = PeerWinSize,
                     ack_nr = ai_utp_util:bit16(AckNo + 1)},
  Packet0 = Packet#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID},
  case send(Net0,Packet0,ReplyMicro) of
    {ok,SendTimeNow} -> Net0#utp_net{last_send = SendTimeNow};
    _ -> Net0
  end;
    
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
    duplicate ->  send_ack(Net); %% 强制发送ACK
    {_,Net1} -> ack(Net1,Packet,Timing)
  end;
process_incoming(st_state,?ESTABLISHED,Net, Packet,Timing) ->
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

connect(#utp_net{max_window = MaxWindow,seq_nr = SeqNR} = Net,ConnID)->
  SeqNR0 =
    case SeqNR of
      undefined -> ai_utp_util:bit16_random();
      _ -> SeqNR
    end,
  SeqNo = ai_utp_util:bit16(SeqNR0 - 1),
  Packet = ai_utp_protocol:make_syn_packet(SeqNo),
  Now =  ai_utp_util:microsecond(),
  Net0 = Net#utp_net{
           last_send = Now,
           last_recv = Now,
           conn_id = ConnID,
           peer_conn_id = ai_utp_util:bit16(ConnID + 1),
           seq_nr = SeqNR0,
           last_seq_nr = SeqNR0,
           last_decay_win = Now /1000,
           state = ?SYN_SEND},
  send(Net0,Packet#utp_packet{conn_id = ConnID,win_sz = MaxWindow},0),
  Net0.

accept(#utp_net{max_window = MaxWindow} = Net,
       #utp_packet{
          conn_id = PeerConnID,
          seq_no = AckNo,
          win_sz = PeerWinSize},
       {TS,_,Now})->
  SeqNo = ai_utp_util:bit16_random(),
  Res = ai_utp_protocol:make_ack_packet(SeqNo, AckNo),
  ConnID = ai_utp_util:bit16(PeerConnID + 1),
  ReplyMicro = Now - TS,
  SeqNR = ai_utp_util:bit16(SeqNo + 1) ,
  Net0 = Net#utp_net{
           last_send = Now,
           last_recv = Now,
           max_peer_window = PeerWinSize,
           conn_id = ConnID,
           peer_conn_id = PeerConnID,
           ack_nr = ai_utp_util:bit16(AckNo + 1),
           seq_nr = SeqNR,
           last_seq_nr = SeqNR,
           last_decay_win = Now /1000,
           reply_micro = ReplyMicro,
           state  = ?SYN_RECEIVE},
  send(Net0,Res#utp_packet{win_sz = MaxWindow,conn_id = PeerConnID},ReplyMicro),
  {Net0,ConnID}.

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

send_packet(Net,ToFill,Packet,ReplyMicro)->
  case send(Net,Packet,ReplyMicro) of
    {ok,Now} -> {Now,#utp_packet_wrap{packet = Packet,transmissions = 1,
                                      payload = ToFill,send_time = Now}};
    _ -> {undefined,#utp_packet_wrap{packet = Packet,transmissions = 0,
                                     payload = ToFill,send_time = undefined}}
  end.

%% 对于所有没有发送成功的包，都不记录在发送窗口内
fill_tx_queue(Net,0) -> Net;
fill_tx_queue(#utp_net{sndbuf_size = 0}= Net,_) -> Net;
fill_tx_queue(#utp_net{
                 seq_nr = SeqNo,
                 last_seq_nr = SeqNo,
                 ack_nr = AckNR,
                 cur_window = CurWindow,
                 cur_window_packets = CurWindowPackets,
                 outbuf = OutBuf,
                 peer_conn_id = PeerConnID,
                 sndbuf = SndBuf,
                 reply_micro = ReplyMicro,
                 sndbuf_size = SndBufSize} = Net,Bytes) ->
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
      {LastSend,WrapPacket} = send_packet(Net, ToFill, Packet0,ReplyMicro),
      if LastSend == undefined ->
          Net#utp_net{last_seq_nr = ai_utp_util:bit16(SeqNo + 1),
                      outbuf = queue:in(WrapPacket,OutBuf),
                      sndbuf_size = SndBufSize - ToFill,
                      sndbuf = SndBuf0};
        true ->
          NextSeqNo =  ai_utp_util:bit16(SeqNo + 1),
          Net0 = Net#utp_net{last_send = LastSend,
                             seq_nr = NextSeqNo,
                             last_seq_nr = NextSeqNo,
                             cur_window = CurWindow + ToFill,
                             cur_window_packets = CurWindowPackets + 1,
                             outbuf = queue:in(WrapPacket,OutBuf),
                             sndbuf_size = SndBufSize - ToFill,
                             sndbuf = SndBuf0},
          fill_tx_queue(Net0,Bytes - ToFill)
      end;
    {Filled,Bin,SndBuf0}->
      Packet = ai_utp_protocol:make_data_packet(SeqNo, AckNo),
      Packet0 = Packet#utp_packet{payload = Bin,
                                  win_sz = WinSize,
                                  conn_id = PeerConnID},
      {LastSend,WrapPacket} = send_packet(Net,Filled, Packet0,ReplyMicro),
      if LastSend == undefined ->
          Net#utp_net{
            last_seq_nr = ai_utp_util:bit16(SeqNo + 1),
            outbuf = queue:in(WrapPacket,OutBuf),
            sndbuf_size = SndBufSize - Filled,
            sndbuf = SndBuf0};
         true ->
          NextSeqNo =  ai_utp_util:bit16(SeqNo + 1),
          Net#utp_net{
            last_send = LastSend,
            seq_nr = NextSeqNo,
            last_seq_nr = NextSeqNo,
            cur_window = CurWindow + Filled,
            cur_window_packets = CurWindowPackets + 1,
            outbuf = queue:in(WrapPacket,OutBuf),
            sndbuf_size = SndBufSize - Filled,
            sndbuf = SndBuf0}
      end;
    zero -> Net
  end.
%% quick send SeqNR =< SeqNO < LastSeqNR
%% and total payload less or equal Bytes
%fill_tx_queue(#utp_packet{outbuf = OutBuf,
%                          seq_nr = SeqNR,
%                          last_seq_nr = LastSeqNR} = Net,Byets) ->


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
  if (Quick == true) orelse
     (Net0#utp_net.sndbuf_size >= ?PACKET_SIZE)->
      Net1 = fill_tx_queue(Net0,MaxSendBytes),
      {Net1,Proc0};
     true -> {Net0,Proc0}
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

expire_resend(#utp_net{reply_micro = ReplyMicro,
                       cur_window = CurWindow,
                       cur_window_packets = CurWindowPackets,
                       last_ack = LastAck} = Net,
              Now,RTO,OutBuf,Resend) ->
  case queue:out(OutBuf) of
    {empty,_} -> {true,Net#utp_net{outbuf = Resend}};
    {{value,WrapPacket},OutBuf0} ->
      #utp_packet_wrap{packet = Packet,
                       transmissions = Trans,
                       payload = Payload,
                       send_time = SendTime } = WrapPacket,
      #utp_packet{seq_no = SeqNo} = Packet,
      Diff =
        if SendTime == undefined -> undefined;
           true -> (Now - SendTime) / 1000
        end,
      Distance = ai_utp_util:bit16(SeqNo - LastAck),
      if Diff == undefined ->
          case send(Net,Packet,ReplyMicro) of
            {ok,SendTimeNow}->
              Resend0 = queue:in(WrapPacket#utp_packet_wrap{
                                   transmissions =  1,
                                   send_time = SendTimeNow},Resend),
              expire_resend(Net#utp_net{
                              cur_window =  CurWindow + Payload,
                              cur_window_packets = CurWindowPackets + 1
                             },Now,RTO,OutBuf0,Resend0);
            _ -> {false,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}}
          end;
         (Diff > RTO) andalso (Trans > 0)
         andalso (Distance < ?OUTGOING_BUFFER_MAX_SIZE)->
          case send(Net,Packet,ReplyMicro) of
            {ok,SendTimeNow}->
              Resend0 = queue:in(WrapPacket#utp_packet_wrap{
                                   transmissions =  Trans + 1,
                                   send_time = SendTimeNow },Resend),
              expire_resend(Net,Now,RTO,OutBuf0,Resend0);
            _ -> {false,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}}
          end;
         true ->
          %% 默认是可以发送的，最终是否能发送，根据滑动窗口发送
          {true,Net#utp_net{outbuf = queue:join(Resend,OutBuf)}}
      end
  end.
              

force_state(State,#utp_net{last_send = LastSend } = Net,
            Now,RTO)->
  Diff = Now - LastSend,
  if ((State == ?ESTABLISHED) orelse (State == ?CLOSING))
     andalso (Diff > RTO)-> send_ack(Net);
     true  -> Net
  end.

on_tick(?CLOSED,Net,Proc)-> {Net,Proc};
on_tick(State,#utp_net{last_recv = LastReceived,outbuf = OutBuf} =  Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastReceived,
  if Diff >= ?MAX_RECV_IDLE_TIME ->
      {{Net#utp_net{state = ?CLOSED,error = econnaborted},
        [],Now,Net#utp_net.reply_micro},Proc};
     true ->
      RTO = rto(Net),
      {SendNew,Net0} = expire_resend(Net, Now, RTO, OutBuf, queue:new()),
      if SendNew == true ->
          {Net1,Proc0} = do_send(Net0,Proc,true),
          Net2 = force_state(State, Net1, Now, RTO),
          {Net2,Proc0};
         true ->
          Net1 = force_state(State, Net, Now, RTO),
          {Net1,Proc}
      end
  end.

net_error(#utp_net{error = Error})-> Error.

send(#utp_net{socket = Socket,remote = Remote},Packet,TSDiff)->
  ai_utp_util:send(Socket, Remote, Packet, TSDiff).
