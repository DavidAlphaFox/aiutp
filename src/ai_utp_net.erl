-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/4]).
-export([connect/2,accept/3,close/2,close/1]).
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


%% 最后阶段计算并清理所有被Ack过的包
ack(#utp_net{last_ack = LastAck} =Net,
    #utp_packet{ack_no = AckNo,
                win_sz = WndSize,extension = Ext},
    {_,_,Now} = Timing)->
  LastAck0 =
    if LastAck == undefined -> ai_utp_util:bit16(AckNo -1);
       true -> LastAck
    end,
  Less = ai_utp_util:wrapping_compare_less(LastAck0,AckNo,?ACK_NO_MASK),
  %% 只更新了reorder 或者收到重复包
  if Less == true ->
      SAcks = proplists:get_value(sack, Ext,undefined),
      {Lost,AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
      {MinRTT,Times,AckBytes} = ack_bytes(AckPackets,Now),
      {Lost,ai_utp_cc:cc(Net1#utp_net{last_ack = AckNo},Timing, MinRTT,
                   AckBytes,Lost,lists:reverse(Times),WndSize)};
     true -> {0,Net}
  end.
        


fast_resend(#utp_net{
               outbuf = OutBuf
              } = Net,Index, Last,0)->
  Less = ai_utp_util:wrapping_compare_less(Index, Last, ?ACK_NO_MASK),
  case array:get(Index,OutBuf) of
    undefined ->
      if Less == true -> {false,Net};
         true -> {true,Net}
      end;
    #utp_packet_wrap{need_resend = Resend} ->
      if Resend == true -> {false,Net};
         true -> {true,Net}
      end
  end;
fast_resend(#utp_net{reply_micro = ReplyMicro,
                     outbuf = OutBuf,
                     cur_window_packets = CurWindowPackets,
                     cur_window = CurWindow} = Net,
            Index,Last,ResendCount) ->
  Less = ai_utp_util:wrapping_compare_less(Index, Last, ?ACK_NO_MASK),
  if (Less == true) orelse (Index == Last) ->
      case array:get(Index,OutBuf) of
        undefined ->
          if Index == Last -> {false,Net};
             true -> fast_resend(Net,ai_utp_util:bit16(Index + 1),
                                 Last,ResendCount)
          end;
        Wrap ->
          #utp_packet_wrap{packet = Packet,transmissions = Trans,
                           need_resend = Resend, payload = Payload} = Wrap,
          if Resend == true ->
              case ai_utp_net_util:send(Net,Packet,ReplyMicro) of
                {ok,SendTimeNow} ->
                  OutBuf0 = array:set(Index,Wrap#utp_packet_wrap{
                                              need_resend = false,
                                              transmissions = Trans + 1,
                                              send_time = SendTimeNow},OutBuf),
                  fast_resend(Net#utp_net{last_send = SendTimeNow,
                                          outbuf = OutBuf0},
                              ai_utp_util:bit16(Index + 1),Last,
                              ResendCount - 1);
                _ -> {false,Net}
              end;
             (Trans == 0) andalso (Index == Last)->
              %% 说明是新的包，继续传输直到队列满了
              case ai_utp_net_util:send(Net,Packet,ReplyMicro) of
                {ok,SendTimeNow} ->
                  OutBuf0 = array:set(Index,Wrap#utp_packet_wrap{
                                              need_resend = false,
                                              transmissions = Trans + 1,
                                              send_time = SendTimeNow},OutBuf),
                  {true,Net#utp_net{last_send = SendTimeNow,
                                    cur_window = CurWindow + Payload,
                                    outbuf = OutBuf0,
                                    cur_window_packets = CurWindowPackets + 1}};
                _ -> {false,Net}
              end;
             true -> {false,Net} %% 这种情况不应当发生
          end
      end
  end.

fast_resend(#utp_net{seq_nr = SeqNR,cur_window_packets = CurWindowPackets
                    } = Net,AckNo,Lost)->
  Index = ai_utp_util:bit16(AckNo + 1),
  MaxSend = if ?OUTGOING_BUFFER_MAX_SIZE > CurWindowPackets ->
                ?OUTGOING_BUFFER_MAX_SIZE - CurWindowPackets;
               true -> 1
            end,
  NeedSend = erlang:min(MaxSend,Lost),
  fast_resend(Net#utp_net{last_lost = Lost}, Index, SeqNR,NeedSend).

process_incoming(#utp_net{state = ?CLOSED} = Net,_,_,Proc)-> {Net,Proc};
process_incoming(#utp_net{state = State,ack_nr = AckNR,last_lost = Lost,
                          cur_window_packets = CurWindowPackets } = Net,
                 #utp_packet{type = Type,seq_no = SeqNo} = Packet,
                 Timing,Proc) ->
  Quick =
    if AckNR == undefined -> false;
       true ->
        Wanted = ai_utp_util:bit16(AckNR - 1),
        ai_utp_util:wrapping_compare_less(SeqNo, Wanted, ?ACK_NO_MASK)
    end,
  if Quick == true ->
      Net0 = ai_utp_net_util:send_ack(Net, false),
      if (Lost == 0) andalso
         (CurWindowPackets < ?OUTGOING_BUFFER_MAX_SIZE) ->
          do_send(Net0,Proc,true);
         true -> {Net0,Proc}
      end;
     true ->
      case process_incoming(Type,State,
                              Net#utp_net{last_recv = ai_utp_util:microsecond() },
                              Packet,Timing) of
        {SendNew,Net0} ->
          if SendNew == false -> {Net0,Proc};
             true -> do_send(Net0, Proc,true)
          end;
        Net0 -> {Net0,Proc}
      end
  end.


st_syn(?SYN_RECEIVE,
       #utp_net{ack_nr = AckNR,peer_conn_id = PeerConnID} = Net,
       #utp_packet{seq_no = AckNo,conn_id = PeerConnID},_) ->
  Diff = ai_utp_util:bit16(AckNR - 1),
  if Diff == 1 ->
      ai_utp_net_util:send_syn_state(Net);
     true -> Net
  end;
st_syn(_,Net,_,_) ->Net.

st_data(?SYN_RECEIVE,
        #utp_net{ack_nr = PeerSeqNo} = Net,
        #utp_packet{seq_no = PeerSeqNo,win_sz = WndSize} =  Packet,
        Timing) ->
  Net0 = ai_utp_net_util:change_state(
           Net#utp_net{max_peer_window = WndSize},
           ?ESTABLISHED),
  st_data(?ESTABLISHED,Net0,Packet,Timing);
st_data(?ESTABLISHED,Net,
        #utp_packet{seq_no = SeqNo,payload = Payload,
                    ack_no = AckNo}=Packet,Timing) ->
  case ai_utp_buffer:in(SeqNo,Payload,Net) of
    duplicate -> ai_utp_net_util:send_ack(Net, true);
    {_,Net0} ->
      {Lost,Net1} = ack(Net0,Packet,Timing),
      Net2 = ai_utp_net_util:send_ack(Net1,false),
      fast_resend(Net2,AckNo,Lost)
  end;
st_data(_,Net,_,_) -> Net.


%% 处理链接
st_state(?SYN_SEND,#utp_net{seq_nr = SeqNR} = Net,
        #utp_packet{ack_no = AckNo,seq_no = SeqNo} = Packet,Timing)->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 ->
      Net1 = ai_utp_net_util:change_state(
               Net#utp_net{ack_nr = ai_utp_util:bit16(SeqNo + 1)},
               ?ESTABLISHED),
      {_,Net2} = ack(Net1,Packet,Timing),
      Net2;
     true -> Net
  end;
%% 数据收到响应
st_state(?ESTABLISHED,Net,
         #utp_packet{ack_no = AckNo} = Packet,Timing) ->
  {Lost,Net0} = ack(Net,Packet,Timing),
  fast_resend(Net0,AckNo,Lost).

%% 链接被重置
st_reset(?CLOSED,Net,_,_) -> Net;
st_reset(_,Net,_,_) ->
  ai_utp_net_util:change_state(Net, ?RESET, econnreset).
%%主动关闭, 发送出Fin，对面还没有回复Fin
process_incoming(st_state,?CLOSING,
                 #utp_net{fin_sent = true,
                          got_fin = false,
                          fin_seq_no = SeqNo} = Net,
                 #utp_packet{ack_no = AckNo} = Packet,Timing)->
  if AckNo == SeqNo ->
      %% 对面数据已经接收完整了，安心关闭
      Net#utp_net{fin_acked = true};
     true ->
      %% 对面还没有完全接收完成，继续发包
      {Lost,Net0} = ack(Net,Packet,Timing),
      fast_resend(Net0,AckNo,Lost)
  end;
%%被动关闭方，发送出Fin
process_incoming(st_state,?CLOSING,
                 #utp_net{fin_sent = true,
                          got_fin = true,
                          fin_seq_no = SeqNo} = Net,
                 #utp_packet{ack_no = AckNo},_)->
  if AckNo == SeqNo ->
      {false,Net#utp_net{state = ?CLOSED}};
     true -> {false,Net} %% 网络上延迟的包
  end;
process_incoming(st_data,?CLOSING,
                 #utp_net{fin_sent = FinSent} = Net,
                 #utp_packet{seq_no = SeqNo,
                             payload = Payload} = Packet,
                 Timing)->
  if FinSent == true -> Net; %% 主动关闭方，不再接收数据了
     true ->
      %% 被动关闭方，需要等数据收完全
      case ai_utp_buffer:in(SeqNo,Payload,Net) of
        duplicate -> ai_utp_net_util:send_ack(Net,true); %% 强制发送ACK
        {ok,Net0} ->
          {_,Net1} = ack(Net0,Packet,Timing),
          %% 此处不应该发送新数据的，对面不会再接收了
          Net2 = ai_utp_net_util:send_ack(Net1,false),
          {false,Net2};
        {fin,Net0} ->
          %% 先发送st_state,来说明收到最后的数据包
          {_,Net1} = ack(Net0,Packet,Timing),
          Net2 = ai_utp_net_util:send_ack(Net1,false),
          
          #utp_net{ack_nr = AckNR,seq_nr = SeqNR,
                   reply_micro = ReplyMicro,
                   peer_conn_id = ConnID} = Net2,
          AckNo = ai_utp_util:bit16(AckNR - 1),
          FinSeqNo = ai_utp_util:bit16(SeqNR - 1),
          %% 最后发送Fin包，告诉对方关闭
          Fin = ai_utp_protocol:make_fin_packet(FinSeqNo,AckNo),
          Net3 = Net2#utp_net{fin_sent = true,
                              fin_seq_no = FinSeqNo},
          MaxWindow = ai_utp_net_util:window_size(Net3),
          ai_utp_net_util:send(Net3,
               Fin#utp_packet{conn_id = ConnID,win_sz = MaxWindow},
               ReplyMicro),
          {false,Net3}
      end
  end;
%% 主动关闭方，已经收到了Fin
process_incoming(st_fin,?CLOSING,
                 #utp_net{fin_sent = true,
                          fin_acked = true,
                          fin_seq_no = SeqNo,
                          peer_conn_id = ConnID,
                          reply_micro = ReplyMicro,
                          got_fin = false} = Net,
                 #utp_packet{ack_no = SeqNo, seq_no = AckNo},_)->
  Packet = ai_utp_protocol:make_ack_packet(SeqNo, AckNo),
  MaxWindow = ai_utp_net_util:window_size(Net),
  ai_utp_net_util:send(Net,Packet#utp_packet{conn_id = ConnID,win_sz = MaxWindow},ReplyMicro),
  {false,Net#utp_net{state = ?CLOSED}};
%% 被动关闭方，收到了Fin包
process_incoming(st_fin,?CLOSING,
                 #utp_net{got_fin = true,
                          seq_nr = SeqNR,
                          peer_conn_id = ConnID,
                          reply_micro = ReplyMicro,
                          eof_seq_no = SeqNo} = Net,
                 #utp_packet{seq_no = SeqNo},_)->
  %% 对面的fin acked 丢失了
  ResSeqNo = ai_utp_util:bit16(SeqNR -1 ),
  Packet = ai_utp_protocol:make_ack_packet(ResSeqNo, SeqNo),
  MaxWindow = ai_utp_net_util:window_size(Net),
  ai_utp_net_util:send(Net,Packet#utp_packet{conn_id = ConnID,win_sz = MaxWindow},ReplyMicro),
  {false,Net};
process_incoming(st_fin, _,Net,
                 #utp_packet{seq_no = SeqNo} = Packet,
                 Timing)->
  %% 处理ack包
  {_,Net1} = ack(Net#utp_net{
                   state = ?CLOSING,
                   got_fin = true,
                   fin_seq_no = SeqNo
                  },Packet,Timing),
  Net2 = ai_utp_net_util:send_ack(Net1,false),
  #utp_net{ack_nr = AckNR,seq_nr = SeqNR,
           peer_conn_id = ConnID,
           reply_micro = ReplyMicro} = Net2,
  Diff = ai_utp_util:bit16(AckNR - SeqNo),
  if Diff == 1->
      %%所有的数据包已经收全了
      FinSeqNo = ai_utp_util:bit16(SeqNR - 1),
      %% 最后发送Fin包，告诉对方关闭
      Fin = ai_utp_protocol:make_fin_packet(FinSeqNo,SeqNo),
      Net3 = Net2#utp_net{fin_sent = true,
                          fin_seq_no = FinSeqNo},
      MaxWindow = ai_utp_net_util:window_size(Net3),
      ai_utp_net_util:send(Net3,
           Fin#utp_packet{conn_id = ConnID,win_sz = MaxWindow},
           ReplyMicro),
      {false,Net3};
     true -> {false,Net2}
  end;
process_incoming(st_reset,_,Net,_,_) ->
  Net#utp_net{state = ?CLOSED,error=econnreset};
process_incoming(_,_,Net,_,_) -> Net.



close(#utp_net{seq_nr = SeqNR,peer_conn_id = PeerConnID,
               reply_micro = ReplyMicro,
               ack_nr = AckNR} = Net)->
  MaxWindow = ai_utp_net_util:window_size(Net),
  FinSeqNo = ai_utp_util:bit16(SeqNR - 1),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  Fin = ai_utp_protocol:make_fin_packet(FinSeqNo, AckNo),
  Fin0 = Fin#utp_packet{conn_id = PeerConnID,win_sz = MaxWindow},
  ai_utp_net_util:send(Net,Fin0,ReplyMicro),
  Net#utp_net{state = ?CLOSING,
              fin_sent = true,
              fin_seq_no = FinSeqNo,
              sndbuf_size = 0,
              sndbuf = [],
              last_seq_nr = SeqNR}.

close(#utp_net{sndbuf = SndBuf,
               sndbuf_size = SndBufSize
         } = Net,Proc)->
  %% 所有的数据全清理了
  {Payload,_} = ai_utp_process:flush(Proc),
  Size = erlang:byte_size(Payload),
  Net0 =
    if Size > 0 ->
        Net#utp_net{sndbuf = queue:in({Size,Payload},SndBuf),
                    sndbuf_size = SndBufSize + Size};
       true -> Net
    end,
  Net1 = flush(Net0),
  #utp_net{last_seq_nr = SeqNR,peer_conn_id = PeerConnID,
           reply_micro = ReplyMicro,ack_nr = AckNR } = Net1,
  MaxWindow = ai_utp_net_util:window_size(Net1),
  FinSeqNo = ai_utp_util:bit16(SeqNR - 1),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  Fin = ai_utp_protocol:make_fin_packet(FinSeqNo, AckNo),
  Fin0 = Fin#utp_packet{conn_id = PeerConnID,win_sz = MaxWindow},
  ai_utp_net_util:send(Net1,Fin0,ReplyMicro),
  Net1#utp_net{state = ?CLOSING,
              fin_sent = true,
              fin_seq_no = FinSeqNo
              }.


connect(Net,ConnID)->
  Net0 =
    ai_utp_net_util:change_state(Net#utp_net{
                                   conn_id = ConnID,
                                   peer_conn_id = ai_utp_util:bit16(ConnID + 1),
                                   seq_nr = ai_utp_util:bit16_random()
                                  },?SYN_SEND),
  ai_utp_net_util:send_syn(Net0).


accept(Net,#utp_packet{
              conn_id = PeerConnID,
              seq_no = AckNo,
              win_sz = PeerWinSize},
       {TS,_,Now})->
  SeqNR = ai_utp_util:bit16_random(),
  ConnID = ai_utp_util:bit16(PeerConnID + 1),
  ReplyMicro = Now - TS,
  Net0 =
    ai_utp_net_util:send_syn_state(
      ai_utp_net_util:change_state(Net#utp_net{
                                     last_recv = Now,
                                     max_peer_window = PeerWinSize,
                                     conn_id = ConnID,
                                     peer_conn_id = PeerConnID,
                                     ack_nr = ai_utp_net_util:bit16(AckNo + 1),
                                     seq_nr = SeqNR,
                                     last_seq_nr = SeqNR,
                                     reply_micro = ReplyMicro},?SYN_RECEIVE)),
  {Net0,ConnID}.


state(#utp_net{state = State})-> State.


flush(#utp_net{last_seq_nr = SeqNo,ack_nr = AckNR,
               sndbuf_size = SndBufSize,sndbuf = SndBuf,
               outbuf = OutBuf,peer_conn_id = ConnID
              } = Net)->
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WinSize = ai_utp_net_util:window_size(Net),
  case fill_from_buffer(?PACKET_SIZE,SndBuf,SndBufSize) of
    {filled,Bin,SndBuf0} ->
      Packet = ai_utp_protocol:make_data_packet(SeqNo, AckNo),
      WrapPacket =
        #utp_packet_wrap{packet = Packet#utp_packet{win_sz = WinSize,
                                                    payload = Bin,
                                                    conn_id = ConnID},
                         transmissions = 0,
                         payload = ?PACKET_SIZE,
                         send_time = undefined},
      Net0 = Net#utp_net{last_seq_nr = ai_utp_util:bit16(SeqNo + 1),
                         outbuf = array:set(SeqNo,WrapPacket,OutBuf),
                         sndbuf_size = SndBufSize - ?PACKET_SIZE,
                         sndbuf = SndBuf0},
      flush(Net0);
    {Filled,Bin,SndBuf0}->
      Packet = ai_utp_protocol:make_data_packet(SeqNo, AckNo),
      WrapPacket =
        #utp_packet_wrap{packet = Packet#utp_packet{win_sz = WinSize,
                                                    payload = Bin,
                                                    conn_id = ConnID},
                         transmissions = 0,
                         payload = Filled,
                         send_time = undefined},
      Net#utp_net{
        last_seq_nr = ai_utp_util:bit16(SeqNo + 1),
        outbuf = array:set(SeqNo,WrapPacket,OutBuf),
        sndbuf_size = SndBufSize - Filled,
        sndbuf = SndBuf0};
    zero -> Net
  end.

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
  case ai_utp_net_util:send(Net,Packet,ReplyMicro) of
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
  WinSize = ai_utp_net_util:window_size(Net),
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
                      outbuf = array:set(SeqNo,WrapPacket,OutBuf),
                      sndbuf_size = SndBufSize - ToFill,
                      sndbuf = SndBuf0};
        true ->
          NextSeqNo =  ai_utp_util:bit16(SeqNo + 1),
          Net0 = Net#utp_net{last_send = LastSend,
                             seq_nr = NextSeqNo,
                             last_seq_nr = NextSeqNo,
                             cur_window = CurWindow + ToFill,
                             cur_window_packets = CurWindowPackets + 1,
                             outbuf = array:set(SeqNo,WrapPacket,OutBuf),
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
            outbuf = array:set(SeqNo,WrapPacket,OutBuf),
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
            outbuf = array:set(SeqNo,WrapPacket,OutBuf),
            sndbuf_size = SndBufSize - Filled,
            sndbuf = SndBuf0}
      end;
    zero -> Net
  end;
%% quick send SeqNR =< SeqNO < LastSeqNR
%% and total payload less or equal Bytes
fill_tx_queue(#utp_net{
                 seq_nr = SeqNR,
                 last_seq_nr = LastSeqNR} = Net,Bytes) ->
  case fast_send(Net,SeqNR,LastSeqNR,Bytes) of
    {false,Net0}-> Net0;
    {true,Net0} ->
      MaxSendBytes = ai_utp_net_util:max_send_bytes(Net0),
      fill_tx_queue(Net0,MaxSendBytes)
  end.



fast_send(Net,NextSeqNR,_,0)->
  {false,Net#utp_net{seq_nr = NextSeqNR}};
%% 稍后重新计算发送窗口
fast_send(Net,Last,Last,_) ->
  {true,Net#utp_net{seq_nr = Last}};
fast_send(#utp_net{
             reply_micro = ReplyMicro,
             ack_nr = AckNR,
              cur_window_packets = CurWindowPackets,
             cur_window = CurWindow,
             outbuf = OutBuf} = Net,Index,Last,Bytes) ->

  Wrap = array:get(Index,OutBuf),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WindowSize = ai_utp_net_util:window_size(Net),
  #utp_packet_wrap{
     packet = Packet,
     payload = Payload
    } = Wrap,
  if Payload > Bytes ->
      {false,Net#utp_net{seq_nr = Index}};
     true ->
      Packet0 = Packet#utp_packet{ack_no = AckNo,win_sz = WindowSize},
      case ai_utp_net_util:send(Net,Packet0,ReplyMicro) of
        {ok,SendTimeNow} ->
          Net0 = Net#utp_net{cur_window_packets = CurWindowPackets + 1,
                             cur_window = CurWindow + Payload,
                             last_send = SendTimeNow,
                             outbuf = array:set(Index,
                                                #utp_packet_wrap{
                                                   send_time = SendTimeNow,
                                                   transmissions = 1,
                                                   packet = Packet0},OutBuf)},
          fast_send(Net0,ai_utp_util:bit16(Index + 1),Last,Bytes - Payload);
        true -> {false,Net#utp_net{seq_nr = Index}}
      end
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
do_send(#utp_net{state = ?ESTABLISHED } = Net,Proc)->
  %% for some small and senstive data
  %% 100ms is too big
  do_send(Net,Proc,true);
do_send(#utp_net{state = ?CLOSING,fin_sent = true} = Net,Proc)->
  Proc0 = ai_utp_process:error_all(Proc, {error,eshutdown}),
  {Net,Proc0};
do_send(Net,Proc) ->{Net,Proc}.


do_send(Net,Proc,Quick)->
  MaxBufSize = ai_utp_net_util:sndbuf_remain(Net),
  MaxSendBytes = ai_utp_net_util:max_send_bytes(Net),
  {Net0,Proc0} = fill_buffer(Net,MaxBufSize + MaxSendBytes,Proc),
  #utp_net{sndbuf_size = SndBufSize } = Net0,
  if SndBufSize > 0 ->
      if (Quick == true) orelse
         (Net0#utp_net.sndbuf_size >= ?MIN_PACKET_SIZE)->
          Net1 = fill_tx_queue(Net0,MaxSendBytes),
          {Net1,Proc0};
         true -> {Net0,Proc0}
      end;
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
expire_resend(Net,_,0,_) -> {false,Net};
expire_resend(#utp_net{reply_micro = ReplyMicro,
                       seq_nr = Last,
                       rto = RTO,
                       outbuf = OutBuf} = Net,Index,ResendCount,Now) ->
  Less = ai_utp_util:wrapping_compare_less(Index, Last, ?ACK_NO_MASK),
  if Less == true ->
      case array:get(Index,OutBuf) of
        undefined ->
          expire_resend(Net,ai_utp_util:bit16(Index + 1),ResendCount-1,Now);
        Wrap ->
          #utp_packet_wrap{packet = Packet,
                           send_time = SendTime,
                           transmissions = Trans} = Wrap,
          Diff = (Now - SendTime) div 1000,
          if (Trans > 0) andalso (Diff > RTO) ->
              case ai_utp_net_util:send(Net,Packet,ReplyMicro) of
                {ok,SendTimeNow}->
                  OutBuf0 = array:set(Index,Wrap#utp_packet_wrap{
                                              transmissions =  Trans + 1,
                                              send_time = SendTimeNow },OutBuf),
                  expire_resend(Net#utp_net{outbuf = OutBuf0},
                                ai_utp_util:bit16(Index + 1),ResendCount - 1,Now);
                _ -> {false,Net}
              end;
            true ->
              %% 默认是可以发送的，最终是否能发送，根据滑动窗口发送
              {true,Net}
          end
      end;
     true -> {true,Net}
  end.

expire_resend(#utp_net{seq_nr = SeqNR,
                       cur_window_packets = CurWindowPackets} = Net, Now)->
  if CurWindowPackets > 0 ->
      WindowStart = ai_utp_util:bit16(SeqNR - CurWindowPackets),
      ResendCount = erlang:min(CurWindowPackets,?OUTGOING_BUFFER_MAX_SIZE),
      expire_resend(Net,WindowStart,ResendCount,Now);
     true -> {true,Net}
  end.
force_state(State,#utp_net{last_send = LastSend,
                          rto = RTO } = Net)->
  Now = ai_utp_util:microsecond(),
  if (State == ?ESTABLISHED) orelse (State == ?CLOSING)->
      Diff = Now - LastSend,
      if (Diff >= ?MAX_SEND_IDLE_TIME andalso State == ?ESTABLISHED) orelse
         (Diff >= (?MAX_CLOSING_WAIT - RTO) andalso State == ?MAX_CLOSING_WAIT)->
          ai_utp_net_util:send_ack(Net,true);
         (Diff div 1000 > RTO * 1.5) -> ai_utp_net_util:send_ack(Net, false);
         true-> Net
      end;
     true  -> Net
  end.
%% 主动关闭的一方
on_tick(?CLOSING,
        #utp_net{fin_sent = true,
                 fin_seq_no = FinSeqNo,
                 fin_acked = FinAcked,
                 got_fin = false,
                 last_recv = LastReceived,
                 last_send = LastSend,
                 peer_conn_id = ConnID,
                 rto = RTO,
                 ack_nr = AckNR,
                 reply_micro = ReplyMicro,
                 state = State
          } = Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = (Now - LastReceived) div 1000,
  if Diff >= RTO * 2 ->
      {Net#utp_net{state = ?CLOSED}, Proc};
     true ->
      if FinAcked == true -> ok;
         true ->
          SendDiff = (Now - LastSend) div 1000,
          if SendDiff < RTO -> ok;
             true ->
              AckNo = ai_utp_util:bit16(AckNR -1 ),
              Fin = ai_utp_protocol:make_fin_packet(FinSeqNo, AckNo),
              ai_utp_net_util:send(Net,Fin#utp_packet{win_sz = 0,conn_id = ConnID},ReplyMicro)
          end
      end,
      {SendNew,Net0} = expire_resend(Net, Now),
      if SendNew == true ->
          {Net1,Proc0} = do_send(Net0,Proc,true),
          Net2 = force_state(State, Net1),
          {Net2,Proc0};
         true ->
          Net1 = force_state(State, Net),
          {Net1,Proc}
      end
  end;
%%
on_tick(?CLOSING,
        #utp_net{
           got_fin = true,
           state = State,
           fin_sent = FinSent,
           rto = RTO,
           last_recv = LastReceived
          } = Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = (Now - LastReceived) div 1000,
  if Diff >= 2 * RTO ->
      {Net#utp_net{state = ?CLOSED}, Proc};
     true ->
      if FinSent == true ->
          Net1 = force_state(State, Net),
          {Net1,Proc};
         true  -> {Net,Proc}
      end
  end;
on_tick(?CLOSED,Net,Proc)-> {Net,Proc};
on_tick(?SYN_SEND,#utp_net{rto = RTO,
                           last_send = LastSend,
                           syn_sent_count = SynSentCount
                          } = Net,Proc)
  when SynSentCount > ?DUPLICATE_ACKS_BEFORE_RESEND ->
  Now = ai_utp_util:microsecond(),
  Diff = (Now - LastSend) div 1000,
  if Diff >= (RTO * ?DUPLICATE_ACKS_BEFORE_RESEND) ->
      {ai_utp_net_util:change_state(Net, ?CLOSED, etimeout),Proc};
     true -> Net
  end;

on_tick(?SYN_SEND,#utp_net{syn_sent_count = SynSentCount,
                           last_send = LastSend,
                           rto = RTO} = Net,Proc)->
  Now =  ai_utp_util:microsecond(),
  Diff = (Now - LastSend) div 1000,
  if Diff >(RTO * SynSentCount) ->
      {ai_utp_net_util:send_syn(Net),Proc};
     true -> {Net,Proc}
  end;


on_tick(?SYN_RECEIVE,#utp_net{rto = RTO,
                           last_send = LastSend,
                           syn_sent_count = SynSentCount
                          } = Net,Proc)
  when SynSentCount > ?DUPLICATE_ACKS_BEFORE_RESEND ->
  Now = ai_utp_util:microsecond(),
  Diff = (Now - LastSend) div 1000,
  if Diff > (RTO * ?DUPLICATE_ACKS_BEFORE_RESEND) ->
      {ai_utp_net_util:change_state(Net, ?CLOSED, etimeout),Proc};
     true -> Net
  end;

on_tick(?SYN_RECEIVE,#utp_net{syn_sent_count = SynSentCount,
                              rto = RTO,
                              last_send = LastSend} = Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = (Now -LastSend) div 1000,
  if Diff >= (RTO * SynSentCount) ->
      {ai_utp_net_util:send_syn_state(Net),Proc};
     true -> {Net,Proc}
  end;

on_tick(State,#utp_net{last_recv = LastReceived} =  Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastReceived,
  if Diff >= ?MAX_RECV_IDLE_TIME ->
      {Net#utp_net{state = ?CLOSED,error = econnaborted}, Proc};
     true ->
      {SendNew,Net0} = expire_resend(Net, Now),
      if SendNew == true ->
          {Net1,Proc0} = do_send(Net0,Proc,true),
          Net2 = force_state(State, Net1),
          {Net2,Proc0};
         true ->
          Net1 = force_state(State, Net),
          {Net1,Proc}
      end
  end.

net_error(#utp_net{error = Error})-> Error.
