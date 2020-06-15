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
ack(#utp_net{last_ack = LastAck,
             state = State,seq_nr = SeqNR} =Net,
    #utp_packet{ack_no = AckNo,
                win_sz = WndSize,extension = Ext},
    {_,_,Now} = Timing)->
  LastAck0 =
    if LastAck == undefined -> ai_utp_util:bit16(AckNo -1);
       true -> LastAck
    end,
  Less = ai_utp_util:wrapping_compare_less(LastAck0,AckNo,?ACK_NO_MASK),
  %% 只更新了reorder 或者收到重复包
  Result =
    if Less == true  ->
        SAcks = proplists:get_value(sack, Ext,undefined),
        {Lost,AckPackets,Net0} = ai_utp_rx:ack_packet(AckNo,SAcks, Net),
        {MinRTT,Times,AckBytes} = ack_bytes(AckPackets,Now),
        {Lost,ai_utp_cc:cc(Net0#utp_net{last_ack = AckNo},Timing, MinRTT,
                           AckBytes,Lost,lists:reverse(Times),WndSize)};
       true -> {0,Net}
    end,
  case State of
    ?CLOSING ->
      Diff = ai_utp_util:bit16(SeqNR - AckNo),
      if Diff == 1 ->
          {Lost0,Net1} = Result,
          {Lost0,ai_utp_net_util:send_fin(
               ai_utp_net_util:change_state(Net1, ?CLOSED))};
         true -> Result
      end;
    _ -> Result
  end.

        


do_fast_resend(Net,_,0)-> Net;
do_fast_resend(#utp_net{reply_micro = ReplyMicro,
                     outbuf = OutBuf} = Net,
               Index,ResendCount) ->
  case array:get(Index,OutBuf) of
    undefined ->
      do_fast_resend(Net,ai_utp_util:bit16(Index + 1),ResendCount-1);
    Wrap ->
      #utp_packet_wrap{packet = Packet,transmissions = Trans,
                       need_resend = Resend} = Wrap,
      if Resend == true andalso Trans > 0 ->
          case ai_utp_net_util:send(Net,Packet,ReplyMicro) of
            {ok,SendTimeNow} ->
              OutBuf0 = array:set(Index,Wrap#utp_packet_wrap{
                                          need_resend = false,
                                          transmissions = Trans + 1,
                                          send_time = SendTimeNow},OutBuf),
              do_fast_resend(Net#utp_net{last_send = SendTimeNow,
                                         outbuf = OutBuf0},
                             ai_utp_util:bit16(Index + 1),ResendCount - 1);
            _ -> Net
          end;
         true -> Net
      end
  end.

fast_resend(#utp_net{seq_nr = SeqNR} = Net,AckNo,Lost)->
  Diff = ai_utp_util:bit16(SeqNR - AckNo -1),
  MaxSend = erlang:min(Diff,10),
  Index = ai_utp_util:bit16(AckNo + 1),
  do_fast_resend(Net#utp_net{last_lost = Lost}, Index,MaxSend).

process_incoming(#utp_net{state = ?CLOSED} = Net,_,_,Proc)-> {Net,Proc};
process_incoming(#utp_net{state = State,cur_window_packets = CurWindowPackets
                          } = Net,
                 #utp_packet{type = Type} = Packet,
                 Timing,Proc) ->

  Net0 = Net#utp_net{last_recv = ai_utp_util:microsecond() },
  Net1 =
    case Type of
      st_syn -> st_syn(State,Net0,Packet,Timing);
      st_data -> st_data(State,Net0,Packet,Timing);
      st_state -> st_state(State,Net0,Packet,Timing);
      st_fin -> st_fin(State,Net0,Packet,Timing);
      st_reset -> st_reset(State,Net0,Packet,Timing)
    end,
  if ?OUTGOING_BUFFER_MAX_SIZE > CurWindowPackets ->
      do_send(Net1,Proc,true);
     true -> {Net1,Proc}
  end.


st_syn(?SYN_RECEIVE,
       #utp_net{ack_nr = AckNR,peer_conn_id = PeerConnID} = Net,
       #utp_packet{seq_no = AckNo,conn_id = PeerConnID},_) ->
  Diff = ai_utp_util:bit16(AckNR - AckNo),
  if Diff == 1 ->
      ai_utp_net_util:send_syn_state(Net);
     true -> Net
  end;
st_syn(_,Net,_,_) ->Net.

%% 被链接方，收到第二个包，必须是数据包？
%% 这就存在一个问题，被链接方不能在SYN阶段主动广播数据
%% 上层协议设计时，就需要让链接方，发出第一个请求
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
  case ai_utp_rx:in(SeqNo,Payload,Net) of
    duplicate -> ai_utp_net_util:send_ack(Net, false);
    {_,Net0} ->
      {Lost,Net1} = ack(Net0,Packet,Timing),
      Net2 = ai_utp_net_util:send_ack(Net1,false),
      fast_resend(Net2,AckNo,Lost)
  end;
st_data(_,Net,_,_) -> Net.


%% 处理链接
%% 数据收到响应
st_state(?ESTABLISHED,Net,
         #utp_packet{ack_no = AckNo} = Packet,Timing) ->
  {Lost,Net0} = ack(Net,Packet,Timing),
  fast_resend(Net0,AckNo,Lost);
st_state(?CLOSING,Net,#utp_packet{ack_no = AckNo} = Packet,Timing) ->
  {Lost,Net0} = ack(Net,Packet,Timing),
  fast_resend(Net0,AckNo,Lost);
st_state(?SYN_SEND,#utp_net{seq_nr = SeqNR} = Net,
        #utp_packet{ack_no = AckNo,seq_no = SeqNo},_)->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 ->
      Net0 = ai_utp_net_util:change_state(
               Net#utp_net{ack_nr = ai_utp_util:bit16(SeqNo + 1)},
               ?ESTABLISHED),
      ai_utp_net_util:send_ack(Net0,true);
     true -> Net
  end;
st_state(?SYN_RECEIVE,#utp_net{ack_nr = AckNR} = Net,
         #utp_packet{seq_no = AckNo},_)->
  Diff = ai_utp_util:bit16(AckNR - AckNo),
  if Diff == 1 -> ai_utp_net_util:change_state(Net,?ESTABLISHED);
     true -> Net
  end;
st_state(?CLOSE_WAIT,Net,_,_) -> Net.


%% 主动关闭方是CLOSING
%% 被动关闭方是CLOSE_WAIT
st_fin(?CLOSING,Net,_,_)->
  ai_utp_net_util:change_state(Net, ?CLOSED);
st_fin(?CLOSE_WAIT,Net,_,_) ->
  ai_utp_net_util:send_fin(Net);
st_fin(_,Net,#utp_packet{seq_no = SeqNo},_) ->
  ai_utp_net_util:change_state(
    Net#utp_net{got_fin = true,eof_seq_no = SeqNo},
    ?CLOSE_WAIT).



%% 链接被重置
st_reset(?CLOSED,Net,_,_) -> Net;
st_reset(_,Net,_,_) ->
  ai_utp_net_util:change_state(Net, ?CLOSED, econnreset).
close(Net)->
  ai_utp_net_util:change_state(Net, ?CLOSED, econnaborted).
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
  #utp_net{last_ack = LastAck,last_seq_nr = SeqNR,
           cur_window_packets = CurWindowPackets} = Net1,
  Diff = ai_utp_util:bit16(SeqNR - LastAck),
  %% 最后一个ACK和下一个包号相差1，说明已经都响应了
  if Diff == 1 ->
      0 = CurWindowPackets, %% 断言，如果不等一定会崩溃
      ai_utp_net_util:send_fin(
        ai_utp_net_util:change_state(Net1, ?CLOSING));
     true ->
      %% 变状态,但是不发送fin包
      ai_utp_net_util:change_state(Net1, ?CLOSING)
  end.
  
connect(Net,ConnID)->
  SeqNR = ai_utp_util:bit16_random(),
  Net0 =
    ai_utp_net_util:change_state(Net#utp_net{
                                   conn_id = ConnID,
                                   peer_conn_id = ai_utp_util:bit16(ConnID + 1),
                                   seq_nr = SeqNR,
                                   last_seq_nr = SeqNR
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
                                     ack_nr = ai_utp_util:bit16(AckNo + 1),
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
force_state(State,#utp_net{last_send = LastSend,rto = RTO} = Net)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastSend,
  if (State == ?ESTABLISHED orelse State == ?CLOSING)->
      if
        Diff >= ?MAX_SEND_IDLE_TIME ->
          ai_utp_net_util:send_ack(Net,true);
        (Diff div 1000) >= RTO * 1.5 ->
          ai_utp_net_util:send_ack(Net,flase);
        true-> Net
      end;
     true -> Net
  end.

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
     true -> {Net,Proc}
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
     true -> {Net,Proc}
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
on_tick(?CLOSE_WAIT,#utp_net{last_state_changed = LastStateChanged} = Net,
        Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastStateChanged,
  if Diff > ?MAX_CLOSE_WAIT ->
      {ai_utp_net_util:change_state(Net, ?CLOSED),Proc};
     true -> {Net,Proc}
  end;
%% 发送fin包，并且等待时间超过5s了
on_tick(?CLOSING,
        #utp_net{fin_sent = true,
                 last_state_changed = LastStateChanged } = Net,
        Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastStateChanged,
  if Diff > ?MAX_CLOSE_WAIT ->
      {ai_utp_net_util:change_state(Net, ?CLOSED),Proc};
     true -> {Net,Proc}
  end;
on_tick(State,#utp_net{last_recv = LastReceived,conn_id = ConnID} =  Net,Proc)->
  Now = ai_utp_util:microsecond(),
  Diff = Now - LastReceived,
  if Diff >= ?MAX_RECV_IDLE_TIME ->
      logger:error("RECV IDLE TIMEOUT ConnID: ~p~n",[ConnID]),
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
