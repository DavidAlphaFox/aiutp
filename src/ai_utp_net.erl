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
      do_fast_resend(Net,ai_utp_util:bit16(Index + 1),ResendCount -1);
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
  MaxSend = erlang:min(Diff,4),
  Index = ai_utp_util:bit16(AckNo + 1),
  do_fast_resend(Net#utp_net{last_lost = Lost}, Index,MaxSend).

process_incoming(#utp_net{state = ?CLOSED} = Net,_,_,Proc)-> {Net,Proc};
process_incoming(#utp_net{state = State,cur_window_packets = CurWindowPackets,
                          ext_bits = ExtBits} = Net,
                 #utp_packet{type = Type,extension = Ext} = Packet,
                {_,_,Now} = Timing,Proc) ->
  PacketExtBits = proplists:get_value(ext_bits, Ext),
  if PacketExtBits == ExtBits orelse ExtBits == undefined ->
      Net0 = Net#utp_net{last_recv = Now },
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
      end;
     true  -> {Net,Proc}
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
      Net2 =
        if Net#utp_net.inbuf_size /= Net1#utp_net.inbuf_size orelse
           Net#utp_net.ack_nr /= Net1#utp_net.ack_nr ->
            ai_utp_net_util:send_ack(Net1,false);
           true -> Net1
        end,
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
  Net1 = ai_utp_tx:flush(Net0),
  #utp_net{last_ack = LastAck,last_seq_nr = SeqNR,
           cur_window_packets = CurWindowPackets} = Net1,
  if LastAck /= undefined ->
      Diff = ai_utp_util:bit16(SeqNR - LastAck),
      %% 最后一个ACK和下一个包号相差1，说明已经都响应了
      if Diff == 1 ->
          0 = CurWindowPackets, %% 断言，如果不等一定会崩溃
          ai_utp_net_util:send_fin(
            ai_utp_net_util:change_state(Net1, ?CLOSING));
         true ->
          %% 变状态,但是不发送fin包
          ai_utp_net_util:change_state(Net1, ?CLOSING)
      end;
     true ->
      ai_utp_net_util:change_state(Net1, ?CLOSING)
  end.
  
connect(Net,ConnID)->
  SeqNR = ai_utp_util:bit16_random(),
  H = ai_utp_util:bit32_random(),
  L = ai_utp_util:bit32_random(),
  ExtBits = <<H:32/big-unsigned-integer,
              L:32/big-unsigned-integer>>,

  Net0 =
    ai_utp_net_util:change_state(Net#utp_net{
                                   conn_id = ConnID,
                                   peer_conn_id = ai_utp_util:bit16(ConnID + 1),
                                   seq_nr = SeqNR,
                                   last_seq_nr = SeqNR,
                                   ext_bits = ExtBits
                                  },?SYN_SEND),
  ai_utp_net_util:send_syn(Net0).


accept(Net,#utp_packet{
              conn_id = PeerConnID,
              seq_no = AckNo,
              win_sz = PeerWinSize,
              extension = Ext},
       {TS,_,Now})->
  ExtBits = proplists:get_value(ext_bits, Ext),
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
                                     reply_micro = ReplyMicro,
                                     ext_bits = ExtBits},?SYN_RECEIVE)),
  {Net0,ConnID}.


state(#utp_net{state = State})-> State.


%% try to full fill one package
do_send(#utp_net{state = ?ESTABLISHED } = Net,Proc)->
  %% for some small and senstive data
  %% 100ms is too big
  do_send(Net,Proc,true);
do_send(#utp_net{state = ?CLOSING,fin_sent = true} = Net,Proc)->
  Proc0 = ai_utp_process:error_all(Proc, eshutdown),
  {Net,Proc0};
do_send(#utp_net{state = ?CLOSED} = Net,Proc)->
  Proc0 = ai_utp_process:error_all(Proc, closed),
  {Net,Proc0};
do_send(Net,Proc) ->{Net,Proc}.


do_send(Net,Proc,Quick)->
  MaxBufSize = ai_utp_net_util:sndbuf_remain(Net),
  MaxSendBytes = ai_utp_net_util:max_send_bytes(Net),
  {Net0,Proc0} = ai_utp_tx:fill_sndbuf(Net,MaxBufSize + MaxSendBytes,Proc),
  #utp_net{sndbuf_size = SndBufSize } = Net0,
  if SndBufSize > 0 ->
      if (Quick == true) orelse
         (Net0#utp_net.sndbuf_size >= ?MIN_PACKET_SIZE)->
          Net1 = ai_utp_tx:fill_window(Net0,MaxSendBytes),
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
force_state(State, #utp_net{last_send = LastSend } = Net)->
  Now = ai_utp_util:microsecond(),
  if (State == ?ESTABLISHED orelse State == ?CLOSING) andalso
      (Now - LastSend) >= ?MAX_SEND_IDLE_TIME ->
      ai_utp_net_util:send_ack(Net,true);
     true -> Net
  end.

on_tick(?CLOSED,Net,Proc)-> {Net,Proc};
on_tick(?SYN_SEND,#utp_net{rto = RTO,
                           last_send = LastSend,
                           syn_sent_count = SynSentCount
                          } = Net,Proc)
  when SynSentCount > ?MAX_SYN_RESNED ->
  Now = ai_utp_util:microsecond(),
  Diff = (Now - LastSend) div 1000,
  if Diff >= (RTO * ?MAX_SYN_RESNED) ->
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
  when SynSentCount > ?MAX_SYN_RESNED ->
  Now = ai_utp_util:microsecond(),
  Diff = (Now - LastSend) div 1000,
  if Diff > (RTO * ?MAX_SYN_RESNED) ->
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
