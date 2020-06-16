-module(ai_utp_tx).
-include("ai_utp.hrl").

-export([fill_sndbuf/3,fill_window/2,flush/1]).

fill_sndbuf(Net,0,Proc)-> {Net,Proc};
fill_sndbuf(#utp_net{sndbuf = SndBuf,
                    sndbuf_size = SndBufSize} = Net,Bytes,Proc) ->
  case ai_utp_process:fill_sndbuf(Bytes, Proc) of
    {filled,Bin,Proc0}->
      {Net#utp_net{sndbuf = queue:in({Bytes,Bin},SndBuf),
                   sndbuf_size = SndBufSize + Bytes},Proc0};
    {partial,Bin,Proc0}->
      Size = erlang:byte_size(Bin),
      {Net#utp_net{sndbuf = queue:in({Size,Bin},SndBuf),
                   sndbuf_size = SndBufSize + Size },Proc0};
    zero -> {Net,Proc}
  end.

send_packet(Net,ToFill,Packet,ReplyMicro)->
  case ai_utp_net_util:send(Net,Packet,ReplyMicro) of
    {ok,Now} ->
      {Now,#utp_packet_wrap{packet = Packet,transmissions = 1,
                            payload = ToFill,send_time = Now}};
    _ ->
      {undefined,#utp_packet_wrap{packet = Packet,transmissions = 0,
                                  payload = ToFill,send_time = undefined}}
  end.

%% 对于所有没有发送成功的包，都不记录在发送窗口内
fill_window(Net,0) -> Net;
fill_window(#utp_net{sndbuf_size = 0}= Net,_) -> Net;
fill_window(#utp_net{
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
  case fill_from_sndbuf(ToFill,SndBuf,SndBufSize) of
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
          fill_window(Net0,Bytes - ToFill)
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
%% quick send SeqNR =< SeqNo < LastSeqNR
%% and total payload less or equal Bytes
fill_window(#utp_net{
                 seq_nr = SeqNR,
                 last_seq_nr = LastSeqNR} = Net,Bytes) ->
  case fast_send(Net,SeqNR,LastSeqNR,Bytes) of
    {false,Net0}-> Net0;
    {true, Net0} ->
      MaxSendBytes = ai_utp_net_util:max_send_bytes(Net0),
      fill_window(Net0,MaxSendBytes)
  end.



fast_send(Net,NextSeqNR,_,0)-> {false,Net#utp_net{seq_nr = NextSeqNR}};
%% 稍后重新计算发送窗口
fast_send(Net,Last,Last,_) -> {true,Net#utp_net{seq_nr = Last}};
fast_send(#utp_net{
             reply_micro = ReplyMicro,
             ack_nr = AckNR,
             cur_window_packets = CurWindowPackets,
             cur_window = CurWindow,
             outbuf = OutBuf} = Net,Index,Last,Bytes) ->

  Wrap = array:get(Index,OutBuf),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WindowSize = ai_utp_net_util:window_size(Net),
  #utp_packet_wrap{packet = Packet,payload = Payload} = Wrap,
  if Payload > Bytes -> {false,Net#utp_net{seq_nr = Index}};
     true ->
      Packet0 = Packet#utp_packet{ack_no = AckNo,win_sz = WindowSize},
      case ai_utp_net_util:send(Net,Packet0,ReplyMicro) of
        {ok,SendTimeNow} ->
          Wrap0 = #utp_packet_wrap{send_time = SendTimeNow,transmissions = 1,
                                   packet = Packet0},
          Net0 = Net#utp_net{cur_window_packets = CurWindowPackets + 1,
                             cur_window = CurWindow + Payload,
                             last_send = SendTimeNow,
                             outbuf = array:set(Index,Wrap0,OutBuf)},
          fast_send(Net0,ai_utp_util:bit16(Index + 1),Last,Bytes - Payload);
        true -> {false,Net#utp_net{seq_nr = Index}}
      end
  end.


fill_from_sndbuf(_,_,0) -> zero;
fill_from_sndbuf(ToFill,SndBuf,SndBufSize)
  when ToFill >= SndBufSize ->
  Bin = lists:foldl(
          fun({_,Bin},Acc)-> <<Acc/binary,Bin/binary>> end,
          <<>>,queue:to_list(SndBuf)),
  {SndBufSize,Bin,queue:new()};
fill_from_sndbuf(ToFill,SndBuf,_) -> dequeue_sndbuf(ToFill,SndBuf,<<>>).

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

flush(#utp_net{last_seq_nr = SeqNo,ack_nr = AckNR,
               sndbuf_size = SndBufSize,sndbuf = SndBuf,
               outbuf = OutBuf,peer_conn_id = ConnID
              } = Net)->
  AckNo = ai_utp_util:bit16(AckNR - 1),
  WinSize = ai_utp_net_util:window_size(Net),
  case fill_from_sndbuf(?PACKET_SIZE,SndBuf,SndBufSize) of
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
