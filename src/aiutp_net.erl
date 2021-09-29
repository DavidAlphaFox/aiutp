-module(aiutp_net).
-include("aiutp.hrl").
-export([window_size/2,
         is_full/2,
         flush_packets/2,
         send_ack/2,
         send_keep_alive/2,
         send_packet/3,
         send_packet_in_range/5]).

window_size(MaxWindow,InBuf) ->
  ?MIN(((255 - aiutp_buff:size(InBuf)) * ?PACKET_SIZE),MaxWindow).

is_full(Bytes,#aiutp_pcb{time= {Now,_},
                         max_window = MaxWindow,
                         max_window_user = MaxWindowUser,
                         cur_window_packets = CurWindowPackets,
                         cur_window = CurWindow
                        } = PCB)->
  Bytes0 = if Bytes > ?PACKET_SIZE -> ?PACKET_SIZE;
              Bytes =< 0 -> ?PACKET_SIZE;
              true -> Bytes
           end,

  MaxSend = ?MIN(MaxWindow, MaxWindowUser),
  if CurWindowPackets >= (?OUTGOING_BUFFER_MAX_SIZE - 1) ->
      {true,PCB#aiutp_pcb{last_maxed_out_window = Now}};
     (CurWindow + Bytes) > MaxSend ->
      {true,PCB#aiutp_pcb{last_maxed_out_window = Now}};
     true -> {false,PCB}
  end.


build_sack(0,Acc,_,_,_)->
  lists:foldl(
    fun(BI,BAcc)->
        Bits = maps:get(BI,Acc),
        <<BAcc/binary,Bits/big-unsigned-integer>>
    end, <<>>, lists:seq(0, 3));

build_sack(Size,Acc,Base,Iter,InBuf) ->
  Packet = aiutp_buffer:data(Iter,InBuf),
  Index = Packet#aiutp_packet.seq_nr - Base,
  Pos = Index bsr 3,
  if Pos > 3 -> build_sack(0,Acc,Base,Iter,InBuf);
     true ->
      Iter0 = aiutp_buffer:next(Iter, InBuf),
      Mask = 1 bsl (Index band 7),
      Bits = maps:get(Pos,Acc),
      Bits0 = Mask bor Bits,
      build_sack(Size - 1,maps:put(Pos,Bits0,Acc),Base,Iter0,InBuf)
  end.

build_sack(#aiutp_pcb{ack_nr = AckNR,inbuf = InBuf})->
  Size = aiutp_buffer:size(InBuf),
  Size0 = ?MIN(30,Size),
  if Size0 == 0 -> undefined;
     true ->
      Acc = list:foldl(fun(Idx,Map)-> maps:put(Idx,0,Map) end,#{},lists:seq(0,3)),
      Head = aiutp_buffer:head(InBuf),
      build_sack(Size0,Acc,AckNR + 2,Head,InBuf)
  end.

send_ack(Acc,#aiutp_pcb{time = {Now,_},
                        conn_id_send = ConnIdSend,
                        seq_nr = SeqNR, ack_nr = AckNR,
                        reorder_count = ReorderCount,
                        got_fin_reached = GotFinReached,
                        inbuf = InBuf,max_window = MaxWindow,
                        reply_micro = ReplyMicro} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WindowSize = window_size(MaxWindow,InBuf),
  Packet = aiutp_packet:ack(SeqNR, AckNR),
  Packet0 =
    if (ReorderCount /= 0) and (not GotFinReached) ->
        Sack = build_sack(PCB),
        Packet#aiutp_packet{conn_id = ConnIdSend, wnd = WindowSize,
                            tv_usec = MicroNow,reply_micro = ReplyMicro,
                            extension = [{sack,Sack}]};
       true ->
        Packet#aiutp_packet{ conn_id = ConnIdSend,
                             tv_usec = MicroNow,reply_micro = ReplyMicro,
                             wnd = WindowSize}
    end,
  {[aiutp_packet:encode(Packet0)|Acc],PCB#aiutp_pcb{last_sent_packet = Now}}.

send_keep_alive(Acc,#aiutp_pcb{ack_nr = AckNR} = PCB)-> send_ack(Acc,PCB#aiutp_pcb{ack_nr = AckNR -1}).

update_wrap_packet(MicroNow,ReplyMicro,WindowSize,AckNR,WrapPacket)->
    #aiutp_packet_wrap{
       transmissions = Transmission,
       content = Content,
       packet = Packet,
       need_resend = NeedResend,
       payload = Payload
      } = WrapPacket,
  if Transmission == 0 ->
      Packet0 = Packet#aiutp_packet{tv_usec = MicroNow,reply_micro = ReplyMicro,
                                      ack_nr = AckNR,wnd = WindowSize},
      Content0 = aiutp_packet:encode(Packet0),
      Payload0 = erlang:byte_size(Content),
      { Payload0,
        WraPacket#aiutp_packet_wrap{transmissions = Transmission + 1,need_resend = false,
                                    payload = Payload0,time_sent = MicroNow,
                                    packet = Packet0,content = Content0},
        Content};
     true ->
      Packet0 = Packet#aiutp_packet{tv_usec = MicroNow,reply_micro = ReplyMicro,
                                    ack_nr = AckNR, wnd = WindowSize},
      Content0 = aiutp_packet:encode(Packet0),
      Payload0 =
        if NeedResend == true -> Payload;
           true -> 0
        end,
        {Payload0,
         WraPacket#aiutp_packet_wrap{transmissions = Transmission + 1,need_resend = false,
                                     time_sent = MicroNow,packet = Packet0,content = Content0},
         Content0}
    end.


send_packet(Acc,-1,PCB)->{Acc,PCB};
send_packet(Acc,Pos,#aiutp_pcb{time = {Now,_},
                               ack_nr = AckNR,cur_window = CurWindow,
                               inbuf = InBuf,max_window = MaxWindow,
                               outbuf = OutBuf,replay_micro = ReplyMicro} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WrapPacket = aiutp_buffer:data(Pos,OutBuf),
  WindowSize = window_size(MaxWindow, InBuf),
  {SendBytes,WrapPacket0,Content1} = update_wrap_packet(MicroNow,ReplyMicro,
                                                        WindowSize,AckNR,WrapPacket),
  OutBuf0 = aiutp_buffer:replace(Pos,WrapPacket0,OutBuf),
  {[Content1|Acc],PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,outbuf = OutBuf0,last_sent_packet = Now}}.


loop_send(_,_,_,_,_,LastSeq,-1,Acc,PCB) -> {LastSeq,Acc,PCB};
loop_send(_,_,_,_,Limit,LastSeq,-1,Acc,PCB) when Limit == 0 -> {LastSeq,Acc,PCB};
loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,Limit,
          LastSeq,Iter,Acc,
          #aiutp_pcb{time = {Now,_},
                     ack_nr = AckNR,cur_window = CurWindow,
                     outbuf = OutBuf,replay_micro = ReplyMicro} = PCB)  ->

  WrapPacket = aiutp_buffer:data(Iter,OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,

  if (?WRAPPING_DIFF_16(MaxSeq, Packet#aiutp_packet.seq_nr) > 0) and
     (?WRAPPING_DIFF_16(Packet#aiutp_packet.seq_nr, MinSeq) >= 0) ->
      Next = aiutp_buffer:next(Iter, OutBuf),
        {SendBytes,WrapPacket0,Content1} = update_wrap_packet(MicroNow,ReplyMicro,
                                                        WindowSize,AckNR,WrapPacket),
      OutBuf0 = aiutp_buffer:replace(Iter,WrapPacket0,OutBuf),
      loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,Limit -1,Packet#aiutp_packet.seq_nr,
                Next,[Content1|Acc],PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,outbuf = OutBuf0,last_sent_packet = Now});
     true -> {LastSeq,Acc,PCB}
  end.

send_packet_in_range(Acc,MinSeq,MaxSeq,Limit,
                     #aiutp_pcb{max_window = MaxWindow, outbuf = OutBuf,inbuf = InBuf }  = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WindowSize = window_size(MaxWindow, InBuf),
  Iter = aiutp_buffer:head(OutBuf),
  if Limit > 0 -> loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,Limit,MinSeq,Iter,Acc,PCB);
     true -> loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,-1,MinSeq,Iter,Acc,PCB)
  end.


flush_packets(Acc,#aiutp_pcb{outbuf = OutBuf} = PCB)->
  Iter = aiutp_buffer:head(OutBuf),
  flush_packets(Acc,Iter,PCB).
flush_packets(Acc,-1, PCB)-> {Acc,PCB};
flush_packets(Acc,Iter, #aiutp_pcb{outbuf = OutBuf} = PCB)->
  {ISFull,PCB0} = is_full(0,PCB),
  if ISFull == true -> {Acc,PCB0};
     true ->
      {Acc0,PCB1} = aiutp_net:send_packet(Acc, Iter, PCB0),
      Next = aiutp_buffer:next(Iter, OutBuf),
      flush_packets(Acc0, Next,PCB1)
  end.
