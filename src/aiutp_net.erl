-module(aiutp_net).
-include("aiutp.hrl").
-export([window_size/2,
         is_full/2,
         flush_packets/1,
         flush_queue/1,
         send_ack/1,
         send_fin/1,
         send_keep_alive/1,
         send_packet/2,
         send_packet_in_range/4]).

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
send_fin(#aiutp_pcb{outque = OutQue} = PCB)->
  OutQue0 = aiutp_queue:push_back({?ST_FIN}, OutQue),
  flush_queue(PCB#aiutp_pcb{outque = OutQue0}).
send_ack(#aiutp_pcb{time = {Now,_},
                    socket = Acc,
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
  PCB#aiutp_pcb{last_sent_packet = Now,last_rcv_win = WindowSize,
                socket = [aiutp_packet:encode(Packet0)|Acc]}.

send_keep_alive(#aiutp_pcb{ack_nr = AckNR} = PCB)-> send_ack(PCB#aiutp_pcb{ack_nr = AckNR -1}).

update_wrap_packet(MicroNow,ReplyMicro,WindowSize,AckNR,WrapPacket)->
  #aiutp_packet_wrap{
     transmissions = Transmission,
     packet = Packet,
     need_resend = NeedResend,
     payload = Payload
    } = WrapPacket,
  Packet0 = Packet#aiutp_packet{tv_usec = MicroNow,reply_micro = ReplyMicro,
                                ack_nr = AckNR,wnd = WindowSize},
  Content0 = aiutp_packet:encode(Packet0),
  Payload0 =
    if (NeedResend == true) or
       (Transmission == 0) -> Payload;
       true -> 0
    end,
  {Payload0,
   WraPacket#aiutp_packet_wrap{transmissions = Transmission + 1,need_resend = false,
                               time_sent = MicroNow,packet = Packet0,content = Content0},
   Content0}.


send_packet(-1,PCB)->PCB;
send_packet(Pos,#aiutp_pcb{time = {Now,_},
                           socket = Acc,
                           ack_nr = AckNR,cur_window = CurWindow,
                           inbuf = InBuf,max_window = MaxWindow,
                           outbuf = OutBuf,replay_micro = ReplyMicro} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WrapPacket = aiutp_buffer:data(Pos,OutBuf),
  WindowSize = window_size(MaxWindow, InBuf),
  {SendBytes,WrapPacket0,Content1} = update_wrap_packet(MicroNow,ReplyMicro,
                                                        WindowSize,AckNR,WrapPacket),
  OutBuf0 = aiutp_buffer:replace(Pos,WrapPacket0,OutBuf),
  PCB#aiutp_pcb{socket = [Content1|Acc],cur_window = CurWindow + SendBytes,
                outbuf = OutBuf0,last_sent_packet = Now}.


loop_send(_,_,_,_,_,LastSeq,-1,PCB) -> {LastSeq,PCB};
loop_send(_,_,_,_,Limit,LastSeq,-1,PCB) when Limit == 0 -> {LastSeq,PCB};
loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,Limit,
          LastSeq,Iter,#aiutp_pcb{time = {Now,_},socket = Acc,
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
                Next,PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,outbuf = OutBuf0,
                                   socket = [Content1|Acc],last_sent_packet = Now});
     true -> {LastSeq,PCB}
  end.

send_packet_in_range(MinSeq,MaxSeq,Limit,
                     #aiutp_pcb{max_window = MaxWindow,outbuf = OutBuf,inbuf = InBuf }  = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WindowSize = window_size(MaxWindow, InBuf),
  Iter = aiutp_buffer:head(OutBuf),
  if Limit > 0 -> loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,Limit,MinSeq,Iter,PCB);
     true -> loop_send(MicroNow,WindowSize,MinSeq,MaxSeq,-1,MinSeq,Iter,PCB)
  end.


flush_packets(#aiutp_pcb{outbuf = OutBuf} = PCB)->
  Iter = aiutp_buffer:head(OutBuf),
  flush_packets(Iter,PCB).
flush_packets(-1, PCB)-> PCB;
flush_packets(Iter, #aiutp_pcb{outbuf = OutBuf} = PCB)->
  {ISFull,PCB0} = is_full(0,PCB),
  if ISFull == true -> PCB0;
     true ->
      Next = aiutp_buffer:next(Iter, OutBuf),
      WrapPacket = aiutp_buffer:data(Iter,OutBuf),
      if (WrapPacket#aiutp_packet_wrap.transmissions > 0 ) and
         (WrapPacket#aiutp_packet_wrap.need_resend == false) ->
          flush_packets(Next,PCB0);
         true ->
          PCB1 = aiutp_net:send_packet(Iter, PCB0),
          flush_packets(Next,PCB1)
      end
  end.

loop_send_packet(#aiutp_pcb{outqueue = OutQue,outbuf = OutBuf,
                            conn_id_send = ConnID,ack_nr = AckNR,seq_nr = SeqNR,
                            cur_window_packets = CurWindowPackets,
                            max_window = MaxWindow,inbuf = InBuf,
                            socket = Acc} = PCB)->
  if aiutp_queue:size(OutQue) == 0 -> PCB;
     true ->
      {Type,Data} = aiutp_queue:front(OutQue),
      Payload = erlang:byte_size(Data),
      {ISFull,PCB0} = is_full(Payload, PCB),
      if ISFull == true -> PCB0;
         true ->
          Packet =  aiutp_packet:data(SeqNR, AckNR),
          LastRcvWin = window_size(MaxWindow,InBuf),
          Packet0 = Packet#aiutp_packet{conn_id = ConnID,
                                        type = Type,
                                        payload = Data,wnd = LastRcvWin},
          OutQue0 = aiutp_queue:pop_front(OutQue),
          OutBuf0 = aiutp_buffer:append(
                      #aiutp_packet_wrap{payload = Payload,packet = Packet0},OutBuf),
          Iter = aiutp_buffer:tail(OutBuf0),
          PCB1 = send_packet(Iter,PCB0#aiutp_pcb{cur_window_packets = CurWindowPackets + 1,
                                                 outqueue = OutQue0,outbuf = OutBuf0,
                                                 seq_nr = SeqNR + 1,last_rcv_win = LastRcvWin}),
          loop_send_packet(PCB1)
      end
  end.

flush_queue(#aiutp_pcb{time = {Now,_},cur_window_packets = CurWindowPackets,rto = RTO} = PCB)->
  PCB0 =
    if CurWindowPackets == 0 ->
        PCB#aiutp_pcb{ retransmit_timeout = RTO,
                       rto_timeout = Now + RTO};
       true -> PCB
    end,
  loop_send_packet(PCB0).
