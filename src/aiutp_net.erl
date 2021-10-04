-module(aiutp_net).
-include("aiutp.hrl").
-export([window_size/2,
         is_full/2,
         flush_packets/1,
         flush_queue/1,
         schedule_ack/1,
         send_ack/1,
         send_fin/1,
         send_keep_alive/1,
         send_packet/2,
         send_n_packets/4]).

window_size(_MaxWindow,InBuf) -> aiutp_buffer:unused(InBuf) * ?PACKET_SIZE.

max_send(#aiutp_pcb{max_window = MaxWindow,
                    max_window_user = MaxWindowUser,
                    cur_window = CurWindow,
                    cur_window_packets = CurWindowPackets,
                    brust = Brust})->
  if Brust == true ->
      if CurWindowPackets > ?BRUST_OUTGOING_BUFFER_SIZE -> 0;
         true -> ?PACKET_SIZE * (?BRUST_OUTGOING_BUFFER_SIZE - CurWindowPackets)
      end;
     true ->
      MaxSend = erlang:min(MaxWindow, MaxWindowUser),
      if CurWindowPackets >= (?REORDER_BUFFER_MAX_SIZE - 1) -> 0;
         MaxSend > CurWindow ->
          Remain = MaxSend - CurWindow,
          if Remain < ?PACKET_SIZE -> ?PACKET_SIZE;
             true -> Remain
          end;
         true -> 0
      end
  end.

is_full(Bytes,#aiutp_pcb{time= Now,
                         max_window = MaxWindow,
                         max_window_user = MaxWindowUser,
                         cur_window = CurWindow,
                         cur_window_packets = CurWindowPackets,
                         brust = Brust} = PCB)->
  Bytes0 = if Bytes > ?PACKET_SIZE -> ?PACKET_SIZE;
              Bytes < 0 -> ?PACKET_SIZE;
              true -> Bytes
           end,
  if Brust == true ->
      if CurWindowPackets > ?BRUST_OUTGOING_BUFFER_SIZE ->
          {true,PCB#aiutp_pcb{last_maxed_out_window = Now}};
         true -> {false,PCB}
      end;
     true ->
      MaxSend = erlang:min(MaxWindow, MaxWindowUser),
      if CurWindowPackets >= (?OUTGOING_BUFFER_MAX_SIZE - 1) ->
          {true,PCB#aiutp_pcb{last_maxed_out_window = Now}};
         (CurWindow + Bytes0) > MaxSend ->
          {true,PCB#aiutp_pcb{last_maxed_out_window = Now}};
         true -> {false,PCB}
      end
  end.


build_sack(Size,Acc,_,Iter,_)
  when Size == 0 ;
       Iter == -1 ->
  lists:foldl(
    fun(BI,BAcc)->
        Bits = maps:get(BI,Acc),
        <<BAcc/binary,Bits/big-unsigned-integer>>
    end, <<>>, lists:seq(0, 3));


build_sack(Size,Acc,Base,Iter,InBuf) ->
  Packet = aiutp_buffer:data(Iter,InBuf),
  Index = aiutp_util:bit16(Packet#aiutp_packet.seq_nr - Base),
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
  Size0 = erlang:min(30,Size),
  if Size0 == 0 -> undefined;
     true ->
      Acc = lists:foldl(fun(Idx,Map)-> maps:put(Idx,0,Map) end,#{},lists:seq(0,3)),
      Head = aiutp_buffer:head(InBuf),
      build_sack(Size0,Acc,AckNR + 2,Head,InBuf)
  end.
send_fin(#aiutp_pcb{outque = OutQue} = PCB)->
  OutQue0 = aiutp_queue:push_back({?ST_FIN,<<>>}, OutQue),
  flush_queue(PCB#aiutp_pcb{outque = OutQue0}).
send_ack(#aiutp_pcb{time = Now,
                    socket = Socket,
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
    if (ReorderCount /= 0) and
       (GotFinReached == false) ->
        Sack = build_sack(PCB),
        Packet#aiutp_packet{conn_id = ConnIdSend, wnd = WindowSize,
                            tv_usec = MicroNow,reply_micro = ReplyMicro,
                            extension = [{sack,Sack}]};
       true ->
        Packet#aiutp_packet{ conn_id = ConnIdSend,
                             tv_usec = MicroNow,reply_micro = ReplyMicro,
                             wnd = WindowSize}
    end,
  do_send(Socket,aiutp_packet:encode(Packet0)),
  PCB#aiutp_pcb{last_sent_packet = Now,last_rcv_win = WindowSize}.


send_keep_alive(#aiutp_pcb{ack_nr = AckNR} = PCB)->
  PCB0 = send_ack(PCB#aiutp_pcb{ack_nr = aiutp_util:bit16(AckNR -1)}),
  PCB0#aiutp_pcb{ack_nr = AckNR}.

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
   WrapPacket#aiutp_packet_wrap{transmissions = Transmission + 1,need_resend = false,
                               time_sent = MicroNow,packet = Packet0,content = Content0},
   Content0}.


send_packet(-1,PCB)->PCB;
send_packet(Pos,#aiutp_pcb{time = Now,
                           socket = Socket,
                           ack_nr = AckNR,cur_window = CurWindow,
                           inbuf = InBuf,max_window = MaxWindow,
                           outbuf = OutBuf,reply_micro = ReplyMicro} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WrapPacket = aiutp_buffer:data(Pos,OutBuf),
  WindowSize = window_size(MaxWindow, InBuf),
  {SendBytes,WrapPacket0,Content1} = update_wrap_packet(MicroNow,ReplyMicro,
                                                        WindowSize,AckNR,WrapPacket),
  OutBuf0 = aiutp_buffer:replace(Pos,WrapPacket0,OutBuf),
  do_send(Socket,Content1),
  PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,
                outbuf = OutBuf0,last_sent_packet = Now}.



send_n_packets(MinSeq,_,Limit,Iter,PCB)
  when Limit == 0;
       Iter == -1 -> {Limit,MinSeq,PCB};
send_n_packets(MinSeq,MaxSeq,Limit,
               Iter,#aiutp_pcb{time = Now,socket = Socket,
                               max_window = MaxWindow,outbuf = OutBuf,
                               cur_window = CurWindow,inbuf = InBuf,
                               reply_micro = ReplyMicro,ack_nr = AckNR}  = PCB)->
  WrapPacket = aiutp_buffer:data(Iter,OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  if (?WRAPPING_DIFF_16(MaxSeq, Packet#aiutp_packet.seq_nr) > 0) and
     (?WRAPPING_DIFF_16(Packet#aiutp_packet.seq_nr, MinSeq) >= 0) ->
      MicroNow = aiutp_util:microsecond(),
      WindowSize = window_size(MaxWindow, InBuf),
      Next = aiutp_buffer:next(Iter, OutBuf),
      {SendBytes,WrapPacket0,Content1} = update_wrap_packet(MicroNow,ReplyMicro,
                                                            WindowSize,AckNR,WrapPacket),
      OutBuf0 = aiutp_buffer:replace(Iter,WrapPacket0,OutBuf),
      do_send(Socket,Content1),
      send_n_packets(Packet#aiutp_packet.seq_nr,MaxSeq,Limit -1,Next,
                     PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,outbuf = OutBuf0,
                                   last_sent_packet = Now});
     ?WRAPPING_DIFF_16(MinSeq, Packet#aiutp_packet.seq_nr) > 0 ->
      Next = aiutp_buffer:next(Iter, OutBuf),
      send_n_packets(MinSeq,MaxSeq,Limit,Next,PCB);
     true -> {Limit,MinSeq,PCB}
  end.


send_n_packets(MinSeq,MaxSeq,Limit,#aiutp_pcb{outbuf = OutBuf}  = PCB)->
  Iter = aiutp_buffer:head(OutBuf),
  {Remain,LastSeq,PCB0} = send_n_packets(MinSeq,MaxSeq,Limit,Iter,PCB),
  {Limit - Remain,LastSeq,PCB0}.


flush_packets(#aiutp_pcb{outbuf = OutBuf} = PCB)->
  Iter = aiutp_buffer:head(OutBuf),
  flush_packets(Iter,PCB).
flush_packets(-1, PCB)-> PCB;
flush_packets(Iter, #aiutp_pcb{outbuf = OutBuf} = PCB)->
  {ISFull,PCB0} = is_full(-1,PCB),
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



send_new_packet(Type,Data,Payload,
                #aiutp_pcb{outbuf = OutBuf,socket = Socket,cur_window = CurWindow,
                           conn_id_send = ConnId,ack_nr = AckNR,seq_nr = SeqNR,
                           cur_window_packets = CurWindowPackets,
                           max_window = MaxWindow,inbuf = InBuf,
                           reply_micro = ReplyMicro, time = Now} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  Packet =  aiutp_packet:data(SeqNR, AckNR),
  LastRcvWin = window_size(MaxWindow,InBuf),
  Packet0 = Packet#aiutp_packet{conn_id = ConnId,
                                type = Type,
                                payload = Data,wnd = LastRcvWin},
  WindowSize = window_size(MaxWindow, InBuf),
  WrapPacket = #aiutp_packet_wrap{payload = Payload,packet = Packet0},
  {SendBytes,WrapPacket0,Content1} = update_wrap_packet(MicroNow,ReplyMicro,
                                                        WindowSize,AckNR,WrapPacket),
  OutBuf0 = aiutp_buffer:append(WrapPacket0,OutBuf),
  do_send(Socket,Content1),
  PCB#aiutp_pcb{cur_window_packets = CurWindowPackets + 1,
                cur_window = CurWindow + SendBytes,outbuf = OutBuf0,seq_nr = aiutp_util:bit16(SeqNR + 1),
                last_rcv_win = LastRcvWin,last_sent_packet = Now}.


fill_with_next(RequiredSize,Type,#aiutp_pcb{outque = OutQue} = PCB)->
  case aiutp_queue:empty(OutQue) of
    true -> done;
    false ->  fill_with_next(RequiredSize,Type,<<>>, PCB)
  end.
fill_with_next(RequiredSize,Type,Acc,#aiutp_pcb{outque = OutQue} = PCB)->
  case {aiutp_queue:empty(OutQue),erlang:byte_size(Acc)} of
    {true,0} -> done;
    {true,_} -> {done,Acc,PCB};
    _ ->
      case aiutp_queue:front(OutQue) of
        {Type,Bin} ->
          BinSize = erlang:byte_size(Bin),
          if BinSize > RequiredSize ->
              <<More:RequiredSize/binary,Rest/binary>> = Bin,
              {<<Acc/binary,More/binary>>,Rest,PCB#aiutp_pcb{outque = aiutp_queue:pop_front(OutQue)}};
             true ->
              fill_with_next(RequiredSize - BinSize, Type,<<Acc/binary,Bin/binary>>,
                             PCB#aiutp_pcb{outque = aiutp_queue:pop_front(OutQue)})
          end;
        _ ->
          if erlang:byte_size(Acc) > 0 -> {next,Acc,PCB};
             true -> {next,PCB}
          end
      end
  end.

send_data_in_queue(_,<<>>,_,PCB) -> send_data_in_queue(PCB);
send_data_in_queue(Type,Bin,Size,#aiutp_pcb{outque = OutQue} = PCB)
  when Size =< 0 ->
  if erlang:byte_size(Bin) > 0 ->
      PCB#aiutp_pcb{outque = aiutp_queue:push_front({Type,Bin}, OutQue)};
     true ->PCB
  end;
send_data_in_queue(Type,Bin,Size,PCB)->
  BinSize = erlang:size(Bin),
  Size0 =
    if Size > ?PACKET_SIZE -> ?PACKET_SIZE;
       true -> Size
    end,
  if BinSize =< Size0 ->
      case fill_with_next(Size0 - BinSize, Type,PCB) of
        done -> send_new_packet(Type, Bin, BinSize, PCB);
        {done,MoreData,PCB1} ->
          Data = <<Bin/binary,MoreData/binary>>,
          send_new_packet(Type, Data, erlang:byte_size(Data), PCB1);
        {next,PCB1}->
          PCB2 = send_new_packet(Type, Bin, BinSize, PCB1),
          send_data_in_queue(PCB2);
        {next,MoreData,PCB1} ->
          Data = <<Bin/binary,MoreData/binary>>,
          PCB2 = send_new_packet(Type, Data, erlang:byte_size(Data), PCB1),
          send_data_in_queue(PCB2);
        {MoreData,NewBin,PCB1} ->
          Data = <<Bin/binary,MoreData/binary>>,
          PCB2 = send_new_packet(Type, Data, Size0, PCB1),
          send_data_in_queue(Type,NewBin,Size - Size0, PCB2)
        end;
     true ->
      %io:format("send ~p bytes from queue~n",[Size0]),
      <<Data:Size0/binary,Rest/binary>> = Bin,
      PCB0 = send_new_packet(Type, Data, Size0, PCB),
      send_data_in_queue(Type,Rest,Size - Size0, PCB0)
  end.
send_data_in_queue(#aiutp_pcb{outque = OutQue} = PCB)->
  case aiutp_queue:empty(OutQue) of
    true -> PCB;
    _->
      {Type,Bin}  = aiutp_queue:front(OutQue),
      OutQue0 = aiutp_queue:pop_front(OutQue),
      MaxSend = max_send(PCB),
      io:format("max can send ~p~n",[MaxSend]),
      send_data_in_queue(Type,Bin,MaxSend,PCB#aiutp_pcb{outque = OutQue0})
  end.
flush_queue(#aiutp_pcb{time = Now,outque = OutQue,
                       cur_window_packets = CurWindowPackets,
                       rto = RTO} = PCB)->
  case aiutp_queue:empty(OutQue) of
    true -> PCB;
    false ->
      PCB0 =
        if CurWindowPackets == 0 ->
            PCB#aiutp_pcb{ retransmit_timeout = RTO,
                           rto_timeout = Now + RTO};
           true -> PCB
        end,
      {ISFull,PCB1} = is_full(-1,PCB0),
      if ISFull == true -> PCB1;
         true -> send_data_in_queue(PCB1)
      end
  end.

schedule_ack(#aiutp_pcb{ida = false} = PCB) -> PCB;
schedule_ack(PCB) -> send_ack(PCB#aiutp_pcb{ida = false}).

do_send(Socket,Remote,Count,Content)->
  case gen_udp:send(Socket,Remote,Content) of
    ok -> ok ;
    Error ->
      if Count == 0 -> error(Error);
         true ->
          timer:sleep(150),
          do_send(Socket,Remote,Count -1,Content)
      end
  end.
do_send({Socket,Remote},Content) ->
  do_send(Socket,Remote,3,Content).
