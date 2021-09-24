-module(aiutp_net).
-include("aiutp.hrl").
-export([window_size/2,
         send_ack/2,
         send_packet/3]).

window_size(MaxWindow,InBuf) ->
  ?MIN(((255 - aiutp_buff:size(InBuf)) * ?PACKET_SIZE),MaxWindow).


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

send_ack(Acc,#aiutp_pcb{time = {Now,_}
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
  {[aiutp_packet:encode(Packet0)|Acc],PCB#aiutp_pcb{last_sent_packet = Now}}
send_packet(Acc,-1,PCB)->{Acc,PCB};
send_packet(Acc,Pos,#aiutp_pcb{time = {Now,_},
                               ack_nr = AckNR,cur_window = CurWindow,
                               inbuf = InBuf,max_window = MaxWindow,
                               outbuf = OutBuf,replay_micro = ReplyMicro} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  WrapPacket = aiutp_buffer:data(Pos,OutBuf),
  WindowSize = window_size(MaxWindow, InBuf),
  #aiutp_packet_wrap{
     transmissions = Transmission,
     content = Content,
     packet = Packet
    } = WrapPacket,
  {CurWindow0,WrapPacket0,Content1} =
    if Transmission == 0 ->

        Packet0 = Packet#aiutp_packet{tv_usec = MicroNow,reply_micro = ReplyMicro,
                                      ack_nr = AckNR,wnd = WindowSize},
        Content0 = aiutp_packet:encode(Packet0),
        Payload = erlang:byte_size(Content),
        {CurWindow + Payload,
         WraPacket#aiutp_packet_wrap{transmissions = Transmission + 1,need_resend = false,
                                     payload = Payload,time_sent = MicroNow,
                                     packet = Packet0,content = Content0},
         Content};
       true ->
        Packet0 = Packet#aiutp_packet{tv_usec = MicroNow,reply_micro = ReplyMicro,
                                      ack_nr = AckNR, wnd = WindowSize},
        Content0 = aiutp_packet:encode(Packet0),
        {CurWindow,
         WraPacket#aiutp_packet_wrap{transmissions = Transmission + 1,need_resend = false,
                                     time_sent = MicroNow,packet = Packet0,content = Content0},
         Content0}
    end,
  OutBuf0 = aiutp_buffer:replace(Pos,WrapPacket0,OutBuf),
  {[Content1|Acc],PCB#aiutp_pcb{cur_window = CurWindow0,outbuf = OutBuf0,last_sent_packet = Now}}.
