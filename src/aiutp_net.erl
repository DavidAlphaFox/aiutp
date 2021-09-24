-module(aiutp_net).
-include("aiutp.hrl").
-export([window_size/1,
         send_ack/2]).

window_size(InBuf) -> (255 - aiutp_buff:size(InBuf)) * ?PACKET_SIZE.

send_ack(Acc,#aiutp_pcb{conn_id_send = ConnIdSend,
                        seq_nr = SeqNR, ack_nr = AckNR,
                        reorder_count = ReorderCount,
                        got_fin_reached = GotFinReached} = PCB)->
  Packet = aiutp_packet:ack(SeqNR, AckNR),
  Packet0 =
    if (ReorderCount /= 0) and (not GotFinReached) ->
        Sack = build_sack(PCB),
        Packet#aiutp_packet{conn_id = ConnIdSend, wnd = window_size(InBuf),
                            extension = [{sack,Sack}]};
       true ->
        Packet#aiutp_packet{ conn_id = ConnIdSend, wnd = window_size(InBuf)}
    end,
  [Packet0|Acc].

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
