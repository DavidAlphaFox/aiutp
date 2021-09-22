-module(aiutp_pcb).
-include("aiutp.hrl").
-export([new/2,process/2]).

new(ConnIdRecv,ConnIdSend)->
  CurMilli = aiutp_util:millisecond(),
  #aiutp_pcb{
     state = ?CS_IDLE,
     conn_id_send = ConnIdSend,
     conn_id_recv = ConnIdRecv,
     last_got_packet = CurMilli,
     last_sent_packet = CurMilli,
     last_measured_delay = CurMilli + 16#70000000,
     average_sample_time = CurMilli + 5000,
     last_rwin_decay = CurMilli - ?MAX_WINDOW_DECAY,
     our_hist = aiutp_delay:new(CurMilli),
     their_hist = aiutp_delay:new(CurMilli),
     rtt_hist = aiutp_delay:new(CurMilli),
     max_window = ?AIUTP_MTU_DEF - ?UTP_HEADER_SIZE,
     inbuf = aiutp_buffer:new(256),
     outbuf = aiutp_buffer:new(256)
    }.
window_size(InBuf) -> (255 - aiutp_buff:size(InBuf)) * ?PACKET_SIZE.

build_sack(0,Acc,_,_,_)->
  lists:foldl(fun(BI,BAcc)->
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
  
build_sack(#aiutp_packet{ack_nr = AckNR,inbuf = InBuf})->
  Size = aiutp_buffer:size(InBuf),
  Size0 = ?MIN(30,Size),
  if Size0 == 0 -> undefined;
     true ->
      Acc = list:foldl(fun(Idx,Map)-> maps:put(Idx,0,Map) end,#{},lists:seq(0,3)),
      Head = aiutp_buffer:head(InBuf),
      build_sack(Size0,Acc,AckNR+1,Head,InBuf)
  end.

send_ack(Acc,#aiutp_pcb{conn_id_send = ConnIdSend,
                        seq_nr = SeqNR, ack_nr = AckNR,
                        reorder_count = ReorderCount,got_fin_reached = GotFinReached} = PCB)->
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


process(Packet,PCB)-> process([],PCB#aiutp_pcb.state,Packet,PCB).

process(Acc,_,#aiutp_packet{type = st_reset,conn_id = ConnId},
        #aiutp_pcb{conn_id_send = ConnIdSend,conn_id_recv = ConnIdRecv,
                   close_requested = CloseRequested} = PCB)->
  if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
      if CloseRequested == true -> {Acc,PCB#aiutp_pcb{state = ?CS_DESTROY}};
         true -> {Acc,PCB#aiutp_pcb{state = ?CS_RESET}}
      end;
     true-> {Acc,PCB}
  end;
process(Acc,?CS_IDLE,#aiutp_packet{type = st_syn,seq_nr = AckNR}, PCB) ->
  PCB0 = PCB#aiutp_pcb{
           state = ?CS_SYN_RECV,
           ack_nr = AckNR,
           seq_nr = aiutp_util:bit16_random(),
          },
  {send_ack(Acc,PCB0),PCB};
process() ->
