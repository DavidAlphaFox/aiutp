-module(aiutp_rx).
-include("aiutp.hrl").
-export([in/3]).

in(Acc,#aiutp_packet{seq_nr = PktSeqNR} = Packet,
   #aiutp_pcb{ack_nr = AckNR} = PCB)->
  DiffSeq  = aiutp_util:bit16(PktSeqNR - AckNR - 1),
  {Acc0,PCB1} =
    if DiffSeq == 0 -> recv(Acc,Packet,PCB#aiutp_pcb{ack_nr = aiutp_util:bit16(AckNR + 1)});
       true->
        PCB0 = recv_reorder(DiffSeq,Packet,PCB),
        {Acc,PCB0}
    end,
  {Acc0,PCB1#aiutp_pcb{ida = true}}.

recv(Acc,Packet,
     #aiutp_pcb{time={Now,_},
                inque = InQ,
                got_fin_reached = GotFinReached,
                got_fin = GotFin,
                eof_pkt = EOFPkt,
                ack_nr = AckNR,
                rto  = RTO
               } = PCB)->
  InQ0 = aiutp_queue:push_back(Packet#aiutp_packet.payload, InQ),
  {Acc0,PCB1} =
    if (GotFinReached == false) and
       (GotFIn == true) and
       (EOFPkt == AckNR) ->
        PCB0 = PCB#aiutp_pcb {inque = InQ0,
                              got_fin_reached = true,
                              rto_timeout = Now + ?MIN((RTO * 3),60),
                              reorder_count = 0,
                              inbuf = aiutp_buffer:new(?REORDER_BUFFER_MAX_SIZE + 1)
                             },
        {aiutp_net:send_ack(Acc, PCB0),PCB0};
       true -> {Acc,PCB#aiutp_pcb{inque = InQ0}}
    end,
  if PCB1#aiutp_pcb.reorder_count == 0 -> {Acc0,PCB1};
     true ->
      #aiutp_pcb{inbuf = InBuf,
                 reorder_count = ReorderCount} = PCB1,
      Iter = aiutp_buffer:head(InBuf),
      Packet0 = aiutp_buffer:data(Iter, InBuf),
      NextAckNR = aiutp_util:bit16(AckNR + 1),
      if Packet0#aiutp_packet.seq_nr == NextAckNR ->
          recv(Acc,Packet0,PCB1#aiutp_pcb{
                             inbuf = aiutp_buffer:pop(Iter, InBuf),
                             reorder_count = ReorderCount -1 ,
                             ack_nr  = NextAckNR
                            });
         true -> {Acc0,PCB1}
      end
  end.

recv_reorder(DiffSeq,#aiutp_packet{seq_nr = PktSeqNR} = Packet,
             #aiutp_pcb{got_fin = GotFin,eof_pkt = EOFPkt,inbuf = InBuf} = PCB)->
  if (GotFin == true) and
     (?WRAPPING_DIFF_16(EOFPkt, PktSeqNR) < 0) -> PCB;
     DiffSeq > 16#3FFF -> PCB;
     true ->
      Iter = aiutp_buffer:head(InBuf),
      recv_reorder(Packet,Iter,-1,PCB)
  end.

recv_reorder(Packet,-1,_,
             #aiutp_pcb{inbuf = InBuf,reorder_count = ReorderCount} = PCB)->
  PCB#aiutp_pcb{
    inbuf = aiutp_buffer:append(Packet, InBuf),
    reorder_count = ReorderCount + 1
   };
recv_reorder(Packet,Iter,Prev,
             #aiutp_pcb{inbuf = InBuf,reorder_count = ReorderCount} = PCB)->

  Packet0 = aiutp_buffer:data(Iter,InBuf),
  if Packet0#aiutp_packet.seq_nr == Packet#aiutp_packet.seq_nr -> PCB;
     ?WRAPPING_DIFF_16(Packet0#aiutp_packet.seq_nr, Packet#aiutp_packet.seq_nr) > 0 ->
      PCB#aiutp_pcb{
        inbuf = aiutp_buffer:insert(Prev,Packet, InBuf),
        reorder_count = ReorderCount + 1
       };
     true->
      Next = aiutp_buffer:next(Iter, InBuf),
      recv_reoder(Packet,Next,Iter,PCB)
  end.
