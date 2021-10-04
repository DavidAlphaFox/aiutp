-module(aiutp_rx).
-include("aiutp.hrl").
-export([in/2]).

in(#aiutp_packet{seq_nr = PktSeqNR} = Packet,
   #aiutp_pcb{ack_nr = AckNR} = PCB)->
  NextAckNR  = aiutp_util:bit16(AckNR + 1),
  PCB1 =
    if PktSeqNR == NextAckNR  -> recv(Packet,PCB#aiutp_pcb{ack_nr = PktSeqNR});
       true->  recv_reorder(aiutp_util:bit16(PktSeqNR - NextAckNR),Packet,PCB)
    end,
  PCB1#aiutp_pcb{ida = true}.

recv(Packet,#aiutp_pcb{time=Now,inque = InQ,
                       got_fin_reached = GotFinReached,
                       got_fin = GotFin,
                       eof_pkt = EOFPkt,
                       ack_nr = AckNR,
                       rto  = RTO} = PCB)->

  InQ0 = aiutp_queue:push_back(Packet#aiutp_packet.payload, InQ),
  PCB1 =
    if (GotFinReached == false) and
       (GotFin == true) and
       (EOFPkt == AckNR) ->
        PCB0 = PCB#aiutp_pcb{inque = InQ0,
                             got_fin_reached = true,
                             rto_timeout = Now + erlang:min((RTO * 3),60),
                             reorder_count = 0,
                             inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE)},
        aiutp_net:send_ack(PCB0);
       true -> PCB#aiutp_pcb{inque = InQ0}
    end,
  if PCB1#aiutp_pcb.reorder_count == 0 -> PCB1;
     true ->
      #aiutp_pcb{inbuf = InBuf,reorder_count = ReorderCount} = PCB1,
      Iter = aiutp_buffer:head(InBuf),
      Packet0 = aiutp_buffer:data(Iter, InBuf),
      NextAckNR = aiutp_util:bit16(AckNR + 1),
      if Packet0#aiutp_packet.seq_nr == NextAckNR ->
          recv(Packet0,PCB1#aiutp_pcb{
                         inbuf = aiutp_buffer:pop(InBuf),
                         reorder_count = ReorderCount -1 ,
                         ack_nr  = NextAckNR});
         true -> PCB1
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
  PCB#aiutp_pcb{inbuf = aiutp_buffer:append(Packet, InBuf),
                reorder_count = ReorderCount + 1};
recv_reorder(Packet,Iter,Prev,
             #aiutp_pcb{inbuf = InBuf,reorder_count = ReorderCount} = PCB)->
  Packet0 = aiutp_buffer:data(Iter,InBuf),
  if Packet0#aiutp_packet.seq_nr == Packet#aiutp_packet.seq_nr -> PCB;
     ?WRAPPING_DIFF_16(Packet0#aiutp_packet.seq_nr, Packet#aiutp_packet.seq_nr) > 0 ->
      PCB#aiutp_pcb{inbuf = aiutp_buffer:insert(Prev,Packet, InBuf),reorder_count = ReorderCount + 1};
     true->
      Next = aiutp_buffer:next(Iter, InBuf),
      recv_reorder(Packet,Next,Iter,PCB)
  end.
