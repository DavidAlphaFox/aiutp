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
     inbuf = aiutp_buffer:new(?REORDER_BUFFER_MAX_SIZE + 1),
     outbuf = aiutp_buffer:new(?REORDER_BUFFER_MAX_SIZE + 1)
    }.


curr_window(#aiutp_pcb{cur_window_packets = CurWindowPackets})->
  ?MAX(CurWindowPackets + ?ACK_NR_ALLOWED_WINDOW,?ACK_NR_ALLOWED_WINDOW).
% window packets size is used to calculate a minimum
% permissible range for received acks. connections with acks falling
% out of this range are dropped

acks(PktAckNR,#aiutp_pcb{seq_nr = SeqNR,
                         cur_window_packets = CurWindowPackets})->
  Acks = aiutp_util:bit16(PkgAckNR - (SeqNR - 1 - CurWindowPackets)),
  if Acks > CurWindowPackets -> 0; % this happens when we receive an old ack nr
     true -> Acks
  end.
% Process acknowledgment acks is the number of packets that was acked

diff_seq_nr(PktSeqNR,#aiutp_pcb{ack_nr = AckNR})->
  aiutp_util:bit16(PktSeqNR - AckNR -1).
% seqnr is the number of packets past the expected
% packet this is. ack_nr is the last acked, seq_nr is the
% current. Subtracring 1 makes 0 mean "this is the next
% expected packet".

process(Packet,PCB)-> process([],Packet#aiutp_packet.type,Packet,PCB).

process(Acc,_,Packet,#aiutp_pcb{state = State} = PCB)
  when (State == ?CS_DESTROY);
       (State == ?CS_RESET) ->
  {Acc,PCB};
process(Acc,?ST_RESET,
        #aiutp_packet{conn_id = ConnId},
        #aiutp_pcb{conn_id_send = ConnIdSend,
                   conn_id_recv = ConnIdRecv,
                   close_requested = CloseRequested} = PCB)->
  if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
      if CloseRequested == true -> {Acc,PCB#aiutp_pcb{state = ?CS_DESTROY}};
         true -> {Acc,PCB#aiutp_pcb{state = ?CS_RESET}}
      end;
     true-> {Acc,PCB}
  end;
%% 处理SYN
process(Acc,?ST_SYN,
        #aiutp_packet{seq_nr = AckNR},
        #aiutp_pcb{state = ?CS_IDLE} = PCB) ->
  PCB0 = PCB#aiutp_pcb{state = ?CS_SYN_RECV,
                       ack_nr = AckNR,
                       seq_nr = aiutp_util:bit16_random(),
                       last_got_packet = aiutp_util:millisecond()},
  {aiutp_net:send_ack(Acc,PCB0),PCB0};
process(Acc,?ST_SYN,
        #aiutp_packet{seq_nr = AckNR},
        #aiutp_pcb{state = ?CS_SYN_RECV,ack_nr = AckNR} = PCB) ->
  {aiutp_net:send_ack(Acc, PCB),
   PCB#aiutp_pcb{last_got_packet = aiutp_util:millisecond()}};
%% 处理所有非RESET和非SYN
process(Acc,_,
        #aiutp_packet{seq_nr = PktSeqNR,ack_nr = PktAckNR}=Packet,
        #aiutp_pcb{seq_nr = SeqNR,ack_nr = AckNR} = PCB)->
  CurrWindow = curr_window(PCB),
  if (?WRAPPING_DIFF_16((SeqNR - 1), PktAckNR) < 0) or
     (?WRAPPING_DIFF_16(PktAckNR, (SeqNR -1 -CurrWindow)) < 0) ->
      % ignore packets whose ack_nr is invalid. This would imply a spoofed address
      % or a malicious attempt to attach the uTP implementation.
      % acking a packet that hasn't been sent yet!
      % SYN packets have an exception, since there are no previous packets
      {Acc,PCB};
     true -> process_packet(Acc,Packet,PCB)
  end.

process_packet(Acc,
               #aiutp_packet{type = PktType,seq_nr = PktSeqNR} = Packet,
               #aiutp_pcb{state = State} = PCB)->
  Now = aiutp_util:millisecond(),
  PCB0 =
    if State == ?CS_SYN_SENT ->
        % if this is a syn-ack, initialize our ack_nr
        % to match the sequence number we got from the other end
        PCB#aiutp_pcb{ack_nr =  aiutp_util:bit16(PktSeqNR - 1),
                      last_got_packet = Now};
       true -> PCB#aiutp_pcb{last_got_packet = Now}
    end,
  DiffSeqNR = diff_seq_nr(PktSeqNR,PCB0),
  if DiffSeqNR >= ?REORDER_BUFFER_MAX_SIZE ->
      if (DiffSeqNR >= (?SEQ_NR_MASK + 1 - ?REORDER_BUFFER_MAX_SIZE)) and
         PktType /= ?ST_STATE -> {aiutp_net:send_ack(Acc, PCB0),PCB0};
         true -> {Acc,PCB0}
      end;
     true -> process_packet_1(Acc,Now,Packet,PCB0)
  end.

process_packet_1(Acc,Now,
                 #aiutp_packet{type = PktType,ack_nr = PktAckNR } = Packet,
                 #aiutp_pcb{cur_window_packets = CurWindowPackets,
                            duplicate_ack = DuplicateAck,
                            seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0->
  if (PktAckNR == aiutp_util:bit16(SeqNR -CurWindowPackets -1)) and
     PktType == ?ST_STATE ->
      process_packet_2(Acc,Now,Packet,
                       PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1});
     true ->
      process_packet_2(Acc,Now,Packet,
                       PCB#aiutp_pcb{duplicate_ack = 0})
  end;
% if we get the same ack_nr as in the last packet
% increase the duplicate_ack counter, otherwise reset it to 0.
% It's important to only count ACKs in ST_STATE packets. Any other
% packet (primarily ST_DATA) is likely to have been sent because of the
% other end having new outgoing data, not in response to incoming data.
% For instance, if we're receiving a steady stream of payload with no
% outgoing data, and we suddently have a few bytes of payload to send (say,
% a bittorrent HAVE message), we're very likely to see 3 duplicate ACKs
% immediately after sending our payload packet. This effectively disables
% the fast-resend on duplicate-ack logic for bi-directional connections
% (except in the case of a selective ACK). This is in line with BSD4.4 TCP
% implementation.

process_packet_1(Acc,Now,Packet,PCB)->
  process_packet_2(Acc,Now,Packet,PCB).

process_packet_2(Acc,Now,
                 #aiutp_packet{ack_nr = PktAckNR } = Packet,
                 #aiutp_pcb{cur_window_packets = CurWindowPackets,
                            seq_nr = SeqNR,outbuf = OutBuf} = PCB)->
  Acks = acks(PktAckNR,PCB),
  SeqBase = aiutp_util:bit16(SeqNR - CurWindowPackets),
  Iter = aiutp_buffer:head(OutBuf),
  {AckBytes,MinRTT} = acked_packet({0,SeqBae,Acks},{0,?RTT_MAX},Iter,PCB).
