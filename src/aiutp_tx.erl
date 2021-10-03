-module(aiutp_tx).
-include("aiutp.hrl").
-export([pick_acked/2,
         in/2,
         map_sack_to_seq/2]).


%% 迎来计算我们需要从outbuf中移除多少数据包
count_acked_packet(PktAckNR,#aiutp_pcb{seq_nr = SeqNR,
                         cur_window_packets = CurWindowPackets})->
  Acks = aiutp_util:bit16(PktAckNR - (SeqNR - 1 - CurWindowPackets)),
  if Acks > CurWindowPackets -> 0; % this happens when we receive an old ack nr
     true -> Acks
  end.
% Process acknowledgment acks is the number of packets that was acked

caculate_cur_window_packets(SeqNR,OutBuf)->
  Iter = aiutp_buffer:head(OutBuf),
  if Iter == -1 -> 0; %% 最后一个包已经被响应了
     true ->
      WrapPacket = aiutp_buffer:data(Iter,OutBuf),
      Packet = WrapPacket#aiutp_packet_wrap.packet,
      aiutp_util:bit16(SeqNR - Packet#aiutp_packet.seq_nr)
  end.


%% 从Sack的bitmap，转化程序列号
map_sack_to_seq(<<>>,_,_,Acc)-> Acc;
map_sack_to_seq(<<Bits/big-unsigned-integer,Rest/bits>>,Index,Base,Acc) ->
  if Bits == 0 -> map_sack_to_seq(Rest,Index+ 1,Base,Acc);
     true ->
      Offset = Index * 8,
      Acc1 = lists:foldl(
               fun(I,Acc0) ->
                   Hint = Bits band (1 bsl I),
                   if Hint > 0 -> [aiutp_util:bit16(Base + Offset + I)|Acc0];
                      true -> Acc0
                   end
               end,Acc,lists:seq(0, 7)),
      map_sack_to_seq(Rest,Index+1,Base,Acc1)
  end.
map_sack_to_seq([],_) -> [];
map_sack_to_seq([{sack,Bits}|_],Base)-> map_sack_to_seq(Bits,0,Base,[]);
map_sack_to_seq([_|T],Base) -> map_sack_to_seq(T,Base).


%% 找出所有被收到的ACKNR响应的数据包
pick_acked_packet(_,-1,_,Acc,OutBuf)-> {Acc,OutBuf};
pick_acked_packet(MaxSeq,Iter,Prev,Acc,OutBuf)->
  WrapPacket = aiutp_buffer:data(Iter,OutBuf),
  Next = aiutp_buffer:next(Iter,OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  if ?WRAPPING_DIFF_16(Packet#aiutp_packet.seq_nr,MaxSeq) > 0  ->
      {Acc,OutBuf};
     true ->
      OutBuf0 = aiutp_buffer:delete(Iter,Prev,OutBuf),
      pick_acked_packet(MaxSeq,Next,Prev,[WrapPacket|Acc],OutBuf0)
  end.

pick_sacked_packet(_,_,-1,_,Acc,OutBuf)-> {Acc,OutBuf};
pick_sacked_packet(SAcks,MaxSeq,Iter,Prev,Acc,OutBuf)->
  Next = aiutp_buffer:next(Iter, OutBuf),
  WrapPacket = aiutp_buffer:data(Iter, OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  if ?WRAPPING_DIFF_16(Packet#aiutp_packet.seq_nr, MaxSeq) > 0 -> {Acc,OutBuf};
     true ->
      Member = lists:member(Packet#aiutp_packet.seq_nr, SAcks),
      if Member ->
          OutBuf0 = aiutp_buffer:delete(Iter,Prev,OutBuf),
          pick_sacked_packet(SAcks,MaxSeq,Next,Prev,[WrapPacket|Acc],OutBuf0);
         true -> pick_sacked_packet(SAcks,MaxSeq,Next,Iter,Acc,OutBuf)
      end
  end.


pick_sacked_packet([],OutBuf) -> {[],OutBuf};
pick_sacked_packet([MaxSeq|_] = SAcks,OutBuf) ->
  Iter = aiutp_buffer:head(OutBuf),
  pick_sacked_packet(SAcks,MaxSeq, Iter,-1,[], OutBuf).

pick_acked(#aiutp_packet{ack_nr = PktAckNR,extension = Exts },
           #aiutp_pcb{seq_nr = SeqNR,outbuf = OutBuf,cur_window_packets = CurWindowPackets} = PCB) ->

  %% SeqNR = 6 CurWindowPackets = 5 AckNR = 3
  %% Acks = 3 SeqBase = 1 , MaxSeq = 4
  %% SeqNo 为4的包对面没收到
  Acks = count_acked_packet(PktAckNR,PCB),
  {AckedPacket,OutBuf0} =
    if Acks == 0 -> {[],OutBuf};
       true ->
        Iter = aiutp_buffer:head(OutBuf),
        pick_acked_packet(PktAckNR,Iter,-1,[],OutBuf)
    end,
  {SAckedPacket,OutBuf1} =
    if Acks == CurWindowPackets -> {[],OutBuf0};
       true ->
        SAcks = map_sack_to_seq(Exts,aiutp_util:bit16(PktAckNR + 2)),
        pick_sacked_packet(SAcks,OutBuf0)
    end,
  {AckedPacket,SAckedPacket,
   PCB#aiutp_pcb{
     cur_window_packets = caculate_cur_window_packets(SeqNR, OutBuf1),
     outbuf = OutBuf1
    }}.



in(Data,#aiutp_pcb{outque = OutQue} = PCB) ->
  OutQue0 = aiutp_queue:push_back({?ST_DATA,Data}, OutQue),
  aiutp_net:flush_queue(PCB#aiutp_pcb{outque = OutQue0}).
