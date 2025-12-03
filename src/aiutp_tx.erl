-module(aiutp_tx).
-include("aiutp.hrl").
-export([pick_acked/2,
         in/2,
         map_sack_to_seq/2,
         update_skip_counts/2]).


%% @private 计算我们需要从 outbuf 中移除多少数据包
-spec count_acked_packet(non_neg_integer(), #aiutp_pcb{}) -> non_neg_integer().
count_acked_packet(PktAckNR,#aiutp_pcb{seq_nr = SeqNR,
                         cur_window_packets = CurWindowPackets})->
  Acks = aiutp_util:bit16(PktAckNR - (SeqNR - 1 - CurWindowPackets)),
  if Acks > CurWindowPackets -> 0; % 收到旧的 ack nr 时会发生这种情况
     true -> Acks
  end.

%% @private 计算当前窗口包数
-spec caculate_cur_window_packets(non_neg_integer(), aiutp_buffer:aiutp_buffer()) -> non_neg_integer().
caculate_cur_window_packets(SeqNR,OutBuf)->
  Iter = aiutp_buffer:head(OutBuf),
  if Iter == -1 -> 0; %% 最后一个包已经被响应了
     true ->
      WrapPacket = aiutp_buffer:data(Iter,OutBuf),
      Packet = WrapPacket#aiutp_packet_wrap.packet,
      aiutp_util:bit16(SeqNR - Packet#aiutp_packet.seq_nr)
  end.


%% @private 从 SACK 的 bitmap 转换为序列号列表
-spec map_sack_to_seq(binary(), non_neg_integer(), non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
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
%% @doc 从扩展列表中解析 SACK 为序列号列表
-spec map_sack_to_seq(list(), non_neg_integer()) -> [non_neg_integer()].
map_sack_to_seq([],_) -> [];
map_sack_to_seq([{sack,Bits}|_],Base)-> map_sack_to_seq(Bits,0,Base,[]);
map_sack_to_seq([_|T],Base) -> map_sack_to_seq(T,Base).


%% @private 找出所有被 ACK 确认的数据包
-spec pick_acked_packet(non_neg_integer(), integer(), integer(), [#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()) ->
    {[#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()}.
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

%% @private 找出所有被 SACK 确认的数据包
-spec pick_sacked_packet([non_neg_integer()], non_neg_integer(), integer(), integer(),
                         [#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()) ->
    {[#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()}.
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


-spec pick_sacked_packet([non_neg_integer()], aiutp_buffer:aiutp_buffer()) ->
    {[#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()}.
pick_sacked_packet([],OutBuf) -> {[],OutBuf};
pick_sacked_packet([MaxSeq|_] = SAcks,OutBuf) ->
  Iter = aiutp_buffer:head(OutBuf),
  pick_sacked_packet(SAcks,MaxSeq, Iter,-1,[], OutBuf).

%% @doc 提取所有被 ACK 和 SACK 确认的数据包
-spec pick_acked(#aiutp_packet{}, #aiutp_pcb{}) ->
    {[#aiutp_packet_wrap{}], [#aiutp_packet_wrap{}], #aiutp_pcb{}}.
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



%% @doc 将数据写入发送队列
-spec in(binary(), #aiutp_pcb{}) -> #aiutp_pcb{}.
in(Data,#aiutp_pcb{outque = OutQue} = PCB) ->
  OutQue0 = aiutp_queue:push_back({?ST_DATA,Data}, OutQue),
  aiutp_net:flush_queue(PCB#aiutp_pcb{outque = OutQue0}).

%%------------------------------------------------------------------------------
%% @doc 根据 SACK 信息更新发送缓冲区中包的跳过计数
%%
%% BEP-29：当一个包被 SACK 跳过时（即后续的包通过 SACK 被确认，
%% 但该包没有被确认），增加其 skip_count。当 skip_count
%% 达到 3 时，该包应被标记为快速重传。
%%
%% @param SAckedSeqs 被选择性确认的序列号列表
%% @param PCB 协议控制块
%% @returns {SkippedCount, UpdatedPCB}，其中 SkippedCount 是因跳过计数
%%          阈值而新标记为重发的包数量
%% @end
%%------------------------------------------------------------------------------
-spec update_skip_counts([non_neg_integer()], #aiutp_pcb{}) -> {non_neg_integer(), #aiutp_pcb{}}.
update_skip_counts([], PCB) -> {0, PCB};
update_skip_counts(SAckedSeqs, #aiutp_pcb{outbuf = OutBuf, cur_window = CurWindow} = PCB) ->
    MaxSAckedSeq = lists:max(SAckedSeqs),
    Iter = aiutp_buffer:head(OutBuf),
    {SkippedCount, CurWindow0, OutBuf0} = update_skip_counts_iter(
        MaxSAckedSeq, SAckedSeqs, Iter, 0, CurWindow, OutBuf),
    {SkippedCount, PCB#aiutp_pcb{outbuf = OutBuf0, cur_window = CurWindow0}}.

%% @private 遍历 outbuf 并更新跳过计数
update_skip_counts_iter(_, _, -1, SkippedCount, CurWindow, OutBuf) ->
    {SkippedCount, CurWindow, OutBuf};
update_skip_counts_iter(MaxSAckedSeq, SAckedSeqs, Iter, SkippedCount, CurWindow, OutBuf) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Next = aiutp_buffer:next(Iter, OutBuf),
    #aiutp_packet_wrap{
        packet = Packet,
        transmissions = Transmissions,
        need_resend = NeedResend,
        skip_count = SkipCount,
        payload = Payload
    } = WrapPacket,
    SeqNR = Packet#aiutp_packet.seq_nr,

    %% 只处理满足以下条件的包：
    %% 1. 已至少传输一次
    %% 2. 尚未标记为重发
    %% 3. seq_nr < 最大 SACK 序列号（表示被跳过）
    %% 4. 不在 SACK 列表中（未被确认）
    ShouldIncrement = (Transmissions > 0) andalso
                      (NeedResend == false) andalso
                      (?WRAPPING_DIFF_16(MaxSAckedSeq, SeqNR) > 0) andalso
                      (not lists:member(SeqNR, SAckedSeqs)),

    if ShouldIncrement ->
        NewSkipCount = SkipCount + 1,
        if NewSkipCount >= ?DUPLICATE_ACKS_BEFORE_RESEND_BEP29 ->
            %% 标记为快速重传
            WrapPacket0 = WrapPacket#aiutp_packet_wrap{
                skip_count = NewSkipCount,
                need_resend = true
            },
            OutBuf0 = aiutp_buffer:replace(Iter, WrapPacket0, OutBuf),
            %% 调整 cur_window，因为这个包将被重发
            update_skip_counts_iter(MaxSAckedSeq, SAckedSeqs, Next,
                                    SkippedCount + 1, CurWindow - Payload, OutBuf0);
           true ->
            %% 只增加跳过计数
            WrapPacket0 = WrapPacket#aiutp_packet_wrap{skip_count = NewSkipCount},
            OutBuf0 = aiutp_buffer:replace(Iter, WrapPacket0, OutBuf),
            update_skip_counts_iter(MaxSAckedSeq, SAckedSeqs, Next,
                                    SkippedCount, CurWindow, OutBuf0)
        end;
       true ->
        update_skip_counts_iter(MaxSAckedSeq, SAckedSeqs, Next,
                                SkippedCount, CurWindow, OutBuf)
    end.
