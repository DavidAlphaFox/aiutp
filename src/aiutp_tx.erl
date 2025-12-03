%%------------------------------------------------------------------------------
%% @doc uTP 发送处理模块
%%
%% 本模块负责处理出站数据的发送和 ACK/SACK 的处理。
%%
%% == 发送流程 ==
%% ```
%% 应用数据 → write/2 (aiutp_pcb) → in/2 (本模块) → outque
%%         → flush_queue (aiutp_net) → outbuf
%%         → send_packet (aiutp_net) → UDP
%% '''
%%
%% == ACK 处理 ==
%% 当收到 ACK 时：
%% 1. extract_acked_packets/2: 从 outbuf 提取累积确认的包
%% 2. extract_sacked_packets/2: 从 outbuf 提取选择性确认的包
%% 3. 更新 cur_window_packets 计数
%%
%% == SACK 处理 ==
%% 选择性确认（SACK）允许接收方通告哪些乱序包已收到：
%% - parse_sack_extension/2: 从扩展字段解析 SACK 位图
%% - update_skip_counts/2: 更新被跳过包的计数，触发快速重传
%%
%% == 快速重传 ==
%% BEP-29：当一个包被 SACK 跳过 3 次时，应标记为快速重传。
%% skip_count 记录包被跳过的次数，达到阈值时设置 need_resend = true。
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @copyright (C) 2020, David Gao
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_tx).
-include("aiutp.hrl").

%%==============================================================================
%% API 导出
%%==============================================================================
-export([
    in/2,                   %% 写入数据到发送队列
    extract_acked/2,        %% 提取已确认的包
    parse_sack_extension/2, %% 解析 SACK 扩展
    update_skip_counts/2    %% 更新跳过计数
]).

%% 向后兼容别名
-export([
    pick_acked/2,
    map_sack_to_seq/2
]).

%%==============================================================================
%% API 函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 将数据写入发送队列
%%
%% 数据被包装为 {ST_DATA, Binary} 放入 outque，
%% 然后调用 flush_queue 尝试发送。
%%
%% 根据 libutp 实现，当发送缓冲区满时，状态从 CS_CONNECTED 转换为
%% CS_CONNECTED_FULL，用于流量控制。
%%
%% @param Data 要发送的二进制数据
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec in(binary(), #aiutp_pcb{}) -> #aiutp_pcb{}.
in(Data, #aiutp_pcb{outque = OutQue} = PCB) ->
    OutQue1 = aiutp_queue:push_back({?ST_DATA, Data}, OutQue),
    PCB1 = aiutp_net:flush_queue(PCB#aiutp_pcb{outque = OutQue1}),
    %% 检查是否需要转换到 CS_CONNECTED_FULL 状态
    maybe_transition_to_full(PCB1).

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查是否需要从 CS_CONNECTED 转换到 CS_CONNECTED_FULL
%%
%% 根据 libutp：当 is_full() 返回 true 且当前状态是 CS_CONNECTED 时，
%% 转换到 CS_CONNECTED_FULL 状态。这是流量控制的一部分，表示发送
%% 窗口已满，需要等待 ACK 释放空间后才能继续发送。
%% @end
%%------------------------------------------------------------------------------
-spec maybe_transition_to_full(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_transition_to_full(#aiutp_pcb{state = ?CS_CONNECTED} = PCB) ->
    {IsFull, PCB1} = aiutp_net:is_full(-1, PCB),
    case IsFull of
        true -> PCB1#aiutp_pcb{state = ?CS_CONNECTED_FULL};
        false -> PCB1
    end;
maybe_transition_to_full(PCB) ->
    PCB.

%%------------------------------------------------------------------------------
%% @doc 提取所有被 ACK 和 SACK 确认的数据包
%%
%% 根据收到的 ACK 包：
%% 1. 提取累积确认的包（seq_nr <= ack_nr）
%% 2. 提取选择性确认的包（在 SACK 位图中）
%% 3. 更新 cur_window_packets
%%
%% @param Packet 收到的 ACK 包
%% @param PCB 协议控制块
%% @returns {AckedPackets, SAckedPackets, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec extract_acked(#aiutp_packet{}, #aiutp_pcb{}) ->
    {[#aiutp_packet_wrap{}], [#aiutp_packet_wrap{}], #aiutp_pcb{}}.
extract_acked(#aiutp_packet{ack_nr = PktAckNR, extension = Exts},
              #aiutp_pcb{seq_nr = SeqNR, outbuf = OutBuf,
                         cur_window_packets = CurWindowPackets} = PCB) ->

    %% 计算被累积确认的包数量
    AckCount = count_acked_packets(PktAckNR, PCB),

    %% 提取累积确认的包
    {AckedPackets, OutBuf1} = case AckCount of
        0 -> {[], OutBuf};
        _ ->
            Iter = aiutp_buffer:head(OutBuf),
            extract_acked_packets(PktAckNR, Iter, -1, [], OutBuf)
    end,

    %% 提取选择性确认的包
    {SAckedPackets, OutBuf2} = case AckCount == CurWindowPackets of
        true ->
            %% 所有包都已被 ACK，无需处理 SACK
            {[], OutBuf1};
        false ->
            SAckSeqs = parse_sack_extension(Exts, aiutp_util:bit16(PktAckNR + 2)),
            extract_sacked_packets(SAckSeqs, OutBuf1)
    end,

    %% 更新 cur_window_packets
    NewCurWindowPackets = calculate_cur_window_packets(SeqNR, OutBuf2),

    {AckedPackets, SAckedPackets,
     PCB#aiutp_pcb{
         cur_window_packets = NewCurWindowPackets,
         outbuf = OutBuf2
     }}.

%% @doc extract_acked/2 的向后兼容别名
%% @deprecated 请使用 extract_acked/2
-spec pick_acked(#aiutp_packet{}, #aiutp_pcb{}) ->
    {[#aiutp_packet_wrap{}], [#aiutp_packet_wrap{}], #aiutp_pcb{}}.
pick_acked(Packet, PCB) ->
    extract_acked(Packet, PCB).

%%------------------------------------------------------------------------------
%% @doc 从扩展列表中解析 SACK 为序列号列表
%%
%% SACK 扩展包含 4 字节位图，覆盖 [base, base + 31] 的序列号范围。
%% 每字节内的位顺序是 LSB 优先（位 0 = 最小序列号）。
%%
%% @param Extensions 扩展列表
%% @param BaseSeqNR SACK 基准序列号（通常是 ack_nr + 2）
%% @returns 被选择性确认的序列号列表
%% @end
%%------------------------------------------------------------------------------
-spec parse_sack_extension(list(), non_neg_integer()) -> [non_neg_integer()].
parse_sack_extension([], _BaseSeqNR) ->
    [];
parse_sack_extension([{sack, Bits} | _], BaseSeqNR) ->
    parse_sack_bitmap(Bits, 0, BaseSeqNR, []);
parse_sack_extension([_ | Rest], BaseSeqNR) ->
    parse_sack_extension(Rest, BaseSeqNR).

%% @doc parse_sack_extension/2 的向后兼容别名
%% @deprecated 请使用 parse_sack_extension/2
-spec map_sack_to_seq(list(), non_neg_integer()) -> [non_neg_integer()].
map_sack_to_seq(Extensions, BaseSeqNR) ->
    parse_sack_extension(Extensions, BaseSeqNR).

%%------------------------------------------------------------------------------
%% @doc 根据 SACK 信息更新发送缓冲区中包的跳过计数
%%
%% BEP-29：当一个包被 SACK 跳过时（即后续的包通过 SACK 被确认，
%% 但该包没有被确认），增加其 skip_count。当 skip_count
%% 达到 DUPLICATE_ACKS_BEFORE_RESEND (3) 时，标记为快速重传。
%%
%% @param SAckedSeqs 被选择性确认的序列号列表
%% @param PCB 协议控制块
%% @returns {SkippedCount, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec update_skip_counts([non_neg_integer()], #aiutp_pcb{}) ->
    {non_neg_integer(), #aiutp_pcb{}}.
update_skip_counts([], PCB) ->
    {0, PCB};
update_skip_counts(SAckedSeqs, #aiutp_pcb{outbuf = OutBuf, cur_window = CurWindow} = PCB) ->
    MaxSAckedSeq = lists:max(SAckedSeqs),
    Iter = aiutp_buffer:head(OutBuf),
    {SkippedCount, CurWindow1, OutBuf1} =
        update_skip_counts_loop(MaxSAckedSeq, SAckedSeqs, Iter, 0, CurWindow, OutBuf),
    {SkippedCount, PCB#aiutp_pcb{outbuf = OutBuf1, cur_window = CurWindow1}}.

%%==============================================================================
%% 内部函数 - ACK 计算
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算被累积确认的包数量
%%
%% 计算从 outbuf 中需要移除多少包（所有 seq_nr <= ack_nr 的包）。
%%------------------------------------------------------------------------------
-spec count_acked_packets(non_neg_integer(), #aiutp_pcb{}) -> non_neg_integer().
count_acked_packets(PktAckNR, #aiutp_pcb{seq_nr = SeqNR,
                                          cur_window_packets = CurWindowPackets}) ->
    %% 计算已确认的包数量
    %% SeqBase = SeqNR - 1 - CurWindowPackets (最旧未确认包的序列号 - 1)
    Acks = aiutp_util:bit16(PktAckNR - (SeqNR - 1 - CurWindowPackets)),
    case Acks > CurWindowPackets of
        true -> 0;  %% 收到旧的 ACK
        false -> Acks
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算当前窗口包数
%%
%% 根据 outbuf 中最旧的包的序列号计算。
%%------------------------------------------------------------------------------
-spec calculate_cur_window_packets(non_neg_integer(), aiutp_buffer:aiutp_buffer()) ->
    non_neg_integer().
calculate_cur_window_packets(SeqNR, OutBuf) ->
    Iter = aiutp_buffer:head(OutBuf),
    case Iter of
        -1 ->
            %% 所有包都已被确认
            0;
        _ ->
            WrapPacket = aiutp_buffer:data(Iter, OutBuf),
            Packet = WrapPacket#aiutp_packet_wrap.packet,
            aiutp_util:bit16(SeqNR - Packet#aiutp_packet.seq_nr)
    end.

%%==============================================================================
%% 内部函数 - ACK 提取
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 从 outbuf 提取累积确认的包
%%
%% 遍历 outbuf，移除所有 seq_nr <= MaxSeqNR 的包。
%%------------------------------------------------------------------------------
-spec extract_acked_packets(non_neg_integer(), integer(), integer(),
                            [#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()) ->
    {[#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()}.
extract_acked_packets(_MaxSeqNR, -1, _Prev, Acc, OutBuf) ->
    {Acc, OutBuf};
extract_acked_packets(MaxSeqNR, Iter, Prev, Acc, OutBuf) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Next = aiutp_buffer:next(Iter, OutBuf),
    Packet = WrapPacket#aiutp_packet_wrap.packet,

    case ?WRAPPING_DIFF_16(Packet#aiutp_packet.seq_nr, MaxSeqNR) > 0 of
        true ->
            %% 超过 MaxSeqNR，停止
            {Acc, OutBuf};
        false ->
            %% 移除并继续
            OutBuf1 = aiutp_buffer:delete(Iter, Prev, OutBuf),
            extract_acked_packets(MaxSeqNR, Next, Prev, [WrapPacket | Acc], OutBuf1)
    end.

%%==============================================================================
%% 内部函数 - SACK 处理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 解析 SACK 位图为序列号列表
%%
%% SACK 位图每字节表示 8 个序列号，LSB 优先。
%%------------------------------------------------------------------------------
-spec parse_sack_bitmap(binary(), non_neg_integer(), non_neg_integer(),
                        [non_neg_integer()]) -> [non_neg_integer()].
parse_sack_bitmap(<<>>, _ByteIndex, _BaseSeqNR, Acc) ->
    Acc;
parse_sack_bitmap(<<0, Rest/binary>>, ByteIndex, BaseSeqNR, Acc) ->
    %% 优化：跳过全零字节
    parse_sack_bitmap(Rest, ByteIndex + 1, BaseSeqNR, Acc);
parse_sack_bitmap(<<Bits, Rest/binary>>, ByteIndex, BaseSeqNR, Acc) ->
    %% 解析字节中的每一位
    Offset = ByteIndex * 8,
    Acc1 = parse_sack_byte(Bits, 0, Offset, BaseSeqNR, Acc),
    parse_sack_bitmap(Rest, ByteIndex + 1, BaseSeqNR, Acc1).

%%------------------------------------------------------------------------------
%% @private
%% @doc 解析单个 SACK 字节
%%------------------------------------------------------------------------------
-spec parse_sack_byte(non_neg_integer(), non_neg_integer(), non_neg_integer(),
                      non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
parse_sack_byte(_Bits, 8, _Offset, _BaseSeqNR, Acc) ->
    Acc;
parse_sack_byte(Bits, BitIndex, Offset, BaseSeqNR, Acc) ->
    Acc1 = case Bits band (1 bsl BitIndex) of
        0 -> Acc;
        _ -> [aiutp_util:bit16(BaseSeqNR + Offset + BitIndex) | Acc]
    end,
    parse_sack_byte(Bits, BitIndex + 1, Offset, BaseSeqNR, Acc1).

%%------------------------------------------------------------------------------
%% @private
%% @doc 从 outbuf 提取选择性确认的包
%%------------------------------------------------------------------------------
-spec extract_sacked_packets([non_neg_integer()], aiutp_buffer:aiutp_buffer()) ->
    {[#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()}.
extract_sacked_packets([], OutBuf) ->
    {[], OutBuf};
extract_sacked_packets([MaxSeq | _] = SAckSeqs, OutBuf) ->
    Iter = aiutp_buffer:head(OutBuf),
    extract_sacked_packets_loop(SAckSeqs, MaxSeq, Iter, -1, [], OutBuf).

%%------------------------------------------------------------------------------
%% @private
%% @doc 遍历 outbuf 提取 SACK 确认的包
%%------------------------------------------------------------------------------
-spec extract_sacked_packets_loop([non_neg_integer()], non_neg_integer(),
                                  integer(), integer(),
                                  [#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()) ->
    {[#aiutp_packet_wrap{}], aiutp_buffer:aiutp_buffer()}.
extract_sacked_packets_loop(_SAckSeqs, _MaxSeq, -1, _Prev, Acc, OutBuf) ->
    {Acc, OutBuf};
extract_sacked_packets_loop(SAckSeqs, MaxSeq, Iter, Prev, Acc, OutBuf) ->
    Next = aiutp_buffer:next(Iter, OutBuf),
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Packet = WrapPacket#aiutp_packet_wrap.packet,
    SeqNR = Packet#aiutp_packet.seq_nr,

    case ?WRAPPING_DIFF_16(SeqNR, MaxSeq) > 0 of
        true ->
            %% 超过 MaxSeq，停止
            {Acc, OutBuf};
        false ->
            case lists:member(SeqNR, SAckSeqs) of
                true ->
                    %% 在 SACK 列表中，移除
                    OutBuf1 = aiutp_buffer:delete(Iter, Prev, OutBuf),
                    extract_sacked_packets_loop(SAckSeqs, MaxSeq, Next, Prev,
                                                [WrapPacket | Acc], OutBuf1);
                false ->
                    %% 不在 SACK 列表中，继续
                    extract_sacked_packets_loop(SAckSeqs, MaxSeq, Next, Iter, Acc, OutBuf)
            end
    end.

%%==============================================================================
%% 内部函数 - 跳过计数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 遍历 outbuf 更新跳过计数
%%------------------------------------------------------------------------------
-spec update_skip_counts_loop(non_neg_integer(), [non_neg_integer()],
                              integer(), non_neg_integer(),
                              non_neg_integer(), aiutp_buffer:aiutp_buffer()) ->
    {non_neg_integer(), non_neg_integer(), aiutp_buffer:aiutp_buffer()}.
update_skip_counts_loop(_MaxSAckedSeq, _SAckedSeqs, -1, SkippedCount, CurWindow, OutBuf) ->
    {SkippedCount, CurWindow, OutBuf};
update_skip_counts_loop(MaxSAckedSeq, SAckedSeqs, Iter, SkippedCount, CurWindow, OutBuf) ->
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

    %% 判断是否应该增加跳过计数：
    %% 1. 已至少传输一次
    %% 2. 尚未标记为重发
    %% 3. seq_nr < 最大 SACK 序列号（表示被跳过）
    %% 4. 不在 SACK 列表中（未被确认）
    ShouldIncrement = (Transmissions > 0) andalso
                      (NeedResend == false) andalso
                      (?WRAPPING_DIFF_16(MaxSAckedSeq, SeqNR) > 0) andalso
                      (not lists:member(SeqNR, SAckedSeqs)),

    case ShouldIncrement of
        true ->
            NewSkipCount = SkipCount + 1,
            case NewSkipCount >= ?DUPLICATE_ACKS_BEFORE_RESEND of
                true ->
                    %% 达到阈值，标记为快速重传
                    WrapPacket1 = WrapPacket#aiutp_packet_wrap{
                        skip_count = NewSkipCount,
                        need_resend = true
                    },
                    OutBuf1 = aiutp_buffer:replace(Iter, WrapPacket1, OutBuf),
                    %% 从 cur_window 中减去载荷大小（因为将被重发）
                    update_skip_counts_loop(MaxSAckedSeq, SAckedSeqs, Next,
                                            SkippedCount + 1, CurWindow - Payload, OutBuf1);
                false ->
                    %% 只增加计数
                    WrapPacket1 = WrapPacket#aiutp_packet_wrap{skip_count = NewSkipCount},
                    OutBuf1 = aiutp_buffer:replace(Iter, WrapPacket1, OutBuf),
                    update_skip_counts_loop(MaxSAckedSeq, SAckedSeqs, Next,
                                            SkippedCount, CurWindow, OutBuf1)
            end;
        false ->
            update_skip_counts_loop(MaxSAckedSeq, SAckedSeqs, Next,
                                    SkippedCount, CurWindow, OutBuf)
    end.
