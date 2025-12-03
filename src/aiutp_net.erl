%%------------------------------------------------------------------------------
%% @doc uTP 网络 I/O 模块
%%
%% 本模块负责所有网络相关操作，包括发送数据包、窗口管理和 ACK 调度。
%%
%% == 发送流程 ==
%% ```
%% flush_queue/1 → send_queued_data/1 → send_new_packet/4 → do_send/2
%% flush_packets/1 → send_packet/2 → do_send/2
%% '''
%%
%% == 窗口管理 ==
%% - window_size/2: 计算接收窗口大小
%% - is_full/2: 检查发送窗口是否已满
%% - max_send/1: 计算可发送的最大字节数
%%
%% == 发送模式 ==
%% 发送受拥塞窗口 (max_window) 和对端窗口 (max_window_user) 限制
%%
%% == ACK 调度 ==
%% - schedule_ack/1: 检查 ida 标志，如需要则发送 ACK
%% - send_ack/1: 立即发送 ACK 包
%% - send_keep_alive/1: 发送 keepalive 包
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @copyright (C) 2020, David Gao
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_net).
-include("aiutp.hrl").

%%==============================================================================
%% API 导出
%%==============================================================================
-export([
    %% 窗口管理
    window_size/2,
    is_full/2,
    %% 数据包发送
    flush_packets/1,
    flush_queue/1,
    send_packet/2,
    send_n_packets/4,
    send_skipped_packets/5,
    %% 控制包发送
    schedule_ack/1,
    send_ack/1,
    send_fin/1,
    send_reset/1,
    send_keep_alive/1
]).

%%==============================================================================
%% 常量定义
%%==============================================================================

%% UDP 发送重试次数
-define(UDP_SEND_RETRIES, 3).

%% UDP 发送重试间隔（毫秒）
-define(UDP_SEND_RETRY_DELAY, 150).

%%==============================================================================
%% API 函数 - 窗口管理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 计算接收窗口大小（字节）
%%
%% 接收窗口 = 未使用的缓冲区槽位 * 包大小
%%
%% @param MaxWindow 最大窗口大小（未使用）
%% @param InBuf 接收缓冲区
%% @returns 接收窗口大小（字节）
%% @end
%%------------------------------------------------------------------------------
-spec window_size(integer(), aiutp_buffer:aiutp_buffer()) -> non_neg_integer().
window_size(_MaxWindow, InBuf) ->
    aiutp_buffer:unused(InBuf) * ?PACKET_SIZE.

%%------------------------------------------------------------------------------
%% @doc 检查发送窗口是否已满
%%
%% 检查 cur_window + Bytes 是否超过窗口限制
%%
%% @param Bytes 要发送的字节数（-1 表示使用 PACKET_SIZE）
%% @param PCB 协议控制块
%% @returns {IsFull, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec is_full(integer(), #aiutp_pcb{}) -> {boolean(), #aiutp_pcb{}}.
is_full(Bytes, #aiutp_pcb{time = Now,
                          max_window = MaxWindow,
                          max_window_user = MaxWindowUser,
                          cur_window = CurWindow,
                          cur_window_packets = CurWindowPackets} = PCB) ->
    Bytes1 = normalize_bytes(Bytes),
    MaxSend = erlang:min(MaxWindow, MaxWindowUser),

    IsFull = (CurWindowPackets >= (?OUTGOING_BUFFER_MAX_SIZE - 1)) orelse
             ((CurWindow + Bytes1) > MaxSend),

    case IsFull of
        true -> {true, PCB#aiutp_pcb{last_maxed_out_window = Now}};
        false -> {false, PCB}
    end.

%%==============================================================================
%% API 函数 - 数据包发送
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 刷新待发送的数据包
%%
%% 遍历 outbuf，发送所有需要发送的包（新包或标记为重发的包）。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec flush_packets(#aiutp_pcb{}) -> #aiutp_pcb{}.
flush_packets(#aiutp_pcb{outbuf = OutBuf} = PCB) ->
    Iter = aiutp_buffer:head(OutBuf),
    flush_packets_loop(Iter, PCB).

%%------------------------------------------------------------------------------
%% @doc 刷新发送队列
%%
%% 将 outque 中的数据打包发送。如果窗口允许，尽可能多地发送。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec flush_queue(#aiutp_pcb{}) -> #aiutp_pcb{}.
flush_queue(#aiutp_pcb{outque = OutQue} = PCB) ->
    case aiutp_queue:empty(OutQue) of
        true -> PCB;
        false -> do_flush_queue(PCB)
    end.

%%------------------------------------------------------------------------------
%% @doc 发送指定位置的数据包
%%
%% @param Pos outbuf 中的位置（-1 表示无效）
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec send_packet(integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
send_packet(-1, PCB) ->
    PCB;
send_packet(Pos, #aiutp_pcb{time = Now,
                            socket = Socket,
                            ack_nr = AckNR,
                            cur_window = CurWindow,
                            inbuf = InBuf,
                            max_window = MaxWindow,
                            outbuf = OutBuf,
                            reply_micro = ReplyMicro} = PCB) ->
    MicroNow = aiutp_util:microsecond(),
    WrapPacket = aiutp_buffer:data(Pos, OutBuf),
    WindowSize = window_size(MaxWindow, InBuf),

    {SendBytes, WrapPacket1, Content} =
        prepare_packet_for_send(MicroNow, ReplyMicro, WindowSize, AckNR, WrapPacket),

    OutBuf1 = aiutp_buffer:replace(Pos, WrapPacket1, OutBuf),
    do_send(Socket, Content),

    PCB#aiutp_pcb{
        cur_window = CurWindow + SendBytes,
        outbuf = OutBuf1,
        last_sent_packet = Now
    }.

%%------------------------------------------------------------------------------
%% @doc 发送指定序列号范围内的 N 个数据包
%%
%% @param MinSeq 最小序列号
%% @param MaxSeq 最大序列号
%% @param Limit 最多发送的包数
%% @param PCB 协议控制块
%% @returns {SentCount, LastSeqNR, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec send_n_packets(non_neg_integer(), non_neg_integer(), non_neg_integer(), #aiutp_pcb{}) ->
    {non_neg_integer(), non_neg_integer(), #aiutp_pcb{}}.
send_n_packets(MinSeq, MaxSeq, Limit, #aiutp_pcb{outbuf = OutBuf} = PCB) ->
    Iter = aiutp_buffer:head(OutBuf),
    {Remain, LastSeq, PCB1} = send_n_packets_loop(MinSeq, MaxSeq, Limit, Iter, PCB),
    {Limit - Remain, LastSeq, PCB1}.

%%------------------------------------------------------------------------------
%% @doc 发送被 SACK 跳过的数据包（精确快速重传）
%%
%% libutp 风格：只重传在指定范围内且未被 SACK 确认的包。
%% 这比 send_n_packets 更精确，避免重传已被 SACK 确认的包。
%%
%% @param MinSeq 最小序列号
%% @param MaxSeq 最大序列号
%% @param SAckedSeqs SACK 确认的序列号集合
%% @param Limit 最多发送的包数
%% @param PCB 协议控制块
%% @returns {SentCount, LastSeqNR, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec send_skipped_packets(non_neg_integer(), non_neg_integer(), sets:set(),
                           non_neg_integer(), #aiutp_pcb{}) ->
    {non_neg_integer(), non_neg_integer(), #aiutp_pcb{}}.
send_skipped_packets(MinSeq, MaxSeq, SAckedSeqs, Limit, #aiutp_pcb{outbuf = OutBuf} = PCB) ->
    Iter = aiutp_buffer:head(OutBuf),
    {Remain, LastSeq, PCB1} = send_skipped_packets_loop(
        MinSeq, MaxSeq, SAckedSeqs, Limit, Iter, PCB),
    {Limit - Remain, LastSeq, PCB1}.

%%==============================================================================
%% API 函数 - 控制包发送
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 调度 ACK 发送
%%
%% 如果 ida (immediate delayed ack) 标志为 true，立即发送 ACK。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec schedule_ack(#aiutp_pcb{}) -> #aiutp_pcb{}.
schedule_ack(#aiutp_pcb{ida = false} = PCB) ->
    PCB;
schedule_ack(PCB) ->
    send_ack(PCB#aiutp_pcb{ida = false}).

%%------------------------------------------------------------------------------
%% @doc 发送 ACK 包
%%
%% 如果有乱序包（reorder_count > 0），附带 SACK 扩展。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec send_ack(#aiutp_pcb{}) -> #aiutp_pcb{}.
send_ack(#aiutp_pcb{time = Now,
                    socket = Socket,
                    conn_id_send = ConnIdSend,
                    seq_nr = SeqNR,
                    ack_nr = AckNR,
                    reorder_count = ReorderCount,
                    got_fin_reached = GotFinReached,
                    inbuf = InBuf,
                    max_window = MaxWindow,
                    reply_micro = ReplyMicro} = PCB) ->
    MicroNow = aiutp_util:microsecond(),
    WindowSize = window_size(MaxWindow, InBuf),
    Packet = aiutp_packet:ack(SeqNR, AckNR),

    Packet1 = case (ReorderCount /= 0) andalso (GotFinReached == false) of
        true ->
            %% 有乱序包，附带 SACK
            Sack = build_sack(PCB),
            Packet#aiutp_packet{
                conn_id = ConnIdSend,
                wnd = WindowSize,
                tv_usec = MicroNow,
                reply_micro = ReplyMicro,
                extension = [{sack, Sack}]
            };
        false ->
            Packet#aiutp_packet{
                conn_id = ConnIdSend,
                tv_usec = MicroNow,
                reply_micro = ReplyMicro,
                wnd = WindowSize
            }
    end,

    do_send(Socket, aiutp_packet:encode(Packet1)),
    PCB#aiutp_pcb{last_sent_packet = Now, last_rcv_win = WindowSize}.

%%------------------------------------------------------------------------------
%% @doc 发送 FIN 包
%%
%% 将 FIN 放入发送队列并刷新。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec send_fin(#aiutp_pcb{}) -> #aiutp_pcb{}.
send_fin(#aiutp_pcb{outque = OutQue} = PCB) ->
    OutQue1 = aiutp_queue:push_back({?ST_FIN, <<>>}, OutQue),
    flush_queue(PCB#aiutp_pcb{outque = OutQue1}).

%%------------------------------------------------------------------------------
%% @doc 发送 RESET 包
%%
%% BEP-29：RESET 包用于通知对端连接终止，允许对端立即清理状态。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec send_reset(#aiutp_pcb{}) -> #aiutp_pcb{}.
send_reset(#aiutp_pcb{time = Now,
                      socket = Socket,
                      conn_id_send = ConnIdSend,
                      ack_nr = AckNR,
                      reply_micro = ReplyMicro} = PCB) ->
    MicroNow = aiutp_util:microsecond(),
    Packet = aiutp_packet:reset(ConnIdSend, AckNR),
    Packet1 = Packet#aiutp_packet{tv_usec = MicroNow, reply_micro = ReplyMicro},
    do_send(Socket, aiutp_packet:encode(Packet1)),
    PCB#aiutp_pcb{last_sent_packet = Now}.

%%------------------------------------------------------------------------------
%% @doc 发送 Keepalive 包
%%
%% 通过发送一个带有错误 ack_nr 的 ACK 来实现，对端会响应一个新的 ACK。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec send_keep_alive(#aiutp_pcb{}) -> #aiutp_pcb{}.
send_keep_alive(#aiutp_pcb{ack_nr = AckNR} = PCB) ->
    %% 发送一个 ack_nr - 1 的 ACK，触发对端重传
    PCB1 = send_ack(PCB#aiutp_pcb{ack_nr = aiutp_util:bit16(AckNR - 1)}),
    PCB1#aiutp_pcb{ack_nr = AckNR}.

%%==============================================================================
%% 内部函数 - 辅助函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 规范化字节数参数
%%------------------------------------------------------------------------------
-spec normalize_bytes(integer()) -> non_neg_integer().
normalize_bytes(Bytes) when Bytes > ?PACKET_SIZE -> ?PACKET_SIZE;
normalize_bytes(Bytes) when Bytes < 0 -> ?PACKET_SIZE;
normalize_bytes(Bytes) -> Bytes.

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算可发送的最大字节数
%%------------------------------------------------------------------------------
-spec max_send(#aiutp_pcb{}) -> non_neg_integer().
max_send(#aiutp_pcb{max_window = MaxWindow,
                    max_window_user = MaxWindowUser,
                    cur_window = CurWindow,
                    cur_window_packets = CurWindowPackets}) ->
    case CurWindowPackets >= (?OUTGOING_BUFFER_MAX_SIZE - 1) of
        true -> 0;
        false ->
            MaxSend = erlang:min(MaxWindow, MaxWindowUser),
            case MaxSend > CurWindow of
                true ->
                    Remain = MaxSend - CurWindow,
                    case Remain < ?PACKET_SIZE of
                        true -> ?PACKET_SIZE;
                        false -> Remain
                    end;
                false -> 0
            end
    end.

%%==============================================================================
%% 内部函数 - 数据包发送
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 遍历 outbuf 发送待发送的包
%%------------------------------------------------------------------------------
-spec flush_packets_loop(integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
flush_packets_loop(-1, PCB) ->
    PCB;
flush_packets_loop(Iter, #aiutp_pcb{outbuf = OutBuf} = PCB) ->
    {IsFull, PCB1} = is_full(-1, PCB),
    case IsFull of
        true ->
            %% 窗口已满，检查是否需要转换到 CS_CONNECTED_FULL
            maybe_transition_to_full(PCB1);
        false ->
            Next = aiutp_buffer:next(Iter, OutBuf),
            WrapPacket = aiutp_buffer:data(Iter, OutBuf),
            ShouldSend = (WrapPacket#aiutp_packet_wrap.transmissions == 0) orelse
                         (WrapPacket#aiutp_packet_wrap.need_resend == true),
            case ShouldSend of
                true ->
                    PCB2 = send_packet(Iter, PCB1),
                    flush_packets_loop(Next, PCB2);
                false ->
                    flush_packets_loop(Next, PCB1)
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查是否需要从 CS_CONNECTED 转换到 CS_CONNECTED_FULL
%%
%% 根据 libutp：当 is_full() 返回 true 且当前状态是 CS_CONNECTED 时，
%% 转换到 CS_CONNECTED_FULL 状态。
%% @end
%%------------------------------------------------------------------------------
-spec maybe_transition_to_full(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_transition_to_full(#aiutp_pcb{state = ?CS_CONNECTED} = PCB) ->
    PCB#aiutp_pcb{state = ?CS_CONNECTED_FULL};
maybe_transition_to_full(PCB) ->
    PCB.

%%------------------------------------------------------------------------------
%% @private
%% @doc 执行队列刷新
%%------------------------------------------------------------------------------
-spec do_flush_queue(#aiutp_pcb{}) -> #aiutp_pcb{}.
do_flush_queue(#aiutp_pcb{time = Now,
                          cur_window_packets = CurWindowPackets,
                          rto = RTO} = PCB) ->
    %% 如果是第一个包，设置 RTO 超时
    PCB1 = case CurWindowPackets of
        0 -> PCB#aiutp_pcb{retransmit_timeout = RTO, rto_timeout = Now + RTO};
        _ -> PCB
    end,

    {IsFull, PCB2} = is_full(-1, PCB1),
    case IsFull of
        true -> PCB2;
        false -> send_queued_data(PCB2)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 从队列发送数据
%%------------------------------------------------------------------------------
-spec send_queued_data(#aiutp_pcb{}) -> #aiutp_pcb{}.
send_queued_data(#aiutp_pcb{outque = OutQue} = PCB) ->
    case aiutp_queue:empty(OutQue) of
        true -> PCB;
        false ->
            {Type, Bin} = aiutp_queue:front(OutQue),
            OutQue1 = aiutp_queue:pop_front(OutQue),
            MaxSend = max_send(PCB),
            send_data_chunk(Type, Bin, MaxSend, PCB#aiutp_pcb{outque = OutQue1})
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 发送数据块（可能需要分片）
%%------------------------------------------------------------------------------
-spec send_data_chunk(integer(), binary(), integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
send_data_chunk(_Type, <<>>, _MaxSend, PCB) ->
    send_queued_data(PCB);
send_data_chunk(Type, Bin, MaxSend, #aiutp_pcb{outque = OutQue} = PCB) when MaxSend =< 0 ->
    %% 窗口已满，将剩余数据放回队列
    case byte_size(Bin) > 0 of
        true -> PCB#aiutp_pcb{outque = aiutp_queue:push_front({Type, Bin}, OutQue)};
        false -> PCB
    end;
send_data_chunk(Type, Bin, MaxSend, PCB) ->
    BinSize = byte_size(Bin),
    ChunkSize = erlang:min(MaxSend, ?PACKET_SIZE),

    case BinSize =< ChunkSize of
        true ->
            %% 数据可以放入一个包
            send_data_chunk_small(Type, Bin, BinSize, ChunkSize, PCB);
        false ->
            %% 数据需要分片
            <<Data:ChunkSize/binary, Rest/binary>> = Bin,
            PCB1 = send_new_packet(Type, Data, ChunkSize, PCB),
            send_data_chunk(Type, Rest, MaxSend - ChunkSize, PCB1)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 发送小数据块（可能需要与后续数据合并）
%%------------------------------------------------------------------------------
-spec send_data_chunk_small(integer(), binary(), non_neg_integer(),
                            non_neg_integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
send_data_chunk_small(Type, Bin, BinSize, ChunkSize, PCB) ->
    case fill_from_queue(ChunkSize - BinSize, Type, PCB) of
        done ->
            %% 队列为空，直接发送
            send_new_packet(Type, Bin, BinSize, PCB);
        {done, MoreData, PCB1} ->
            %% 队列数据不足，合并后发送
            Data = <<Bin/binary, MoreData/binary>>,
            send_new_packet(Type, Data, byte_size(Data), PCB1);
        {next, PCB1} ->
            %% 队列中是不同类型，先发送当前数据
            PCB2 = send_new_packet(Type, Bin, BinSize, PCB1),
            send_queued_data(PCB2);
        {next, MoreData, PCB1} ->
            %% 队列中是不同类型，合并后发送当前数据
            Data = <<Bin/binary, MoreData/binary>>,
            PCB2 = send_new_packet(Type, Data, byte_size(Data), PCB1),
            send_queued_data(PCB2);
        {MoreData, NewBin, PCB1} ->
            %% 从队列获取了更多数据，还有剩余
            Data = <<Bin/binary, MoreData/binary>>,
            PCB2 = send_new_packet(Type, Data, ChunkSize, PCB1),
            %% 继续发送剩余数据
            MaxSend = max_send(PCB2),
            send_data_chunk(Type, NewBin, MaxSend - ChunkSize, PCB2)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 从队列填充数据
%%------------------------------------------------------------------------------
-spec fill_from_queue(non_neg_integer(), integer(), #aiutp_pcb{}) ->
    done |
    {done, binary(), #aiutp_pcb{}} |
    {next, #aiutp_pcb{}} |
    {next, binary(), #aiutp_pcb{}} |
    {binary(), binary(), #aiutp_pcb{}}.
fill_from_queue(RequiredSize, Type, #aiutp_pcb{outque = OutQue} = PCB) ->
    case aiutp_queue:empty(OutQue) of
        true -> done;
        %% 使用 iolist 累积，避免多次二进制拼接
        false -> fill_from_queue_loop(RequiredSize, Type, [], 0, PCB)
    end.

%% @doc 从队列填充数据（使用 iolist 累积优化）
%% AccList: 累积的二进制块列表（逆序）
%% AccSize: 累积的总大小
-spec fill_from_queue_loop(non_neg_integer(), integer(), iolist(), non_neg_integer(), #aiutp_pcb{}) ->
    done |
    {done, binary(), #aiutp_pcb{}} |
    {next, #aiutp_pcb{}} |
    {next, binary(), #aiutp_pcb{}} |
    {binary(), binary(), #aiutp_pcb{}}.
fill_from_queue_loop(RequiredSize, Type, AccList, AccSize, #aiutp_pcb{outque = OutQue} = PCB) ->
    case {aiutp_queue:empty(OutQue), AccSize} of
        {true, 0} -> done;
        {true, _} -> {done, iolist_to_binary(lists:reverse(AccList)), PCB};
        _ ->
            case aiutp_queue:front(OutQue) of
                {Type, Bin} ->
                    BinSize = byte_size(Bin),
                    case BinSize > RequiredSize of
                        true ->
                            <<More:RequiredSize/binary, Rest/binary>> = Bin,
                            %% 一次性合并所有累积的数据
                            Result = iolist_to_binary(lists:reverse([More | AccList])),
                            {Result, Rest,
                             PCB#aiutp_pcb{outque = aiutp_queue:pop_front(OutQue)}};
                        false ->
                            fill_from_queue_loop(
                                RequiredSize - BinSize, Type,
                                [Bin | AccList], AccSize + BinSize,
                                PCB#aiutp_pcb{outque = aiutp_queue:pop_front(OutQue)})
                    end;
                _ ->
                    %% 不同类型
                    case AccSize > 0 of
                        true -> {next, iolist_to_binary(lists:reverse(AccList)), PCB};
                        false -> {next, PCB}
                    end
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 创建并发送新数据包
%%------------------------------------------------------------------------------
-spec send_new_packet(integer(), binary(), non_neg_integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
send_new_packet(Type, Data, Payload,
                #aiutp_pcb{outbuf = OutBuf,
                           socket = Socket,
                           cur_window = CurWindow,
                           conn_id_send = ConnId,
                           ack_nr = AckNR,
                           seq_nr = SeqNR,
                           cur_window_packets = CurWindowPackets,
                           max_window = MaxWindow,
                           inbuf = InBuf,
                           reply_micro = ReplyMicro,
                           time = Now} = PCB) ->
    MicroNow = aiutp_util:microsecond(),
    LastRcvWin = window_size(MaxWindow, InBuf),

    %% 创建数据包
    Packet = aiutp_packet:data(SeqNR, AckNR),
    Packet1 = Packet#aiutp_packet{
        conn_id = ConnId,
        type = Type,
        payload = Data,
        wnd = LastRcvWin
    },

    %% 检查是否应该作为 MTU 探测包
    %% 条件: 包大小在 (floor, ceiling] 范围内且无在途探测
    IsMtuProbe = aiutp_mtu:should_probe(Payload, PCB),

    %% 包装并发送
    WrapPacket = #aiutp_packet_wrap{payload = Payload, packet = Packet1,
                                    is_mtu_probe = IsMtuProbe},
    WindowSize = window_size(MaxWindow, InBuf),
    {SendBytes, WrapPacket1, Content} =
        prepare_packet_for_send(MicroNow, ReplyMicro, WindowSize, AckNR, WrapPacket),

    OutBuf1 = aiutp_buffer:append(WrapPacket1, OutBuf),
    do_send(Socket, Content),

    %% 如果是 MTU 探测包，更新探测状态
    PCB1 = case IsMtuProbe of
        true ->
            PCB#aiutp_pcb{
                mtu_probe_seq = SeqNR,
                mtu_probe_size = Payload
            };
        false ->
            PCB
    end,

    PCB1#aiutp_pcb{
        cur_window_packets = CurWindowPackets + 1,
        cur_window = CurWindow + SendBytes,
        outbuf = OutBuf1,
        seq_nr = aiutp_util:bit16(SeqNR + 1),
        last_rcv_win = LastRcvWin,
        last_sent_packet = Now
    }.

%%------------------------------------------------------------------------------
%% @private
%% @doc 发送指定范围内的包（循环）
%%------------------------------------------------------------------------------
-spec send_n_packets_loop(non_neg_integer(), non_neg_integer(), non_neg_integer(),
                          integer(), #aiutp_pcb{}) ->
    {non_neg_integer(), non_neg_integer(), #aiutp_pcb{}}.
send_n_packets_loop(MinSeq, _MaxSeq, Limit, Iter, PCB)
  when Limit == 0; Iter == -1 ->
    {Limit, MinSeq, PCB};
send_n_packets_loop(MinSeq, MaxSeq, Limit, Iter,
                    #aiutp_pcb{time = Now,
                               socket = Socket,
                               max_window = MaxWindow,
                               outbuf = OutBuf,
                               cur_window = CurWindow,
                               inbuf = InBuf,
                               reply_micro = ReplyMicro,
                               ack_nr = AckNR} = PCB) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Packet = WrapPacket#aiutp_packet_wrap.packet,
    PktSeqNR = Packet#aiutp_packet.seq_nr,

    InRange = (?WRAPPING_DIFF_16(MaxSeq, PktSeqNR) > 0) andalso
              (?WRAPPING_DIFF_16(PktSeqNR, MinSeq) >= 0),

    case InRange of
        true ->
            MicroNow = aiutp_util:microsecond(),
            WindowSize = window_size(MaxWindow, InBuf),
            Next = aiutp_buffer:next(Iter, OutBuf),
            {SendBytes, WrapPacket1, Content} =
                prepare_packet_for_send(MicroNow, ReplyMicro, WindowSize, AckNR, WrapPacket),
            OutBuf1 = aiutp_buffer:replace(Iter, WrapPacket1, OutBuf),
            do_send(Socket, Content),
            send_n_packets_loop(
                PktSeqNR, MaxSeq, Limit - 1, Next,
                PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,
                              outbuf = OutBuf1,
                              last_sent_packet = Now});
        false ->
            case ?WRAPPING_DIFF_16(MinSeq, PktSeqNR) > 0 of
                true ->
                    %% 包序列号小于 MinSeq，继续查找
                    Next = aiutp_buffer:next(Iter, OutBuf),
                    send_n_packets_loop(MinSeq, MaxSeq, Limit, Next, PCB);
                false ->
                    %% 包序列号超出范围，停止
                    {Limit, MinSeq, PCB}
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 发送被跳过的数据包（精确快速重传）
%%
%% libutp 风格：只重传在指定范围内、未被 SACK 确认、且已发送过的包。
%% 这比 send_n_packets_loop 更精确，避免重传已被 SACK 确认的包。
%%
%% 筛选条件：
%% 1. 序列号在 [MinSeq, MaxSeq) 范围内
%% 2. 不在 SAckedSeqs 集合中（未被 SACK 确认）
%% 3. transmissions > 0（已经发送过）
%%------------------------------------------------------------------------------
-spec send_skipped_packets_loop(non_neg_integer(), non_neg_integer(), sets:set(),
                                non_neg_integer(), integer(), #aiutp_pcb{}) ->
    {non_neg_integer(), non_neg_integer(), #aiutp_pcb{}}.
send_skipped_packets_loop(MinSeq, _MaxSeq, _SAckedSeqs, Limit, Iter, PCB)
  when Limit == 0; Iter == -1 ->
    {Limit, MinSeq, PCB};
send_skipped_packets_loop(MinSeq, MaxSeq, SAckedSeqs, Limit, Iter,
                          #aiutp_pcb{time = Now,
                                     socket = Socket,
                                     max_window = MaxWindow,
                                     outbuf = OutBuf,
                                     cur_window = CurWindow,
                                     inbuf = InBuf,
                                     reply_micro = ReplyMicro,
                                     ack_nr = AckNR} = PCB) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Packet = WrapPacket#aiutp_packet_wrap.packet,
    PktSeqNR = Packet#aiutp_packet.seq_nr,
    Transmissions = WrapPacket#aiutp_packet_wrap.transmissions,

    %% 检查是否在序列号范围内
    InRange = (?WRAPPING_DIFF_16(MaxSeq, PktSeqNR) > 0) andalso
              (?WRAPPING_DIFF_16(PktSeqNR, MinSeq) >= 0),

    %% libutp: 只重传满足以下条件的包：
    %% 1. 在范围内
    %% 2. 已发送过 (transmissions > 0)
    %% 3. 未被 SACK 确认（不在 SAckedSeqs 中）
    ShouldResend = InRange andalso
                   (Transmissions > 0) andalso
                   (not sets:is_element(PktSeqNR, SAckedSeqs)),

    case InRange of
        true ->
            Next = aiutp_buffer:next(Iter, OutBuf),
            case ShouldResend of
                true ->
                    %% 重传这个被跳过的包
                    MicroNow = aiutp_util:microsecond(),
                    WindowSize = window_size(MaxWindow, InBuf),
                    {SendBytes, WrapPacket1, Content} =
                        prepare_packet_for_send(MicroNow, ReplyMicro, WindowSize, AckNR, WrapPacket),
                    OutBuf1 = aiutp_buffer:replace(Iter, WrapPacket1, OutBuf),
                    do_send(Socket, Content),
                    send_skipped_packets_loop(
                        PktSeqNR, MaxSeq, SAckedSeqs, Limit - 1, Next,
                        PCB#aiutp_pcb{cur_window = CurWindow + SendBytes,
                                      outbuf = OutBuf1,
                                      last_sent_packet = Now});
                false ->
                    %% 包在范围内但不需要重传（已被 SACK 确认或未发送），继续查找
                    send_skipped_packets_loop(MinSeq, MaxSeq, SAckedSeqs, Limit, Next, PCB)
            end;
        false ->
            case ?WRAPPING_DIFF_16(MinSeq, PktSeqNR) > 0 of
                true ->
                    %% 包序列号小于 MinSeq，继续查找
                    Next = aiutp_buffer:next(Iter, OutBuf),
                    send_skipped_packets_loop(MinSeq, MaxSeq, SAckedSeqs, Limit, Next, PCB);
                false ->
                    %% 包序列号超出范围，停止
                    {Limit, MinSeq, PCB}
            end
    end.

%%==============================================================================
%% 内部函数 - 包准备
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 准备数据包用于发送
%%
%% 更新时间戳、窗口大小等字段，并编码。
%%
%% @returns {SendBytes, UpdatedWrapPacket, EncodedContent}
%%------------------------------------------------------------------------------
-spec prepare_packet_for_send(integer(), integer(), non_neg_integer(),
                              non_neg_integer(), #aiutp_packet_wrap{}) ->
    {non_neg_integer(), #aiutp_packet_wrap{}, binary()}.
prepare_packet_for_send(MicroNow, ReplyMicro, WindowSize, AckNR, WrapPacket) ->
    #aiutp_packet_wrap{
        transmissions = Transmissions,
        packet = Packet,
        need_resend = NeedResend,
        payload = Payload
    } = WrapPacket,

    %% 更新包字段
    Packet1 = Packet#aiutp_packet{
        tv_usec = MicroNow,
        reply_micro = ReplyMicro,
        ack_nr = AckNR,
        wnd = WindowSize
    },
    Content = aiutp_packet:encode(Packet1),

    %% 计算发送字节数（只有新包或重传包才计入）
    SendBytes = case NeedResend orelse (Transmissions == 0) of
        true -> Payload;
        false -> 0
    end,

    WrapPacket1 = WrapPacket#aiutp_packet_wrap{
        transmissions = Transmissions + 1,
        need_resend = false,
        time_sent = MicroNow,
        packet = Packet1,
        content = Content
    },

    {SendBytes, WrapPacket1, Content}.

%%==============================================================================
%% 内部函数 - SACK 构建
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 构建 SACK 位图
%%
%% SACK 覆盖 [ack_nr + 2, ack_nr + 2 + 31] 的序列号范围。
%%------------------------------------------------------------------------------
-spec build_sack(#aiutp_pcb{}) -> binary() | undefined.
build_sack(#aiutp_pcb{ack_nr = AckNR, inbuf = InBuf}) ->
    Size = aiutp_buffer:size(InBuf),
    Size1 = erlang:min(30, Size),
    case Size1 of
        0 -> undefined;
        _ ->
            BaseSeqNR = AckNR + 2,
            Head = aiutp_buffer:head(InBuf),
            build_sack_bitmap(Size1, #{0 => 0, 1 => 0, 2 => 0, 3 => 0}, BaseSeqNR, Head, InBuf)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 构建 SACK 位图（循环）
%%------------------------------------------------------------------------------
-spec build_sack_bitmap(non_neg_integer(), map(), non_neg_integer(),
                        integer(), aiutp_buffer:aiutp_buffer()) -> binary().
build_sack_bitmap(0, Acc, _BaseSeqNR, _Iter, _InBuf) ->
    finalize_sack_bitmap(Acc);
build_sack_bitmap(_Size, Acc, _BaseSeqNR, -1, _InBuf) ->
    finalize_sack_bitmap(Acc);
build_sack_bitmap(Size, Acc, BaseSeqNR, Iter, InBuf) ->
    Packet = aiutp_buffer:data(Iter, InBuf),
    Index = aiutp_util:bit16(Packet#aiutp_packet.seq_nr - BaseSeqNR),
    BytePos = Index bsr 3,

    case BytePos > 3 of
        true ->
            %% 超出 SACK 范围
            finalize_sack_bitmap(Acc);
        false ->
            Next = aiutp_buffer:next(Iter, InBuf),
            Mask = 1 bsl (Index band 7),
            Bits = maps:get(BytePos, Acc),
            Acc1 = maps:put(BytePos, Mask bor Bits, Acc),
            build_sack_bitmap(Size - 1, Acc1, BaseSeqNR, Next, InBuf)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 将 SACK map 转换为二进制
%%------------------------------------------------------------------------------
-spec finalize_sack_bitmap(map()) -> binary().
finalize_sack_bitmap(Acc) ->
    <<(maps:get(0, Acc)), (maps:get(1, Acc)),
      (maps:get(2, Acc)), (maps:get(3, Acc))>>.

%%==============================================================================
%% 内部函数 - UDP 发送
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 发送 UDP 数据包（带重试）
%%
%% BEP-29：网络发送失败应优雅处理。协议的超时和重传机制会处理丢包情况。
%%------------------------------------------------------------------------------
-spec do_send({gen_udp:socket(), {inet:ip_address(), inet:port_number()}}, binary()) ->
    ok | {error, term()}.
do_send({Socket, Remote}, Content) ->
    do_send_with_retry(Socket, Remote, ?UDP_SEND_RETRIES, Content).

-spec do_send_with_retry(gen_udp:socket(), {inet:ip_address(), inet:port_number()},
                         non_neg_integer(), binary()) -> ok | {error, term()}.
do_send_with_retry(Socket, Remote, Count, Content) ->
    case gen_udp:send(Socket, Remote, Content) of
        ok -> ok;
        {error, Reason} = Error ->
            case Count of
                0 ->
                    %% 所有重试已耗尽
                    logger:warning("uTP UDP send failed after retries: ~p, remote: ~p",
                                   [Reason, Remote]),
                    Error;
                _ ->
                    timer:sleep(?UDP_SEND_RETRY_DELAY),
                    do_send_with_retry(Socket, Remote, Count - 1, Content)
            end
    end.
