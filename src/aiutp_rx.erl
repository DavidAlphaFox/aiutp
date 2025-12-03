%%------------------------------------------------------------------------------
%% @doc uTP 接收处理模块
%%
%% 本模块负责处理入站数据包的接收和重排序，确保数据按序交付给上层。
%%
%% == 接收流程 ==
%% ```
%% 入站数据包 → process_incoming/2 (aiutp_pcb)
%%            → handle_data_and_fin/2 (aiutp_pcb)
%%            → in/2 (本模块)
%%            → 按序到达? ─Yes→ deliver_in_order/2 → inque
%%                        └No─→ buffer_out_of_order/3 → inbuf (重排序缓冲区)
%% '''
%%
%% == 重排序机制 ==
%% uTP 使用 16 位序列号，接收方维护：
%% - ack_nr: 已确认的最后序列号（所有 <= ack_nr 的包都已收到）
%% - inbuf: 乱序包缓冲区（按序列号排序）
%% - reorder_count: 缓冲区中的包数量
%%
%% 当乱序包到达时：
%% 1. 检查序列号是否在可接受范围内
%% 2. 插入到 inbuf 中的正确位置（保持有序）
%% 3. 当缺失的包到达时，递归处理缓冲区中的后续包
%%
%% == FIN 处理 ==
%% 当收到 FIN 包（got_fin = true）时：
%% - 记录 eof_pkt = FIN 的序列号
%% - 继续接收直到 ack_nr == eof_pkt
%% - 此时设置 got_fin_reached = true，表示所有数据已收到
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @copyright (C) 2020, David Gao
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_rx).
-include("aiutp.hrl").

%%==============================================================================
%% API 导出
%%==============================================================================
-export([in/2]).

%%==============================================================================
%% 常量定义
%%==============================================================================

%% 最大重排序距离（序列号差值）
%% 超过此距离的包被认为无效或过期
-define(MAX_REORDER_DISTANCE, 16#3FFF).

%%==============================================================================
%% API 函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 处理接收到的数据包
%%
%% 根据包的序列号决定处理方式：
%% - 如果是期望的下一个包（seq_nr == ack_nr + 1），直接交付
%% - 如果是乱序包，放入重排序缓冲区
%%
%% 处理完成后设置 ida = true，表示需要发送 ACK。
%%
%% @param Packet 接收到的数据包
%% @param PCB 协议控制块
%% @returns 更新后的 PCB（ida = true）
%% @end
%%------------------------------------------------------------------------------
-spec in(#aiutp_packet{}, #aiutp_pcb{}) -> #aiutp_pcb{}.
in(#aiutp_packet{seq_nr = PktSeqNR} = Packet,
   #aiutp_pcb{ack_nr = AckNR} = PCB) ->
    ExpectedSeqNR = aiutp_util:bit16(AckNR + 1),
    PCB1 = case PktSeqNR == ExpectedSeqNR of
        true ->
            %% 按序到达，直接交付
            deliver_in_order(Packet, PCB#aiutp_pcb{ack_nr = PktSeqNR});
        false ->
            %% 乱序到达，放入重排序缓冲区
            SeqDistance = aiutp_util:bit16(PktSeqNR - ExpectedSeqNR),
            buffer_out_of_order(SeqDistance, Packet, PCB)
    end,
    %% 设置立即 ACK 标志
    PCB1#aiutp_pcb{ida = true}.

%%==============================================================================
%% 内部函数 - 按序交付
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理按序到达的数据包
%%
%% 1. 将数据推入接收队列 (inque)
%% 2. 检查是否达到 FIN（所有数据已收到）
%% 3. 检查重排序缓冲区中是否有后续包可以交付
%%------------------------------------------------------------------------------
-spec deliver_in_order(#aiutp_packet{}, #aiutp_pcb{}) -> #aiutp_pcb{}.
deliver_in_order(Packet, #aiutp_pcb{inque = InQue} = PCB) ->
    %% 将载荷推入接收队列
    InQue1 = aiutp_queue:push_back(Packet#aiutp_packet.payload, InQue),
    PCB1 = PCB#aiutp_pcb{inque = InQue1},

    %% 检查是否达到 FIN
    PCB2 = maybe_handle_fin_reached(PCB1),

    %% 尝试交付重排序缓冲区中的后续包
    maybe_deliver_buffered(PCB2).

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查是否达到 FIN 状态
%%
%% 当以下条件同时满足时，设置 got_fin_reached = true：
%% - got_fin = true（已收到 FIN 包）
%% - ack_nr == eof_pkt（FIN 之前的所有包都已收到）
%%------------------------------------------------------------------------------
-spec maybe_handle_fin_reached(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_handle_fin_reached(#aiutp_pcb{got_fin_reached = true} = PCB) ->
    %% 已经处理过 FIN
    PCB;
maybe_handle_fin_reached(#aiutp_pcb{got_fin = false} = PCB) ->
    %% 还没收到 FIN
    PCB;
maybe_handle_fin_reached(#aiutp_pcb{got_fin = true,
                                     eof_pkt = EOFPkt,
                                     ack_nr = AckNR,
                                     time = Now,
                                     rto = RTO} = PCB) ->
    case EOFPkt == AckNR of
        true ->
            %% FIN 之前的所有包都已收到
            %% 设置一个短超时，然后发送最终 ACK
            PCB1 = PCB#aiutp_pcb{
                got_fin_reached = true,
                rto_timeout = Now + erlang:min(RTO * 3, 60),
                reorder_count = 0,
                inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE)
            },
            aiutp_net:send_ack(PCB1);
        false ->
            PCB
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 尝试从重排序缓冲区交付后续包
%%
%% 检查缓冲区头部的包是否是期望的下一个包，
%% 如果是则递归交付，直到遇到缺口或缓冲区为空。
%%------------------------------------------------------------------------------
-spec maybe_deliver_buffered(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_deliver_buffered(#aiutp_pcb{reorder_count = 0} = PCB) ->
    %% 缓冲区为空
    PCB;
maybe_deliver_buffered(#aiutp_pcb{inbuf = InBuf,
                                   reorder_count = ReorderCount,
                                   ack_nr = AckNR} = PCB) ->
    Iter = aiutp_buffer:head(InBuf),
    BufferedPacket = aiutp_buffer:data(Iter, InBuf),
    ExpectedSeqNR = aiutp_util:bit16(AckNR + 1),

    case BufferedPacket#aiutp_packet.seq_nr == ExpectedSeqNR of
        true ->
            %% 缓冲区头部就是下一个期望的包
            PCB1 = PCB#aiutp_pcb{
                inbuf = aiutp_buffer:pop(InBuf),
                reorder_count = ReorderCount - 1,
                ack_nr = ExpectedSeqNR
            },
            %% 递归交付
            deliver_in_order(BufferedPacket, PCB1);
        false ->
            %% 还有缺口，停止交付
            PCB
    end.

%%==============================================================================
%% 内部函数 - 乱序缓冲
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理乱序到达的数据包
%%
%% 验证包是否有效，然后插入到重排序缓冲区。
%%
%% 无效情况：
%% - 已收到 FIN 且包序列号超过 FIN
%% - 序列号距离超过最大重排序距离
%%------------------------------------------------------------------------------
-spec buffer_out_of_order(non_neg_integer(), #aiutp_packet{}, #aiutp_pcb{}) ->
    #aiutp_pcb{}.
buffer_out_of_order(SeqDistance, _, PCB) when SeqDistance > ?MAX_REORDER_DISTANCE ->
    %% 序列号距离过大，丢弃
    PCB;
buffer_out_of_order(_, #aiutp_packet{seq_nr = PktSeqNR},
                    #aiutp_pcb{got_fin = true, eof_pkt = EOFPkt} = PCB)
  when ?WRAPPING_DIFF_16(EOFPkt, PktSeqNR) < 0 ->
    %% 包序列号超过 FIN，丢弃
    PCB;
buffer_out_of_order(SeqDistance, Packet, #aiutp_pcb{inbuf = InBuf, reorder_count = ReorderCount} = PCB) ->
    %% 性能优化：利用序列号距离计算插入位置
    %% 如果缓冲区较小，可以直接用二分查找或利用距离定位
    case ReorderCount of
        0 ->
            %% 缓冲区为空，直接追加
            PCB#aiutp_pcb{
                inbuf = aiutp_buffer:append(Packet, InBuf),
                reorder_count = 1
            };
        _ when ReorderCount < 4 ->
            %% 缓冲区很小，线性遍历仍然高效
            Iter = aiutp_buffer:head(InBuf),
            insert_into_reorder_buffer(Packet, Iter, -1, PCB);
        _ ->
            %% 缓冲区较大时，使用优化的插入策略
            %% 利用 SeqDistance 进行启发式插入
            insert_with_hint(SeqDistance, Packet, PCB)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 使用序列号距离作为提示进行优化插入
%%
%% 根据 SeqDistance 决定从头部还是尾部开始搜索：
%% - 较小的距离倾向于靠近头部
%% - 较大的距离倾向于靠近尾部
%% 这减少了平均遍历次数。
%%------------------------------------------------------------------------------
-spec insert_with_hint(non_neg_integer(), #aiutp_packet{}, #aiutp_pcb{}) -> #aiutp_pcb{}.
insert_with_hint(_SeqDistance, Packet,
                 #aiutp_pcb{inbuf = InBuf, reorder_count = ReorderCount} = PCB) ->
    %% 启发式：如果距离小于缓冲区大小的一半，从头部搜索
    %% 否则从尾部搜索（需要反向遍历）
    %% 当前 aiutp_buffer 不支持反向遍历，但我们可以检查尾部
    Tail = aiutp_buffer:tail(InBuf),
    case Tail of
        -1 ->
            %% 空缓冲区
            PCB#aiutp_pcb{
                inbuf = aiutp_buffer:append(Packet, InBuf),
                reorder_count = ReorderCount + 1
            };
        _ ->
            %% 检查是否应该追加到尾部（常见情况）
            TailPacket = aiutp_buffer:data(Tail, InBuf),
            TailSeqNR = TailPacket#aiutp_packet.seq_nr,
            PacketSeqNR = Packet#aiutp_packet.seq_nr,

            case ?WRAPPING_DIFF_16(PacketSeqNR, TailSeqNR) of
                Diff when Diff > 0 ->
                    %% 新包序列号大于尾部，追加到尾部（O(1)）
                    PCB#aiutp_pcb{
                        inbuf = aiutp_buffer:append(Packet, InBuf),
                        reorder_count = ReorderCount + 1
                    };
                0 ->
                    %% 重复包，丢弃
                    PCB;
                _ ->
                    %% 需要插入到中间，根据距离决定搜索方向
                    %% 由于链表只支持正向遍历，仍然从头部开始
                    %% 但对于大多数乱序场景，包通常追加到尾部
                    Iter = aiutp_buffer:head(InBuf),
                    insert_into_reorder_buffer(Packet, Iter, -1, PCB)
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 将包插入到重排序缓冲区（保持有序）
%%
%% 遍历缓冲区找到正确的插入位置：
%% - 如果找到相同序列号的包，丢弃（重复）
%% - 如果找到更大序列号的包，在其前面插入
%% - 如果遍历到末尾，追加到末尾
%%------------------------------------------------------------------------------
-spec insert_into_reorder_buffer(#aiutp_packet{}, integer(), integer(), #aiutp_pcb{}) ->
    #aiutp_pcb{}.
insert_into_reorder_buffer(Packet, -1, _Prev,
                           #aiutp_pcb{inbuf = InBuf, reorder_count = ReorderCount} = PCB) ->
    %% 遍历到末尾，追加
    PCB#aiutp_pcb{
        inbuf = aiutp_buffer:append(Packet, InBuf),
        reorder_count = ReorderCount + 1
    };
insert_into_reorder_buffer(Packet, Iter, Prev,
                           #aiutp_pcb{inbuf = InBuf, reorder_count = ReorderCount} = PCB) ->
    BufferedPacket = aiutp_buffer:data(Iter, InBuf),
    BufferedSeqNR = BufferedPacket#aiutp_packet.seq_nr,
    PacketSeqNR = Packet#aiutp_packet.seq_nr,

    if
        BufferedSeqNR == PacketSeqNR ->
            %% 重复包，丢弃
            PCB;
        ?WRAPPING_DIFF_16(BufferedSeqNR, PacketSeqNR) > 0 ->
            %% 找到更大序列号的包，在其前面插入
            PCB#aiutp_pcb{
                inbuf = aiutp_buffer:insert(Prev, Packet, InBuf),
                reorder_count = ReorderCount + 1
            };
        true ->
            %% 继续遍历
            Next = aiutp_buffer:next(Iter, InBuf),
            insert_into_reorder_buffer(Packet, Next, Iter, PCB)
    end.
