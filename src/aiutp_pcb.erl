%%------------------------------------------------------------------------------
%% @doc uTP 连接的协议控制块
%%
%% 本模块管理 uTP 连接的核心状态和操作，包括连接建立、数据传输和连接关闭。
%%
%% PCB 维护所有连接状态，包括：
%% - 连接标识符（接收/发送 ID）
%% - 序列号和确认号
%% - 发送/接收缓冲区
%% - RTT/RTO 估算值
%% - 拥塞窗口参数
%%
%% 相关模块：
%% - aiutp_pcb_cc: 拥塞控制（LEDBAT 算法）
%% - aiutp_pcb_timeout: 超时处理和重传
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb).
-include("aiutp.hrl").

-export([state/1,
         process_incoming/2,
         check_timeouts/1,
         write/2,
         close/1,
         read/1,
         connect/2,
         accept/2,
         closed/1,
         flush/1]).

%% socket 引用的类型定义
-type socket_ref() :: {gen_udp:socket(), {inet:ip_address(), inet:port_number()}}.

%%------------------------------------------------------------------------------
%% @doc 创建新的协议控制块
%%
%% 使用默认值初始化新的 uTP 连接 PCB。
%%
%% @param ConnIdRecv 接收连接 ID
%% @param ConnIdSend 发送连接 ID
%% @param Socket UDP socket 引用
%% @returns 新的 #aiutp_pcb{} 记录
%% @end
%%------------------------------------------------------------------------------
-spec new(integer(), integer(), socket_ref()) -> #aiutp_pcb{}.
new(ConnIdRecv, ConnIdSend, Socket) ->
    CurMilli = aiutp_util:millisecond(),
    PCB = #aiutp_pcb{
        time = CurMilli,
        state = ?CS_IDLE,
        socket = Socket,
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
        max_window = ?PACKET_SIZE,
        inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        outbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        inque = aiutp_queue:new(),
        outque = aiutp_queue:new()
    },
    %% 初始化 MTU 发现状态
    aiutp_mtu:reset(PCB).

%%------------------------------------------------------------------------------
%% @doc 获取当前连接状态
%% @end
%%------------------------------------------------------------------------------
-spec state(#aiutp_pcb{}) -> atom().
state(#aiutp_pcb{state = State}) -> State.

%%------------------------------------------------------------------------------
%% @doc 检查连接是否已关闭并返回原因
%%
%% 根据 libutp 实现，连接关闭的判断逻辑：
%% - CS_RESET: 连接被对端强制重置
%% - CS_DESTROY: 连接进入销毁状态
%%   - fin_sent_acked = true: 我们的 FIN 已被确认 -> normal
%%   - got_fin_reached = true: 对端 FIN 及之前的包都已收到 -> normal
%%   - 无 FIN 交换: 超时或错误导致的销毁 -> timeout
%%
%% 注意: got_fin_reached=true 在非 CS_DESTROY 状态下不触发关闭。
%% 这表示对端已发送 FIN（半关闭），但本地连接仍然有效，
%% 需要上层调用 close() 来完成关闭流程。
%%
%% @returns {closed, Reason} | not_closed
%% 其中 Reason: normal | reset | timeout
%% @end
%%------------------------------------------------------------------------------
-spec closed(#aiutp_pcb{}) -> {closed, normal | reset | timeout} | not_closed.
%% 连接被重置
closed(#aiutp_pcb{state = ?CS_RESET}) ->
    {closed, reset};
%% 连接进入销毁状态
closed(#aiutp_pcb{state = ?CS_DESTROY,
                  fin_sent_acked = FinSentAcked,
                  got_fin_reached = GotFinReached}) ->
    if
        %% 我们的 FIN 已被确认，正常关闭
        FinSentAcked -> {closed, normal};
        %% 对端 FIN 及之前的包都已收到，正常关闭
        GotFinReached -> {closed, normal};
        %% 无 FIN 交换就进入 DESTROY，说明是超时
        true -> {closed, timeout}
    end;
%% 其他状态：连接未关闭
closed(_) ->
    not_closed.

%%------------------------------------------------------------------------------
%% @doc 处理入站数据包
%%
%% 数据包处理的入口点。处理完成后调度 ACK 发送。
%% 这是 aiutp_channel 收到数据包时调用的主入口。
%%
%% 处理流程：
%% 1. dispatch_by_type - 按包类型路由（RESET、SYN 或其他）
%% 2. validate_and_init - 检查序列号范围
%% 3. handle_duplicate_acks - 检测重复 ACK 触发快速重传
%% 4. process_ack_and_sack - 处理 ACK/SACK，拥塞控制
%% 5. update_connection_state - 状态机转换
%% 6. handle_data_and_fin - 处理数据载荷和 FIN
%%
%% @param {Packet, Timestamp} 入站包及接收时间戳
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec process_incoming({#aiutp_packet{}, integer()}, #aiutp_pcb{}) -> #aiutp_pcb{}.
process_incoming({Packet, TS}, PCB) ->
    aiutp_net:schedule_ack(
        dispatch_by_type(Packet#aiutp_packet.type, Packet,
                         PCB#aiutp_pcb{recv_time = TS})).

%%==============================================================================
%% 按类型分发数据包
%%==============================================================================

%% @private 按类型分发数据包 - 首先过滤已销毁/重置状态
%% 在 DESTROY 或 RESET 状态下收到的包将被静默丢弃
dispatch_by_type(_, _, #aiutp_pcb{state = State} = PCB)
  when (State == ?CS_DESTROY);
       (State == ?CS_RESET) ->
    PCB;

%% @private 处理 ST_RESET 包 - 立即终止连接
%% 根据 BEP-29：RESET 导致连接立即断开，无需响应
dispatch_by_type(?ST_RESET,
                 #aiutp_packet{conn_id = ConnId},
                 #aiutp_pcb{conn_id_send = ConnIdSend,
                            conn_id_recv = ConnIdRecv,
                            close_requested = CloseRequested} = PCB) ->
    if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
        if CloseRequested == true -> PCB#aiutp_pcb{state = ?CS_DESTROY};
           true -> PCB#aiutp_pcb{state = ?CS_RESET}
        end;
       true -> PCB
    end;

%% @private 处理 ST_SYN 包 - 新的入站连接
%% 状态转换：IDLE -> SYN_RECV，并发送 SYN-ACK
dispatch_by_type(?ST_SYN,
                 #aiutp_packet{seq_nr = AckNR},
                 #aiutp_pcb{state = ?CS_IDLE} = PCB) ->
    SeqNR = aiutp_util:bit16_random(),
    PCB0 = PCB#aiutp_pcb{
        state = ?CS_SYN_RECV,
        ack_nr = AckNR,
        seq_nr = SeqNR,
        fast_resend_seq_nr = SeqNR,
        last_got_packet = aiutp_util:millisecond()
    },
    aiutp_net:send_ack(PCB0);

%% @private 处理重复的 SYN - 重传我们的 SYN-ACK
dispatch_by_type(?ST_SYN,
                 #aiutp_packet{seq_nr = AckNR},
                 #aiutp_pcb{state = ?CS_SYN_RECV, ack_nr = AckNR} = PCB) ->
    PCB0 = PCB#aiutp_pcb{last_got_packet = aiutp_util:millisecond()},
    aiutp_net:send_ack(PCB0);

%% @private 处理其他所有包类型（ST_DATA、ST_STATE、ST_FIN）
%% 在进入完整处理流程前验证 ACK 号
dispatch_by_type(_,
                 #aiutp_packet{type = PktType, ack_nr = PktAckNR} = Packet,
                 #aiutp_pcb{state = State, seq_nr = SeqNR,
                            cur_window_packets = CurWindowPackets} = PCB) ->
    %% 计算允许的 ACK 范围用于验证
    CurrWindow = erlang:max(CurWindowPackets + ?ACK_NR_ALLOWED_WINDOW, ?ACK_NR_ALLOWED_WINDOW),
    MaxSeqNR = aiutp_util:bit16(SeqNR - 1),
    MinSeqNR = aiutp_util:bit16(SeqNR - 1 - CurrWindow),

    %% 验证 ACK 号以检测伪造/恶意数据包
    if ((PktType /= ?ST_SYN) or (State /= ?CS_SYN_RECV)) and
       (?WRAPPING_DIFF_16(MaxSeqNR, PktAckNR) < 0) or
       (?WRAPPING_DIFF_16(PktAckNR, MinSeqNR) < 0) ->
        %% 无效 ACK - 忽略数据包（可能是地址伪造或攻击）
        PCB;
       true ->
        validate_and_init(Packet, PCB)
    end.

%%==============================================================================
%% 数据包处理流程
%%
%% 分发后，数据包经过以下阶段：
%% 1. validate_and_init     - 初始化计时，检查序列号范围
%% 2. handle_duplicate_acks - 检测重复 ACK 触发快速重传
%% 3. process_ack_and_sack  - 处理 ACK/SACK，拥塞控制
%% 4. update_connection_state - 处理状态机转换
%% 5. handle_data_and_fin   - 处理数据载荷和 FIN
%%==============================================================================

%% @private 阶段 1：验证序列号并初始化包处理
%%
%% - 对于 SYN_SENT 状态：从入站包初始化 ack_nr
%% - 检查包序列号是否在可接受的重排序范围内
%% - 丢弃序列号偏差过大的包
%% - 为很旧的包调度立即 ACK（可能是重传）
validate_and_init(#aiutp_packet{type = PktType, seq_nr = PktSeqNR} = Packet,
                  #aiutp_pcb{state = State} = PCB) ->
    Now = aiutp_util:millisecond(),
    PCB0 =
        if State == ?CS_SYN_SENT ->
            %% 收到 SYN-ACK：初始化 ack_nr
            PCB#aiutp_pcb{
                ack_nr = aiutp_util:bit16(PktSeqNR - 1),
                last_got_packet = Now,
                time = Now
            };
           true ->
            PCB#aiutp_pcb{last_got_packet = Now, time = Now}
        end,

    %% 检查包是否在重排序缓冲区范围内
    NextPktAckNR = aiutp_util:bit16(PCB0#aiutp_pcb.ack_nr + 1),
    SeqDistance = aiutp_util:bit16(PktSeqNR - NextPktAckNR),

    if SeqDistance >= ?OUTGOING_BUFFER_MAX_SIZE ->
        %% 包序列号偏差过大
        if (SeqDistance >= (?SEQ_NR_MASK + 1 - ?OUTGOING_BUFFER_MAX_SIZE)) and
           (PktType /= ?ST_STATE) ->
            %% 旧包，调度立即 ACK
            PCB0#aiutp_pcb{ida = true};
           true -> PCB0
        end;
       true ->
        handle_duplicate_acks(Packet, PCB0)
    end.

%% @private 阶段 2：处理重复 ACK 检测以触发快速重传
%%
%% 根据 BEP-29：当收到 DUPLICATE_ACKS_BEFORE_RESEND (3) 个相同序列号的
%% 重复 ACK 时，触发最旧未确认包的快速重传（与 TCP 一致）。
%%
%% 重复 ACK 是指与之前具有相同 ack_nr 的 ST_STATE 包，
%% 表明接收方仍在等待某个特定包。
handle_duplicate_acks(#aiutp_packet{type = PktType, ack_nr = PktAckNR} = Packet,
                      #aiutp_pcb{cur_window_packets = CurWindowPackets,
                                 duplicate_ack = DuplicateAck,
                                 seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0 ->
    Seq = aiutp_util:bit16(SeqNR - CurWindowPackets - 1),
    if (PktAckNR == Seq) and (PktType == ?ST_STATE) ->
        %% 收到重复 ACK
        if DuplicateAck + 1 == ?DUPLICATE_ACKS_BEFORE_RESEND ->
            %% 触发最旧未确认包的快速重传
            PCB0 = aiutp_net:send_packet(
                       aiutp_buffer:head(PCB#aiutp_pcb.outbuf),
                       PCB#aiutp_pcb{duplicate_ack = 0}),
            process_ack_and_sack(Packet, PCB0);
           true ->
            process_ack_and_sack(Packet, PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1})
        end;
       true ->
        %% 不是重复 ACK，重置计数器
        process_ack_and_sack(Packet, PCB#aiutp_pcb{duplicate_ack = 0})
    end;
handle_duplicate_acks(Packet, PCB) ->
    process_ack_and_sack(Packet, PCB).

%% @private 阶段 3：处理 ACK、SACK 并应用拥塞控制
%%
%% 这是核心 ACK 处理阶段，分为以下子步骤：
%% 1. extract_and_process_acks - 提取已确认的包并更新跳过计数
%% 2. apply_congestion_control - 应用 LEDBAT 拥塞控制
%% 3. update_ack_state - 更新 ACK 相关状态（零窗口、连接状态等）
%% 4. process_rtt_from_acks - 从已确认包计算 RTT
%% 5. handle_fast_retransmit - 处理快速重传逻辑
%%
%% 关于快速重传的详细场景说明，请参考：
%% docs/development/fast-retransmit-scenarios.md
process_ack_and_sack(Packet, PCB) ->
    %% 步骤 1: 提取已确认的包并更新跳过计数
    {AckedPackets, SAckedPackets, SAckedSeqs, PCB1} = extract_and_process_acks(Packet, PCB),

    %% 步骤 2: 应用 LEDBAT 拥塞控制
    PCB2 = apply_congestion_control(Packet, AckedPackets, SAckedPackets, PCB1),

    %% 步骤 3: 更新 ACK 相关状态
    PCB3 = update_ack_state(Packet, PCB2),

    %% 步骤 4: 从已确认包计算 RTT
    PCB4 = process_rtt_from_acks(AckedPackets, PCB3),

    %% 步骤 5: 处理快速重传（流水线发送、快速超时恢复、SACK 触发重传）
    PCB5 = handle_fast_retransmit(SAckedPackets, SAckedSeqs, PCB4),

    update_connection_state(Packet, PCB5).

%%------------------------------------------------------------------------------
%% process_ack_and_sack 子函数
%%------------------------------------------------------------------------------

%% @private 步骤 1: 提取已确认的包并更新 SACK 跳过计数
%%
%% 处理流程：
%% 1. 从 outbuf 提取被累积确认的包（seq_nr <= ack_nr）
%% 2. 从 outbuf 提取被选择性确认的包（在 SACK 位图中）
%% 3. 解析 SACK 扩展获取序列号列表
%% 4. 更新被跳过包的 skip_count，达到阈值时标记 need_resend=true
%%
%% SACK 跳过计数机制（参考 libutp）：
%% - 当包 A 的序列号小于 SACK 中的序列号，且 A 不在 SACK 中时，A 被"跳过"
%% - 每次被跳过时 skip_count++
%% - 当 skip_count >= DUPLICATE_ACKS_BEFORE_RESEND (3) 时触发快速重传
%%
%% 示例：发送 10,11,12,13，包 11 丢失，收到 SACK={12,13}
%%       包 11 的 skip_count 增加，多次后触发快速重传
-spec extract_and_process_acks(#aiutp_packet{}, #aiutp_pcb{}) ->
    {[#aiutp_packet_wrap{}], [#aiutp_packet_wrap{}], [non_neg_integer()], #aiutp_pcb{}}.
extract_and_process_acks(#aiutp_packet{ack_nr = PktAckNR, extension = Exts} = Packet, PCB) ->
    %% 从发送缓冲区提取已确认的包
    {AckedPackets, SAckedPackets, PCB0} = aiutp_tx:extract_acked(Packet, PCB),

    %% BEP-29：解析 SACK 扩展获取被选择性确认的序列号列表
    %% SACK 覆盖范围从 ack_nr + 2 开始（ack_nr + 1 是期望的下一个包）
    SAckedSeqs = aiutp_tx:parse_sack_extension(Exts, aiutp_util:bit16(PktAckNR + 2)),

    %% 更新被 SACK 跳过的包的跳过计数
    %% 这可能导致某些包被标记为 need_resend=true
    {_SkippedCount, PCB1} = aiutp_tx:update_skip_counts(SAckedSeqs, PCB0),

    %% 处理 MTU 探测包确认
    %% 如果探测包被确认，更新 MTU 发现状态
    PCB2 = handle_mtu_probe_acks(AckedPackets ++ SAckedPackets, PCB1),

    {AckedPackets, SAckedPackets, SAckedSeqs, PCB2}.

%% @private 步骤 2: 应用 LEDBAT 拥塞控制
%%
%% LEDBAT (Low Extra Delay Background Transport) 算法：
%% - 目标延迟: 100ms
%% - 当延迟低于目标时增加窗口
%% - 当延迟高于目标时减少窗口
%% - 包含慢启动和拥塞避免两个阶段
%%
%% 时钟漂移惩罚：
%% - 检测对端时钟变慢超过 200ms/5s 时应用惩罚
%% - 防止对端通过减慢时钟"作弊"获取更多带宽
-spec apply_congestion_control(#aiutp_packet{}, [#aiutp_packet_wrap{}],
                               [#aiutp_packet_wrap{}], #aiutp_pcb{}) -> #aiutp_pcb{}.
apply_congestion_control(Packet, AckedPackets, SAckedPackets,
                         #aiutp_pcb{time = Now, recv_time = RecvTime} = PCB) ->
    %% 计算已确认字节总数和最小 RTT
    {AckedBytes, MinRTT} = aiutp_pcb_cc:caculate_acked_bytes(
                               {0, ?RTT_MAX}, RecvTime, AckedPackets, SAckedPackets),

    %% 根据时间戳差计算实际单向延迟
    {ActualDelay, PCB1} = aiutp_rtt:caculate_delay(Now, RecvTime, Packet, PCB),

    %% 更新延迟历史记录用于 LEDBAT
    OurHist = PCB1#aiutp_pcb.our_hist,
    OurHistValue = aiutp_delay:value(OurHist),
    OurHist0 =
        if OurHistValue > MinRTT ->
            aiutp_delay:shift(aiutp_util:bit32(OurHistValue - MinRTT), OurHist);
           true -> OurHist
        end,

    %% 如果收到有效 ACK 则应用 LEDBAT 拥塞控制
    if (ActualDelay /= 0) and (AckedBytes > 0) ->
        aiutp_pcb_cc:cc_control(Now, AckedBytes, MinRTT,
                                PCB1#aiutp_pcb{our_hist = OurHist0});
       true ->
        PCB1#aiutp_pcb{our_hist = OurHist0}
    end.

%% @private 步骤 3: 更新 ACK 相关状态
%%
%% 处理：
%% - 零窗口探测定时器（对端窗口为 0 时设置 15 秒探测）
%% - 连接状态转换（SYN_RECV -> CONNECTED, SYN_SENT -> CONNECTED）
%% - FIN 确认状态（当所有包包括 FIN 都被确认时）
%% - 更新 max_window_user（对端通告的接收窗口）
%% - 更新 fast_resend_seq_nr（防止重复快速重传）
-spec update_ack_state(#aiutp_packet{}, #aiutp_pcb{}) -> #aiutp_pcb{}.
update_ack_state(#aiutp_packet{type = PktType, ack_nr = PktAckNR,
                               wnd = PktMaxWindowUser},
                 #aiutp_pcb{state = State,
                            time = Now,
                            fast_resend_seq_nr = FastResendSeqNR,
                            zerowindow_time = ZeroWindowTime,
                            fin_sent = FinSent,
                            close_requested = CloseRequested,
                            fin_sent_acked = FinSentAcked,
                            cur_window_packets = CurWindowPackets} = PCB) ->
    %% 处理零窗口 - 当对端通告窗口为 0 时设置探测定时器
    ZeroWindowTime0 =
        if PktMaxWindowUser == 0 -> Now + 15000;
           true -> ZeroWindowTime
        end,

    %% 连接建立的状态转换
    State0 =
        if (PktType == ?ST_DATA) and (State == ?CS_SYN_RECV) -> ?CS_CONNECTED;
           true -> State
        end,

    %% 检查 FIN 是否被确认和最终状态转换
    {State1, FinSentAcked0} =
        if (PktType == ?ST_STATE) and (State0 == ?CS_SYN_SENT) ->
            %% 收到 SYN-ACK，连接已建立
            {?CS_CONNECTED, false};
           (FinSent == true) and (CurWindowPackets == 0) ->
            %% 包括 FIN 在内的所有包都已被确认
            if CloseRequested -> {?CS_DESTROY, true};
               true -> {State0, true}
            end;
           true -> {State0, FinSentAcked}
        end,

    %% 更新 fast_resend_seq_nr 防止重复快速重传
    %% 当收到更新的 ACK 时，更新此值以跳过已确认的序列号范围
    PktAckNR0 = aiutp_util:bit16(PktAckNR + 1),
    FastResendSeqNR0 =
        if ?WRAPPING_DIFF_16(FastResendSeqNR, PktAckNR0) < 0 -> PktAckNR0;
           true -> FastResendSeqNR
        end,

    PCB#aiutp_pcb{
        max_window_user = PktMaxWindowUser,
        state = State1,
        fin_sent_acked = FinSentAcked0,
        fast_resend_seq_nr = FastResendSeqNR0,
        zerowindow_time = ZeroWindowTime0
    }.

%% @private 步骤 4: 从已确认包计算 RTT 并更新 RTO
%%
%% RTT 计算使用 Karn 算法：
%% - 只使用首次传输的包计算 RTT（重传的包不用于 RTT 计算）
%% - RTT 平滑：SRTT = (1-α)*SRTT + α*R，α = 1/8
%% - RTT 变差：RTTVAR = (1-β)*RTTVAR + β*|SRTT-R|，β = 1/4
%% - RTO = SRTT + 4*RTTVAR，限制在 [600ms, 6000ms]
%%
%% 同时更新 cur_window（从确认的包中减去已确认的字节）
-spec process_rtt_from_acks([#aiutp_packet_wrap{}], #aiutp_pcb{}) -> #aiutp_pcb{}.
process_rtt_from_acks(AckedPackets,
                      #aiutp_pcb{time = Now, recv_time = RecvTime,
                                 cur_window = CurWindow,
                                 rtt = RTT, rto = RTO,
                                 rtt_var = RTTVar,
                                 rtt_hist = RTTHist} = PCB) ->
    %% 使用 foldr 处理已确认的包（Karn 算法）
    {_, CurWindow0, RTT0, RTO0, RTTVar0, RTTHist0} =
        lists:foldr(
            fun(I, AccPCB) -> aiutp_pcb_cc:ack_packet(RecvTime, I, AccPCB) end,
            {Now, CurWindow, RTT, RTO, RTTVar, RTTHist},
            AckedPackets),

    Now0 = aiutp_util:millisecond(),
    PCB#aiutp_pcb{
        cur_window = CurWindow0,
        rtt = RTT0,
        rtt_var = RTTVar0,
        rtt_hist = RTTHist0,
        rto = RTO0,
        retransmit_count = 0,
        retransmit_timeout = RTO0,
        rto_timeout = RTO0 + Now0
    }.

%% @private 步骤 5: 处理快速重传
%%
%% 快速重传触发场景：
%%
%% 1. 流水线发送：当只有一个包在传输中时，立即发送下一个包以保持流水线
%%
%% 2. 快速超时恢复（fast_timeout 模式）：
%%    - 当 RTO 超时后进入此模式
%%    - 每收到一个 ACK 就重传一个包
%%    - 直到最旧未确认包改变（说明丢失的包已恢复）
%%
%%    示例：发送 10,11,12，包 10 丢失后 RTO 超时
%%          进入 fast_timeout，每次收到 ACK 重传一个包
%%          当包 10 被确认后退出 fast_timeout
%%
%% 3. SACK 触发的批量重传：
%%    - 当 SACK 表明有包被跳过超过 3 次时
%%    - 最多重传 4 个包
%%    - 触发窗口衰减（max_window *= 0.5）
%%
%%    示例：发送 10-20，包 11,13,15 丢失
%%          收到 SACK={12,14,16,17,18,19,20}
%%          检测到跨度超过阈值，重传 11,13,15（最多 4 个）
-spec handle_fast_retransmit([#aiutp_packet_wrap{}], [non_neg_integer()], #aiutp_pcb{}) ->
    #aiutp_pcb{}.
handle_fast_retransmit(SAckedPackets, _SAckedSeqs,
                       #aiutp_pcb{cur_window_packets = CurWindowPackets,
                                  fast_timeout = FastTimeout,
                                  fast_resend_seq_nr = FastResendSeqNR,
                                  seq_nr = SeqNR,
                                  outbuf = OutBuf,
                                  recv_time = RecvTime} = PCB) ->
    %% 场景 1: 流水线发送 - 保持传输流畅
    PCB1 =
        if CurWindowPackets == 1 ->
            aiutp_net:send_packet(aiutp_buffer:head(OutBuf), PCB);
           true -> PCB
        end,

    %% 场景 2: 快速超时恢复
    %% 在 RTO 超时后进入此模式，每收到 ACK 重传一个包
    PCB2 =
        if FastTimeout ->
            OldestUnackedSeq = aiutp_util:bit16(SeqNR - CurWindowPackets),
            if OldestUnackedSeq /= FastResendSeqNR ->
                %% 最旧未确认包已改变，退出快速超时模式
                PCB1#aiutp_pcb{fast_timeout = false};
               true ->
                %% 继续重传下一个包
                aiutp_net:send_packet(
                    aiutp_buffer:head(PCB1#aiutp_pcb.outbuf),
                    PCB1#aiutp_pcb{fast_resend_seq_nr = aiutp_util:bit16(FastResendSeqNR + 1)})
            end;
           true -> PCB1
        end,

    %% 场景 3: SACK 触发的批量快速重传
    aiutp_pcb_cc:selective_ack_packet(SAckedPackets, RecvTime, PCB2).

%% @private 阶段 4：更新连接状态机
%%
%% 处理：
%% - 当缓冲区空间释放时从 CONNECTED_FULL 转回 CONNECTED
%% - 过滤 ST_STATE 包（不携带数据）
%% - 将数据包路由到最终处理阶段
update_connection_state(#aiutp_packet{type = PktType} = Packet,
                        #aiutp_pcb{state = State} = PCB) ->
    %% 检查发送缓冲区是否不再满
    {ISFull, PCB0} = aiutp_net:is_full(-1, PCB),
    PCB1 =
        if (ISFull == false) and (State == ?CS_CONNECTED_FULL) ->
            PCB0#aiutp_pcb{state = ?CS_CONNECTED};
           true -> PCB0
        end,

    %% ST_STATE 包（纯 ACK）不携带数据，在此停止处理
    if PktType == ?ST_STATE -> PCB1;
       (State /= ?CS_CONNECTED) and (State /= ?CS_CONNECTED_FULL) -> PCB1;
       true -> handle_data_and_fin(Packet, PCB1)
    end.

%% @private 阶段 5：处理数据载荷和 FIN 包
%%
%% 对于 ST_FIN 包：
%% - 记录 got_fin = true 和 eof_pkt = seq_nr
%% - 继续接收直到 ack_nr 达到 eof_pkt
%% - 这确保 FIN 之前的所有数据都被接收
%%
%% 对于 ST_DATA 包：
%% - 传递给 aiutp_rx 进行重组和交付
handle_data_and_fin(#aiutp_packet{type = PktType, seq_nr = PktSeqNR} = Packet,
                    #aiutp_pcb{got_fin = GotFin} = PCB) ->
    PCB0 =
        if (PktType == ?ST_FIN) and (GotFin == false) ->
            %% 记录已收到 FIN - 但还不关闭
            %% 必须等待直到 eof_pkt 之前的所有包都到达
            PCB#aiutp_pcb{got_fin = true, eof_pkt = PktSeqNR};
           true -> PCB
        end,
    %% 处理数据（或 FIN 的空载荷）
    aiutp_rx:in(Packet, PCB0).

%%------------------------------------------------------------------------------
%% @private MTU 探测包 ACK 处理
%%
%% 遍历已确认的包列表，检查是否有 MTU 探测包被确认。
%% 如果探测包被确认，调用 aiutp_mtu:on_probe_acked/2 更新 MTU 发现状态。
%%------------------------------------------------------------------------------
-spec handle_mtu_probe_acks([#aiutp_packet_wrap{}], #aiutp_pcb{}) -> #aiutp_pcb{}.
handle_mtu_probe_acks([], PCB) ->
    PCB;
handle_mtu_probe_acks([#aiutp_packet_wrap{is_mtu_probe = true, packet = Packet} | Rest], PCB) ->
    SeqNR = Packet#aiutp_packet.seq_nr,
    PCB1 = aiutp_mtu:on_probe_acked(SeqNR, PCB),
    handle_mtu_probe_acks(Rest, PCB1);
handle_mtu_probe_acks([_ | Rest], PCB) ->
    handle_mtu_probe_acks(Rest, PCB).

%%------------------------------------------------------------------------------
%% @doc 检查和处理超时
%%
%% 委托给 aiutp_pcb_timeout 模块。
%% @end
%%------------------------------------------------------------------------------
-spec check_timeouts(#aiutp_pcb{}) -> #aiutp_pcb{}.
check_timeouts(PCB) ->
    aiutp_pcb_timeout:check_timeouts(PCB).

%%------------------------------------------------------------------------------
%% @doc 向连接写入数据
%%
%% @param Data 要发送的二进制数据
%% @param PCB 协议控制块
%% @returns {ok, PCB} | {{error, Reason}, PCB}
%% @end
%%------------------------------------------------------------------------------
-spec write(binary(), #aiutp_pcb{}) -> {ok | {error, atom()}, #aiutp_pcb{}}.
write(_, #aiutp_pcb{state = State} = PCB)
  when (State /= ?CS_CONNECTED),
       (State /= ?CS_CONNECTED_FULL) ->
    {{error, not_connected}, PCB};
write(_, #aiutp_pcb{fin_sent = FinSent} = PCB)
  when FinSent == true ->
    {{error, closed}, PCB};
write(Data, PCB) ->
    PCB0 = aiutp_tx:in(Data, PCB#aiutp_pcb{time = aiutp_util:millisecond()}),
    {ok, PCB0}.

%%------------------------------------------------------------------------------
%% @doc 关闭连接
%%
%% 通过发送 FIN 包启动优雅关闭。
%%
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec close(#aiutp_pcb{}) -> #aiutp_pcb{}.
close(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_UNINITIALIZED;
       State == ?CS_IDLE;
       State == ?CS_DESTROY ->
    PCB#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_SENT ->
    %% 连接建立中被取消，发送 RESET 通知对端
    PCB0 = aiutp_net:send_reset(PCB),
    PCB0#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
    %% 接受连接过程中被取消，发送 RESET 通知对端
    PCB0 = aiutp_net:send_reset(PCB),
    PCB0#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{fin_sent_acked = FinSentAcked, fin_sent = FinSent} = PCB) ->
    PCB0 = PCB#aiutp_pcb{close_requested = true},
    if FinSent == false ->
        aiutp_net:send_fin(PCB0#aiutp_pcb{fin_sent = true});
       FinSentAcked == true ->
        PCB0#aiutp_pcb{state = ?CS_DESTROY};
       true -> PCB0
    end.

%%------------------------------------------------------------------------------
%% @doc 从连接读取数据
%%
%% 返回接收队列中累积的数据。
%%
%% @param PCB 协议控制块
%% @returns {Data | undefined, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec read(#aiutp_pcb{}) -> {binary() | undefined, #aiutp_pcb{}}.
read(#aiutp_pcb{inque = InQue, max_window = MaxWindow,
                inbuf = InBuf, last_rcv_win = LastRcvWin} = PCB) ->
    L = aiutp_queue:to_list(InQue),
    WindowSize = aiutp_net:window_size(MaxWindow, InBuf),
    Now = aiutp_util:millisecond(),

    %% 如果窗口打开则发送 ACK
    PCB0 =
        if WindowSize > LastRcvWin ->
            if LastRcvWin == 0 ->
                aiutp_net:send_ack(PCB#aiutp_pcb{time = Now});
               true ->
                PCB#aiutp_pcb{time = Now, ida = true}
            end;
           true -> PCB#aiutp_pcb{time = Now}
        end,

    QueSize = aiutp_queue:size(InQue),
    if QueSize > 0 ->
        %% 使用 iolist_to_binary 一次性转换，避免多次二进制拼接
        {iolist_to_binary(L), PCB0#aiutp_pcb{inque = aiutp_queue:new()}};
       true ->
        {undefined, PCB0}
    end.

%%------------------------------------------------------------------------------
%% @doc 发起出站连接
%%
%% 创建新的 PCB 并发送 SYN 包以建立连接。
%%
%% @param Socket UDP socket 引用
%% @param ConnIdRecv 接收连接 ID
%% @returns 已发送 SYN 的初始化 PCB
%% @end
%%------------------------------------------------------------------------------
-spec connect(socket_ref(), non_neg_integer()) -> #aiutp_pcb{}.
connect(Socket, ConnIdRecv) ->
    ConnIdSend = aiutp_util:bit16(ConnIdRecv + 1),
    PCB = new(ConnIdRecv, ConnIdSend, Socket),
    #aiutp_pcb{max_window = MaxWindow, inbuf = InBuf,
               conn_id_recv = ConnId, outbuf = OutBuf} = PCB,

    Now = aiutp_util:millisecond(),
    SeqNR = aiutp_util:bit16_random(),
    WindowSize = aiutp_net:window_size(MaxWindow, InBuf),

    %% 构建 SYN 包
    Packet = aiutp_packet:syn(SeqNR),
    Packet0 = Packet#aiutp_packet{conn_id = ConnId, wnd = WindowSize, seq_nr = SeqNR},
    WrapPacket = #aiutp_packet_wrap{packet = Packet0},

    %% 添加到发送缓冲区
    OutBuf0 = aiutp_buffer:append(WrapPacket, OutBuf),
    Iter = aiutp_buffer:head(OutBuf0),

    PCB0 = PCB#aiutp_pcb{
        state = ?CS_SYN_SENT,
        time = Now,
        retransmit_timeout = 3000,
        rto_timeout = 3000 + Now,
        last_rcv_win = WindowSize,
        outbuf = OutBuf0,
        cur_window_packets = 1,
        seq_nr = SeqNR + 1
    },
    aiutp_net:send_packet(Iter, PCB0).

%%------------------------------------------------------------------------------
%% @doc 接受入站连接
%%
%% 为入站 SYN 包创建新的 PCB 并处理。
%%
%% @param Socket UDP socket 引用
%% @param {Packet, Timestamp} 带时间戳的入站 SYN 包
%% @returns {ConnIdRecv, 初始化后的PCB}
%% @end
%%------------------------------------------------------------------------------
-spec accept(socket_ref(), {#aiutp_packet{}, non_neg_integer()}) -> {non_neg_integer(), #aiutp_pcb{}}.
accept(Socket, {#aiutp_packet{conn_id = ConnIdSend}, _} = Packet) ->
    ConnIdRecv = aiutp_util:bit16(ConnIdSend + 1),
    PCB = new(ConnIdRecv, ConnIdSend, Socket),
    PCB1 = process_incoming(Packet, PCB),
    {ConnIdRecv, PCB1}.

%%------------------------------------------------------------------------------
%% @doc 刷新待发送的出站数据
%%
%% @param PCB 协议控制块
%% @returns 刷新队列后的更新 PCB
%% @end
%%------------------------------------------------------------------------------
-spec flush(#aiutp_pcb{}) -> #aiutp_pcb{}.
flush(PCB) ->
    aiutp_net:flush_queue(PCB).

%% 协议包格式参考：
%%
%% 0       4       8               16              24              32
%% +-------+-------+---------------+---------------+---------------+
%% | type  | ver   | extension     | connection_id                 |
%% +-------+-------+---------------+---------------+---------------+
%% | timestamp_microseconds                                        |
%% +---------------+---------------+---------------+---------------+
%% | timestamp_difference_microseconds                             |
%% +---------------+---------------+---------------+---------------+
%% | wnd_size                                                      |
%% +---------------+---------------+---------------+---------------+
%% | seq_nr                        | ack_nr                        |
%% +---------------+---------------+---------------+---------------+
%%
%% - timestamp_microseconds: 发送时间戳
%% - timestamp_difference_microseconds: 单向延迟（接收时间 - 发送时间）
%% - wnd_size: 接收方可用缓冲区空间（字节）
%%
%% 选择性确认（SACK）：最大 4 字节 = 32 个包 * 512 字节 = 16KB
%% 覆盖范围 [ack_nr + 2, ack_nr + 2 + 31]
%% 每字节内的位顺序是反转的（LSB = 最小序列号）
