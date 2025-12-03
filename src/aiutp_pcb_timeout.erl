%%------------------------------------------------------------------------------
%% @doc uTP 协议控制块的超时处理模块
%%
%% 本模块实现 uTP 协议中的超时检测和处理逻辑，对齐 libutp 实现。
%%
%% == 超时类型 ==
%%
%% === RTO 超时（重传超时）===
%% - 当 rto_timeout 到期且有未确认的包时触发
%% - 执行指数退避：RTO *= 2（libutp 标准）
%% - 标记所有未确认包为需要重发
%% - 重置拥塞窗口到 1 个包大小，启用慢启动
%%
%% === Keepalive 超时 ===
%% - 每 KEEPALIVE_INTERVAL (29秒) 发送一次心跳
%% - 用于保持 NAT 映射和检测死连接
%% - 如果 2 * KEEPALIVE_INTERVAL 内没有收到包，关闭连接
%%
%% === 连接超时 ===
%% - SYN_SENT: 最多重试 MAX_SYN_RETRIES (2) 次
%% - 其他状态: 最多重试 MAX_RETRANSMIT_COUNT (4) 次
%% - 超过限制后发送 RESET 并销毁连接
%%
%% === 零窗口探测 ===
%% - 当对端通告零窗口时启用
%% - 定期尝试恢复窗口
%%
%% @reference libutp: https://github.com/bittorrent/libutp
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_timeout).
-include("aiutp.hrl").

-export([check_timeouts/1,
         mark_need_resend/4]).

%%------------------------------------------------------------------------------
%% 常量定义
%%------------------------------------------------------------------------------

%% libutp: SYN 包最大重试次数
-define(MAX_SYN_RETRIES, 2).

%% libutp: 一般包最大重试次数
-define(MAX_RETRANSMIT_COUNT, 4).

%%------------------------------------------------------------------------------
%% API 函数
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc 超时检查的主入口点
%%
%% 定期调用以处理各种超时条件：
%% 1. 刷新待发送的包
%% 2. 检查 RTO 超时
%% 3. 检查 keepalive 超时
%% 4. 检查连接状态转换
%%
%% @param PCB 协议控制块
%% @returns 超时处理后的更新 PCB
%% @end
%%------------------------------------------------------------------------------
-spec check_timeouts(#aiutp_pcb{}) -> #aiutp_pcb{}.
check_timeouts(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_DESTROY;
       State == ?CS_RESET ->
    %% 已销毁或重置的连接不需要超时检查
    PCB;
check_timeouts(PCB) ->
    Now = aiutp_util:millisecond(),
    %% 先刷新待发送的包
    PCB0 = aiutp_net:flush_packets(PCB),
    do_check_timeouts(PCB0#aiutp_pcb{time = Now}).

%%------------------------------------------------------------------------------
%% @doc 标记发送缓冲区中需要重发的包
%%
%% libutp: 遍历所有未确认的包并标记为 need_resend。
%% 同时从 cur_window 中减去这些包的载荷大小。
%%
%% @param CurWindowPackets 传输中的包数量
%% @param CurWindow 当前窗口大小（字节）
%% @param Iter 当前缓冲区迭代器（-1 表示结束）
%% @param OutBuf 发送缓冲区
%% @returns {NewCurWindow, NewOutBuf}
%% @end
%%------------------------------------------------------------------------------
-spec mark_need_resend(integer(), integer(), integer(), term()) -> {integer(), term()}.
mark_need_resend(_, CurWindow, -1, OutBuf) ->
    {CurWindow, OutBuf};
mark_need_resend(0, CurWindow, _, OutBuf) ->
    {CurWindow, OutBuf};
mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf) ->
    Next = aiutp_buffer:next(Iter, OutBuf),
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    #aiutp_packet_wrap{
        transmissions = Transmissions,
        need_resend = NeedResend,
        payload = Payload
    } = WrapPacket,

    %% libutp: 跳过从未发送的包(transmissions=0)和已标记的包
    if (NeedResend == true) orelse (Transmissions == 0) ->
        mark_need_resend(CurWindowPackets - 1, CurWindow, Next, OutBuf);
       true ->
        %% 标记为需要重发，并从 cur_window 中扣除载荷大小
        WrapPacket0 = WrapPacket#aiutp_packet_wrap{need_resend = true},
        OutBuf0 = aiutp_buffer:replace(Iter, WrapPacket0, OutBuf),
        mark_need_resend(CurWindowPackets - 1, CurWindow - Payload, Next, OutBuf0)
    end.

%%------------------------------------------------------------------------------
%% 内部函数
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc 根据状态分发超时检查
%%------------------------------------------------------------------------------
do_check_timeouts(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_UNINITIALIZED;
       State == ?CS_IDLE;
       State == ?CS_RESET ->
    %% 这些状态不需要超时检查
    PCB;

do_check_timeouts(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_SENT;
       State == ?CS_SYN_RECV;
       State == ?CS_CONNECTED;
       State == ?CS_CONNECTED_FULL ->
    %% 活跃连接状态需要完整的超时检查
    check_active_connection_timeouts(PCB);

do_check_timeouts(PCB) ->
    %% 其他状态使用默认处理
    PCB.

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查活跃连接的超时
%%------------------------------------------------------------------------------
check_active_connection_timeouts(#aiutp_pcb{
        time = Now,
        state = State,
        rto_timeout = RTOTimeout,
        burst = Burst,
        cur_window_packets = CurWindowPackets
    } = PCB) ->

    %% 步骤 1: 处理零窗口探测
    PCB0 = handle_zero_window_probe(PCB),

    %% 步骤 2: 检查 RTO 超时
    {Continue, PCB1} =
        if (Burst == false) andalso
           (RTOTimeout > 0) andalso
           (Now >= RTOTimeout) ->
            %% 标准模式 RTO 超时
            handle_rto_timeout(PCB0);

           (Burst == true) andalso
           (CurWindowPackets > 0) andalso
           (State == ?CS_CONNECTED) ->
            %% 突发模式下检查是否需要重发
            handle_burst_mode_check(PCB0);

           true ->
            {true, PCB0}
        end,

    %% 步骤 3: 如果连接仍然有效，继续其他检查
    if Continue ->
        PCB2 = handle_keepalive(PCB1),
        PCB3 = flush_if_no_pending_packets(PCB2),
        maybe_transition_from_full(PCB3);
       true ->
        PCB1
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理零窗口探测
%%
%% libutp: 当对端通告零窗口时，定期尝试恢复
%%------------------------------------------------------------------------------
handle_zero_window_probe(#aiutp_pcb{
        time = Now,
        zerowindow_time = ZeroWindowTime,
        max_window_user = MaxWindowUser
    } = PCB) ->
    if (MaxWindowUser == 0) andalso (Now >= ZeroWindowTime) ->
        %% libutp: max_window_user = PACKET_SIZE
        PCB#aiutp_pcb{max_window_user = ?PACKET_SIZE};
       true ->
        PCB
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 RTO 超时
%%
%% 返回 {Continue, PCB}，Continue 为 false 表示连接已终止
%%------------------------------------------------------------------------------
handle_rto_timeout(PCB) ->
    %% 先检查是否应该终止连接
    case check_fatal_timeout(PCB) of
        {false, PCB0} ->
            {false, PCB0};
        {true, PCB0} ->
            %% 连接仍然有效，执行重传超时处理
            do_retransmit_timeout(PCB0)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查致命超时条件
%%
%% libutp 致命超时条件：
%% 1. SYN_RECV 状态超时 -> 直接销毁
%% 2. Keepalive 超时（2 * KEEPALIVE_INTERVAL 没有收到包）
%% 3. SYN_SENT 重试超过 MAX_SYN_RETRIES
%% 4. 一般重试超过 MAX_RETRANSMIT_COUNT
%%
%% @returns {Continue, PCB} - Continue 为 false 表示连接应终止
%%------------------------------------------------------------------------------
check_fatal_timeout(#aiutp_pcb{state = ?CS_SYN_RECV} = PCB) ->
    %% libutp: SYN_RECV 超时直接销毁，不发送 RESET
    %% 因为可能是恶意连接尝试
    logger:debug("SYN_RECV timeout, destroying connection"),
    {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};

check_fatal_timeout(#aiutp_pcb{
        time = Now,
        last_got_packet = LastGotPacket,
        close_requested = CloseRequested
    } = PCB)
  when (LastGotPacket > 0) andalso
       (Now - LastGotPacket > ?KEEPALIVE_INTERVAL * 2) ->
    %% libutp: Keepalive 超时
    logger:warning("Connection timeout: no packet received for ~p ms",
                   [Now - LastGotPacket]),
    PCB0 = aiutp_net:send_reset(PCB),
    NewState = if CloseRequested -> ?CS_DESTROY; true -> ?CS_RESET end,
    {false, PCB0#aiutp_pcb{state = NewState}};

check_fatal_timeout(#aiutp_pcb{
        state = ?CS_SYN_SENT,
        retransmit_count = RetransmitCount,
        close_requested = CloseRequested
    } = PCB)
  when RetransmitCount >= ?MAX_SYN_RETRIES ->
    %% libutp: SYN_SENT 超过最大重试次数
    logger:warning("SYN_SENT timeout after ~p retries", [RetransmitCount]),
    PCB0 = aiutp_net:send_reset(PCB),
    NewState = if CloseRequested -> ?CS_DESTROY; true -> ?CS_RESET end,
    {false, PCB0#aiutp_pcb{state = NewState}};

check_fatal_timeout(#aiutp_pcb{
        retransmit_count = RetransmitCount,
        close_requested = CloseRequested,
        burst = false
    } = PCB)
  when RetransmitCount >= ?MAX_RETRANSMIT_COUNT ->
    %% libutp: 一般状态超过最大重试次数
    logger:warning("Connection timeout after ~p retries", [RetransmitCount]),
    PCB0 = aiutp_net:send_reset(PCB),
    NewState = if CloseRequested -> ?CS_DESTROY; true -> ?CS_RESET end,
    {false, PCB0#aiutp_pcb{state = NewState}};

check_fatal_timeout(PCB) ->
    {true, PCB}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 执行重传超时处理
%%
%% libutp 逻辑：
%% 1. 指数退避：RTO *= 2
%% 2. 调整拥塞窗口
%% 3. 标记包为需要重发
%% 4. 发送第一个包
%%------------------------------------------------------------------------------
do_retransmit_timeout(#aiutp_pcb{
        time = Now,
        retransmit_timeout = RetransmitTimeout,
        cur_window_packets = CurWindowPackets,
        cur_window = CurWindow,
        max_window = MaxWindow,
        outbuf = OutBuf,
        seq_nr = SeqNR,
        retransmit_count = RetransmitCount
    } = PCB) ->

    %% libutp: 指数退避 RTO *= 2
    NewTimeout = RetransmitTimeout * 2,

    %% 调整拥塞窗口
    PCB0 =
        if (CurWindowPackets == 0) andalso (MaxWindow > ?PACKET_SIZE) ->
            %% libutp: 空闲连接，保守减少窗口到 2/3
            PCB#aiutp_pcb{
                retransmit_timeout = NewTimeout,
                rto_timeout = Now + NewTimeout,
                duplicate_ack = 0,
                max_window = erlang:max((MaxWindow * 2 div 3), ?PACKET_SIZE)
            };
           true ->
            %% libutp: 活跃连接，重置窗口到 1 个包大小并启用慢启动
            PCB#aiutp_pcb{
                retransmit_timeout = NewTimeout,
                rto_timeout = Now + NewTimeout,
                duplicate_ack = 0,
                max_window = ?PACKET_SIZE,
                slow_start = true,
                ssthresh = erlang:max(MaxWindow div 2, ?PACKET_SIZE)
            }
        end,

    %% 如果有未确认的包，标记为需要重发并发送第一个
    if CurWindowPackets > 0 ->
        Iter = aiutp_buffer:head(OutBuf),
        {CurWindow0, OutBuf0} = mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf),
        PCB1 = PCB0#aiutp_pcb{
            cur_window = CurWindow0,
            outbuf = OutBuf0,
            retransmit_count = RetransmitCount + 1,
            fast_timeout = true,
            timeout_seq_nr = SeqNR
        },
        %% 立即发送第一个需要重发的包
        {true, aiutp_net:send_packet(aiutp_buffer:head(OutBuf0), PCB1)};
       true ->
        {true, PCB0}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 突发模式下的超时检查
%%------------------------------------------------------------------------------
handle_burst_mode_check(#aiutp_pcb{
        cur_window_packets = CurWindowPackets,
        cur_window = CurWindow,
        outbuf = OutBuf
    } = PCB) ->
    case check_fatal_timeout(PCB) of
        {true, _} ->
            %% 连接有效，标记包为需要重发
            Iter = aiutp_buffer:head(OutBuf),
            {CurWindow0, OutBuf0} = mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf),
            PCB0 = aiutp_net:flush_packets(PCB#aiutp_pcb{
                cur_window = CurWindow0,
                outbuf = OutBuf0
            }),
            {true, PCB0};
        {false, PCB0} ->
            {false, PCB0}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 keepalive
%%
%% libutp: 每 KEEPALIVE_INTERVAL 发送一次心跳
%%------------------------------------------------------------------------------
handle_keepalive(#aiutp_pcb{
        time = Now,
        state = State,
        fin_sent = FinSent,
        burst = Burst,
        last_sent_packet = LastSentPacket
    } = PCB) ->

    ShouldSendKeepalive =
        (FinSent == false) andalso
        (
            %% 标准模式：CONNECTED 状态且超过 KEEPALIVE_INTERVAL
            (((State == ?CS_CONNECTED) orelse (State == ?CS_CONNECTED_FULL)) andalso
             (Now - LastSentPacket >= ?KEEPALIVE_INTERVAL))
            orelse
            %% 突发模式：超过 5 秒
            ((Burst == true) andalso (Now - LastSentPacket >= 5000))
        ),

    if ShouldSendKeepalive ->
        aiutp_net:send_keep_alive(PCB);
       true ->
        PCB
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 如果没有待发送的包，刷新发送队列
%%------------------------------------------------------------------------------
flush_if_no_pending_packets(#aiutp_pcb{cur_window_packets = 0} = PCB) ->
    aiutp_net:flush_queue(PCB);
flush_if_no_pending_packets(PCB) ->
    PCB.

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查是否可以从 CONNECTED_FULL 转换到 CONNECTED
%%------------------------------------------------------------------------------
maybe_transition_from_full(#aiutp_pcb{state = ?CS_CONNECTED_FULL} = PCB) ->
    {IsFull, PCB0} = aiutp_net:is_full(-1, PCB),
    if IsFull == false ->
        PCB0#aiutp_pcb{state = ?CS_CONNECTED};
       true ->
        PCB0
    end;
maybe_transition_from_full(PCB) ->
    PCB.
