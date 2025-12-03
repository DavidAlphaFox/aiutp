%%------------------------------------------------------------------------------
%% @doc uTP 协议控制块的超时处理
%%
%% 本模块处理 uTP 协议中的各种超时条件：
%% - Keepalive 超时检测
%% - 基于 RTT 的连接超时
%% - 重传超时和计数
%% - 数据包重发标记
%%
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_timeout).
-include("aiutp.hrl").

-export([check_timeouts/1,
         mark_need_resend/4]).

%%------------------------------------------------------------------------------
%% @doc 超时检查的主入口点
%%
%% 刷新待发送的包并检查各种超时条件。
%% 应定期调用以处理重传和 keepalive。
%%
%% @param PCB 协议控制块
%% @returns 超时处理后的更新 PCB
%% @end
%%------------------------------------------------------------------------------
-spec check_timeouts(#aiutp_pcb{}) -> #aiutp_pcb{}.
check_timeouts(#aiutp_pcb{state = State} = PCB)
  when State /= ?CS_DESTROY,
       State /= ?CS_RESET ->
    Now = aiutp_util:millisecond(),
    PCB0 = aiutp_net:flush_packets(PCB),
    check_timeouts_0(PCB0#aiutp_pcb{time = Now});
check_timeouts(PCB) -> PCB.

%%------------------------------------------------------------------------------
%% @private
%% @doc 过滤不需要超时检查的状态
%%------------------------------------------------------------------------------
check_timeouts_0(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_UNINITIALIZED;
       State == ?CS_IDLE;
       State == ?CS_RESET -> PCB;
check_timeouts_0(#aiutp_pcb{time = Now,
                            state = State,
                            zerowindow_time = ZeroWindowTime,
                            max_window_user = MaxWindowUser,
                            rto_timeout = RTOTimeout,
                            fin_sent = FinSent,
                            cur_window_packets = CurWindowPackets,
                            cur_window = CurWindow,
                            outbuf = OutBuf,
                            burst = Burst} = PCB) ->
    %% 处理零窗口探测
    PCB0 =
        if (MaxWindowUser == 0) and
           (Now - ZeroWindowTime >= 0) ->
            PCB#aiutp_pcb{max_window_user = ?MIN_WINDOW_SIZE};
           true -> PCB
        end,

    %% 检查 RTO 超时或突发模式超时
    {Continue, PCB1} =
        if (Burst == false) and
           (RTOTimeout > 0) and
           (Now - RTOTimeout >= 0) ->
            check_timeouts_2(check_timeouts_1(PCB0));
           (Burst == true) and
           (CurWindowPackets > 0) and
           (State == ?CS_CONNECTED) ->
            case check_timeouts_1(PCB0) of
                {true, _} ->
                    Iter = aiutp_buffer:head(OutBuf),
                    {CurWindow0, OutBuf0} = mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf),
                    {true, aiutp_net:flush_packets(PCB#aiutp_pcb{cur_window = CurWindow0, outbuf = OutBuf0})};
                {false, _} = W -> W
            end;
           true -> {true, PCB0}
        end,

    if Continue == true ->
        %% 处理 keepalive
        PCBKeepAlive =
            if (FinSent == false) and
               ((State == ?CS_CONNECTED) or (State == ?CS_CONNECTED_FULL)) and
               (Now - PCB1#aiutp_pcb.last_sent_packet >= ?KEEPALIVE_INTERVAL) ->
                aiutp_net:send_keep_alive(PCB1);
               (FinSent == false) and
               (Burst == true) and
               (Now - PCB1#aiutp_pcb.last_sent_packet >= 5000) ->
                aiutp_net:send_keep_alive(PCB1);
               true -> PCB1
            end,

        %% 如果没有未确认的包，刷新队列
        PCBFlush =
            if PCBKeepAlive#aiutp_pcb.cur_window_packets == 0 ->
                aiutp_net:flush_queue(PCBKeepAlive);
               true -> PCBKeepAlive
            end,

        %% 检查是否可以从 CONNECTED_FULL 转换到 CONNECTED
        {ISFull, PCB2} = aiutp_net:is_full(-1, PCBFlush),
        if (State == ?CS_CONNECTED_FULL) and
           (ISFull == false) ->
            PCB2#aiutp_pcb{state = ?CS_CONNECTED};
           true -> PCB2
        end;
       true -> PCB1
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 检查致命超时条件
%%
%% 如果连接应被销毁/重置返回 {false, PCB}，
%% 如果连接仍然有效返回 {true, PCB}。
%%------------------------------------------------------------------------------
check_timeouts_1(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
    %% SYN_RECV 超时，发送 RESET 通知对端后销毁连接
    PCB0 = aiutp_net:send_reset(PCB),
    {false, PCB0#aiutp_pcb{state = ?CS_DESTROY}};

check_timeouts_1(#aiutp_pcb{time = Now,
                            last_got_packet = LastGotPacket,
                            close_requested = CloseRequested} = PCB)
  when (LastGotPacket > 0), (Now - LastGotPacket > ?KEEPALIVE_INTERVAL * 2) ->
    logger:warning("Connection closed due to keepalive timeout: last_packet=~p", [LastGotPacket]),
    %% BEP-29：在状态转换前发送 RESET 通知对端
    PCB0 = aiutp_net:send_reset(PCB),
    if CloseRequested == true -> {false, PCB0#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB0#aiutp_pcb{state = ?CS_RESET}}
    end;

check_timeouts_1(#aiutp_pcb{rtt = RTT, close_requested = CloseRequested} = PCB)
  when (RTT > 6000) ->
    logger:warning("Connection closed due to excessive RTT: rtt=~p", [RTT]),
    %% BEP-29：在状态转换前发送 RESET 通知对端
    PCB0 = aiutp_net:send_reset(PCB),
    if CloseRequested == true -> {false, PCB0#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB0#aiutp_pcb{state = ?CS_RESET}}
    end;

check_timeouts_1(#aiutp_pcb{state = State,
                            close_requested = CloseRequested,
                            retransmit_count = RetransmitCount,
                            burst = false} = PCB)
  when (RetransmitCount >= 4);
       ((State == ?CS_SYN_SENT) and RetransmitCount > 2) ->
    logger:warning("Connection closed due to max retransmit count: count=~p", [RetransmitCount]),
    %% BEP-29：在状态转换前发送 RESET 通知对端
    PCB0 = aiutp_net:send_reset(PCB),
    if CloseRequested == true -> {false, PCB0#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB0#aiutp_pcb{state = ?CS_RESET}}
    end;

check_timeouts_1(PCB) -> {true, PCB}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理重传超时
%%
%% 增加 RTO，减小窗口，并标记需要重发的包。
%%------------------------------------------------------------------------------
check_timeouts_2({false, _} = W) -> W;
check_timeouts_2({true, #aiutp_pcb{time = Now,
                                   retransmit_timeout = RetransmitTimeout,
                                   cur_window_packets = CurWindowPackets,
                                   cur_window = CurWindow,
                                   max_window = MaxWindow,
                                   outbuf = OutBuf,
                                   seq_nr = SeqNR,
                                   retransmit_count = RetransmitCount} = PCB}) ->
    %% 指数退避：RTO *= 1.5
    %% 指数退避：timeout = timeout * 1.5，使用整数运算避免浮点数
    NewTimeout = (RetransmitTimeout * 3) div 2,

    PCB0 =
        if (CurWindowPackets == 0) and (MaxWindow > ?PACKET_SIZE) ->
            %% 没有未确认的包，只衰减窗口
            PCB#aiutp_pcb{
                retransmit_timeout = NewTimeout,
                rto_timeout = Now + NewTimeout,
                duplicate_ack = 0,
                max_window = erlang:max((MaxWindow * 2 div 3), ?MIN_WINDOW_SIZE)
            };
           true ->
            %% 有未确认的包，减小窗口并启用慢启动
            PCB#aiutp_pcb{
                retransmit_timeout = NewTimeout,
                rto_timeout = Now + NewTimeout,
                duplicate_ack = 0,
                max_window = erlang:max((MaxWindow div 2), ?MIN_WINDOW_SIZE),
                slow_start = true
            }
        end,

    if CurWindowPackets > 0 ->
        %% 标记需要重发的包并发送第一个包
        Iter = aiutp_buffer:head(OutBuf),
        {CurWindow0, OutBuf0} = mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf),
        PCB1 = PCB0#aiutp_pcb{
            cur_window = CurWindow0,
            outbuf = OutBuf0,
            retransmit_count = RetransmitCount + 1,
            fast_timeout = true,
            timeout_seq_nr = SeqNR
        },
        {true, aiutp_net:send_packet(aiutp_buffer:head(OutBuf0), PCB1)};
       true -> {true, PCB0}
    end.

%%------------------------------------------------------------------------------
%% @doc 标记发送缓冲区中需要重发的包
%%
%% 遍历缓冲区并标记未确认的包进行重传。
%% 调整当前窗口以计入重发的数据。
%%
%% @param CurWindowPackets 传输中的包数量
%% @param CurWindow 当前窗口大小（字节）
%% @param Iter 当前缓冲区迭代器（-1 表示结束）
%% @param OutBuf 发送缓冲区
%% @returns {NewCurWindow, NewOutBuf}
%% @end
%%------------------------------------------------------------------------------
-spec mark_need_resend(integer(), integer(), integer(), term()) -> {integer(), term()}.
mark_need_resend(_, CurWindow, -1, OutBuf) -> {CurWindow, OutBuf};
mark_need_resend(0, CurWindow, _, OutBuf) -> {CurWindow, OutBuf};
mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf) ->
    Next = aiutp_buffer:next(Iter, OutBuf),
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    #aiutp_packet_wrap{
        transmissions = Transmissions,
        need_resend = NeedResend,
        payload = Payload
    } = WrapPacket,

    if (NeedResend == true) or (Transmissions == 0) ->
        %% 已标记或从未发送
        mark_need_resend(CurWindowPackets - 1, CurWindow, Next, OutBuf);
       true ->
        %% 标记重发并调整窗口
        WrapPacket0 = WrapPacket#aiutp_packet_wrap{need_resend = true},
        OutBuf0 = aiutp_buffer:replace(Iter, WrapPacket0, OutBuf),
        mark_need_resend(CurWindowPackets - 1, CurWindow - Payload, Next, OutBuf0)
    end.
