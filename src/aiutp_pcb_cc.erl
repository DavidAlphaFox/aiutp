%%------------------------------------------------------------------------------
%% @doc uTP 协议控制块的拥塞控制
%%
%% 本模块实现 BEP-29 规范中的 LEDBAT（低额外延迟后台传输）拥塞控制算法。
%%
%% 主要函数：
%% - cc_control/4: LEDBAT 拥塞控制主逻辑
%% - maybe_decay_win/1: 超时时窗口衰减
%% - ack_packet/3: 处理已确认的包以计算 RTT
%% - caculate_acked_bytes/4: 计算已确认字节总数和最小 RTT
%% - selective_ack_packet/3: 处理选择性确认（SACK）
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_cc).
-include("aiutp.hrl").

-export([cc_control/4,
         maybe_decay_win/1,
         ack_packet/3,
         caculate_acked_bytes/4,
         selective_ack_packet/3]).

%%------------------------------------------------------------------------------
%% @doc LEDBAT 拥塞控制算法
%%
%% 根据相对于目标的测量延迟调整拥塞窗口。
%% 实现慢启动和拥塞避免两个阶段。
%%
%% 算法来源: RFC 6817 和 libutp (utp_internal.cpp)
%%
%% LEDBAT 公式:
%%   cwnd += GAIN * off_target * bytes_acked * MSS / cwnd
%%   其中 off_target = (TARGET - queuing_delay) / TARGET
%%
%% 时钟漂移惩罚 (libutp):
%%   当检测到对端时钟变慢超过 200ms/5s 时，应用惩罚延迟
%%   penalty = (-clock_drift - 200000) / 7
%%   our_delay += penalty
%%   目的：防止对端通过减慢时钟来"作弊"获取更多带宽
%%
%% @param Now 当前时间戳（毫秒）
%% @param AckedBytes 已确认字节数
%% @param RTT 测量的往返时间（微秒）
%% @param PCB 协议控制块
%% @returns 更新窗口大小后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec cc_control(integer(), integer(), integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
cc_control(Now, AckedBytes, RTT,
           #aiutp_pcb{our_hist = OurHist,
                      max_window = MaxWindow,
                      last_maxed_out_window = LastMaxedOutWindow,
                      slow_start = SlowStart, ssthresh = SSThresh,
                      clock_drift = ClockDrift} = PCB) ->
    %% 获取当前延迟估计（取历史最小值和当前 RTT 的较小者）
    OurHistValue = aiutp_delay:value(OurHist),
    OurDelayRaw = erlang:min(aiutp_util:bit32(RTT), OurHistValue),

    %% libutp 时钟漂移惩罚机制
    %% 当 clock_drift < -200000（对端时钟变慢超过 200ms/5s）时应用惩罚
    %% 这防止对端通过减慢时钟来"作弊"获取更多带宽
    %% Reason: 如果对端减慢时钟，会使我们测量的延迟看起来更低，
    %%         从而让我们的拥塞窗口增长过快，占用过多带宽
    OurDelay = apply_clock_drift_penalty(ClockDrift, OurDelayRaw),

    %% 目标延迟固定为 100ms（RFC 6817 要求 <= 100ms）
    Target = ?TARGET_DELAY,

    %% 计算偏离目标的程度
    %% off_target > 0: 延迟低于目标，可以增加窗口
    %% off_target < 0: 延迟高于目标，需要减小窗口
    OffTarget = Target - OurDelay,

    %% 计算窗口因子和延迟因子（libutp 公式）
    %% window_factor = min(bytes_acked, max_window) / max(max_window, bytes_acked)
    Win0 = erlang:min(AckedBytes, MaxWindow),
    Win1 = erlang:max(AckedBytes, MaxWindow),
    WindowFactor = Win0 / Win1,

    %% delay_factor = off_target / target
    %% 限制 off_target 到 [-target, target] 范围，防止异常值
    ClampedOffTarget = aiutp_util:clamp(OffTarget, -Target, Target),
    DelayFactor = ClampedOffTarget / Target,

    %% 计算窗口调整的缩放增益
    %% scaled_gain = MAX_CWND_INCREASE_BYTES_PER_RTT * window_factor * delay_factor
    ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,

    %% libutp: 如果窗口在 WINDOW_SATURATION_TIMEOUT 内没有被充分利用，不增长窗口
    %% 这防止了在速率受限时窗口无限增长
    ScaledGain0 =
        if (ScaledGain > 0) andalso (Now - LastMaxedOutWindow > ?WINDOW_SATURATION_TIMEOUT) -> 0;
           true -> erlang:trunc(ScaledGain)
        end,

    %% LEDBAT 拥塞窗口
    LedbatCwnd = erlang:max(?MIN_WINDOW_SIZE, (MaxWindow + ScaledGain0)),

    %% 慢启动或拥塞避免
    %% libutp: 慢启动时每 RTT 增加一个 PACKET_SIZE
    {SlowStart0, SSThresh0, MaxWindow0} =
        if SlowStart ->
            SSCwnd = MaxWindow + erlang:trunc(WindowFactor * ?PACKET_SIZE),
            if SSCwnd > SSThresh ->
                %% 超过慢启动阈值，退出慢启动
                {false, SSThresh, MaxWindow};
               OurDelay > Target * 0.9 ->
                %% 延迟达到目标的 90%，退出慢启动
                {false, MaxWindow, MaxWindow};
               true ->
                %% 继续慢启动，取 SSCwnd 和 LedbatCwnd 的较大者
                {SlowStart, SSThresh, erlang:max(SSCwnd, LedbatCwnd)}
            end;
           true ->
            %% 拥塞避免阶段
            {SlowStart, SSThresh, LedbatCwnd}
        end,

    PCB#aiutp_pcb{
        slow_start = SlowStart0,
        ssthresh = SSThresh0,
        max_window = aiutp_util:clamp(MaxWindow0, ?MIN_WINDOW_SIZE,
                                      ?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE)
    }.

%%------------------------------------------------------------------------------
%% @doc 不活跃时窗口衰减
%%
%% libutp: 如果自上次衰减以来已过去足够时间，则将拥塞窗口减少 50%。
%% 用于防止陈旧的窗口大小。
%%
%% RFC 6817: 丢包时 cwnd = min(cwnd, max(cwnd/2, MIN_CWND * MSS))
%%
%% @param PCB 协议控制块
%% @returns 可能衰减窗口后的更新 PCB
%% @end
%%------------------------------------------------------------------------------
-spec maybe_decay_win(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_decay_win(#aiutp_pcb{time = Now,
                           max_window = MaxWindow,
                           last_rwin_decay = LastRWinDecay} = PCB) ->
    if (Now - LastRWinDecay) < ?MAX_WINDOW_DECAY -> PCB;
       true ->
           %% libutp: max_window = max_window * 0.5
           MaxWindow0 = erlang:max(MaxWindow div 2, ?MIN_WINDOW_SIZE),
           PCB#aiutp_pcb{
               slow_start = false,
               ssthresh = MaxWindow0,
               max_window = MaxWindow0,
               last_rwin_decay = Now
           }
    end.

%%------------------------------------------------------------------------------
%% @doc 处理已确认的包以计算 RTT
%%
%% 根据已确认的包更新 RTT 估计值和当前窗口。
%% 仅使用首次传输来计算 RTT（Karn 算法）。
%%
%% @param MicroNow 当前时间（微秒）
%% @param WrapPacket 已确认的包装包
%% @param Acc 累加器元组 {Now, CurWindow, RTT, RTO, RTTVar, RTTHist}
%% @returns 更新后的累加器
%% @end
%%------------------------------------------------------------------------------
-spec ack_packet(integer(), #aiutp_packet_wrap{}, tuple()) -> tuple().
ack_packet(MicroNow,
           #aiutp_packet_wrap{transmissions = Transmissions,
                              time_sent = TimeSent,
                              need_resend = NeedResend,
                              payload = Payload},
           {Now, CurWindow, RTT, RTO, RTTVar, RTTHist}) ->
    {RTT1, RTTVar1, RTO0, RTTHist1} =
        if Transmissions == 1 ->
            %% 仅对首次传输计算 RTT（Karn 算法）
            {RTT0, RTTVar0, ERTT} = aiutp_rtt:caculate_rtt(RTT, RTTVar, TimeSent, MicroNow),
            RTTHist0 =
                if RTT /= 0 -> aiutp_delay:add_sample(ERTT, Now, RTTHist);
                   true -> RTTHist
                end,
            {RTT0, RTTVar0, aiutp_util:clamp((RTT0 + RTTVar0 * 4), 600, 6000), RTTHist0};
           true -> {RTT, RTTVar, RTO, RTTHist}
        end,

    %% 调整当前窗口（不计算重发的包）
    CurWindow0 =
        if NeedResend == false -> CurWindow - Payload;
           true -> CurWindow
        end,

    {Now, CurWindow0, RTT1, RTO0, RTTVar1, RTTHist1}.

%%------------------------------------------------------------------------------
%% @doc 计算已确认字节总数和最小 RTT
%%
%% 处理已确认和选择性确认的包列表，
%% 计算拥塞控制所需的总字节数和最小 RTT。
%%
%% @param Acc 初始累加器 {Bytes, RTT}
%% @param Now 当前时间戳
%% @param AckedPackets 正常确认的包列表
%% @param SAckedPackets 选择性确认的包列表
%% @returns {TotalBytes, MinRTT}
%% @end
%%------------------------------------------------------------------------------
-spec caculate_acked_bytes(tuple(), integer(), list(), list()) -> tuple().
caculate_acked_bytes(Acc, Now, AckedPackets, SAckedPackets) ->
    Fun = fun(WrapPacket, {Bytes, RTT}) ->
              TimeSent = WrapPacket#aiutp_packet_wrap.time_sent,
              RTT0 =
                  if TimeSent < Now -> erlang:min(RTT, (Now - TimeSent));
                     true -> erlang:min(RTT, 50000)
                  end,
              {Bytes + WrapPacket#aiutp_packet_wrap.payload, RTT0}
          end,
    Acc0 = lists:foldl(Fun, Acc, AckedPackets),
    lists:foldl(Fun, Acc0, SAckedPackets).

%%------------------------------------------------------------------------------
%% @doc 处理选择性确认（SACK）包
%%
%% 处理选择性确认的包，更新 RTT 估计值，
%% 并在需要时触发快速重传。
%%
%% 注意：当前实现与 C++ 版本相比可能浪费带宽。
%% 这是未来优化的已知限制。
%%
%% @param SAckedPackets 选择性确认的包列表
%% @param MicroNow 当前时间（微秒）
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
%% @end
%%------------------------------------------------------------------------------
-spec selective_ack_packet(list(), integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
selective_ack_packet(_, _, #aiutp_pcb{cur_window_packets = CurWindowPackets} = PCB)
  when CurWindowPackets == 0 -> PCB;
selective_ack_packet([], _, PCB) -> PCB;
selective_ack_packet(SAckedPackets,
                     MicroNow,
                     #aiutp_pcb{seq_nr = SeqNR,
                                cur_window_packets = CurWindowPackets,
                                fast_resend_seq_nr = FastResendSeqNR} = PCB) ->
    Now0 = aiutp_util:millisecond(),

    %% 从 SACK 的包更新 RTT 和窗口
    {_, CurWindow0, RTT0, RTO0, RTTVar0, RTTHist0} =
        lists:foldr(
            fun(I, AccPCB) -> ack_packet(MicroNow, I, AccPCB) end,
            {Now0, PCB#aiutp_pcb.cur_window, PCB#aiutp_pcb.rtt, PCB#aiutp_pcb.rto,
             PCB#aiutp_pcb.rtt_var, PCB#aiutp_pcb.rtt_hist},
            SAckedPackets),

    PCB0 = PCB#aiutp_pcb{
        cur_window = CurWindow0,
        rtt = RTT0,
        rtt_var = RTTVar0,
        rtt_hist = RTTHist0,
        rto = RTO0,
        retransmit_count = 0,
        retransmit_timeout = RTO0,
        rto_timeout = RTO0 + Now0
    },

    %% 计算快速重传的序列号范围
    %% libutp: 只重传序列号 >= fast_resend_seq_nr 的包，防止重复重传
    [El | _] = SAckedPackets,
    OldestUnackedSeq = aiutp_util:bit16(SeqNR - CurWindowPackets),
    Packet = El#aiutp_packet_wrap.packet,
    MaxSeq = aiutp_util:bit16(Packet#aiutp_packet.seq_nr - 1),

    %% libutp: MinSeq 应该是 max(OldestUnackedSeq, FastResendSeqNR)
    %% 这样可以避免重传已经快速重传过的包
    MinSeq = case ?WRAPPING_DIFF_16(FastResendSeqNR, OldestUnackedSeq) > 0 of
        true -> FastResendSeqNR;
        false -> OldestUnackedSeq
    end,

    %% 构建 SACK 确认的序列号集合，用于精确筛选需要重传的包
    SAckedSeqs = sets:from_list([
        (W#aiutp_packet_wrap.packet)#aiutp_packet.seq_nr || W <- SAckedPackets
    ]),

    if ?WRAPPING_DIFF_16(MaxSeq, MinSeq) > ?DUPLICATE_ACKS_BEFORE_RESEND ->
        %% 触发快速重传：只重传被跳过的包（不在 SACK 集合中的包）
        {Sent, LastSeq, PCB1} = aiutp_net:send_skipped_packets(
            MinSeq, MaxSeq, SAckedSeqs, 4, PCB0),
        PCB2 = PCB1#aiutp_pcb{
            fast_resend_seq_nr = aiutp_util:bit16(LastSeq + 1),
            duplicate_ack = erlang:length(SAckedPackets)
        },
        if Sent > 0 -> maybe_decay_win(PCB2);
           true -> PCB2
        end;
       true -> PCB0
    end.

%%------------------------------------------------------------------------------
%% 内部函数
%%------------------------------------------------------------------------------

%% libutp 时钟漂移惩罚阈值（微秒/5秒）
%% 当 clock_drift < -200000 时应用惩罚
-define(CLOCK_DRIFT_PENALTY_THRESHOLD, -200000).

%%------------------------------------------------------------------------------
%% @doc 应用时钟漂移惩罚
%%
%% libutp 实现：当检测到对端时钟变慢超过阈值时，增加感知延迟。
%% 这防止对端通过减慢时钟来"作弊"获取更多带宽。
%%
%% 公式: penalty = (-clock_drift - 200000) / 7
%%       our_delay = our_delay + penalty
%%
%% @param ClockDrift 估计的时钟漂移（微秒/5秒）
%% @param OurDelay 原始延迟估计（微秒）
%% @returns 应用惩罚后的延迟（微秒）
%% @end
%%------------------------------------------------------------------------------
-spec apply_clock_drift_penalty(integer(), non_neg_integer()) -> non_neg_integer().
apply_clock_drift_penalty(ClockDrift, OurDelay) when ClockDrift < ?CLOCK_DRIFT_PENALTY_THRESHOLD ->
    %% 对端时钟变慢超过 200ms/5s，应用惩罚
    Penalty = (-ClockDrift - 200000) div 7,
    OurDelay + Penalty;
apply_clock_drift_penalty(_ClockDrift, OurDelay) ->
    %% 时钟漂移在正常范围内，不应用惩罚
    OurDelay.
