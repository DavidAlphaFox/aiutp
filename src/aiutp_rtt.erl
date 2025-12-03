%%------------------------------------------------------------------------------
%% @doc RTT (Round-Trip Time) 估计模块
%%
%% 本模块实现 uTP 协议的 RTT 和延迟估计算法，用于拥塞控制和超时计算。
%%
%% == RTT 估计 (RFC 6298) ==
%% 使用指数加权移动平均 (EWMA) 算法：
%% ```
%% SRTT = (1 - alpha) * SRTT + alpha * R'
%% RTTVAR = (1 - beta) * RTTVAR + beta * |SRTT - R'|
%%
%% 其中:
%%   alpha = 1/8, beta = 1/4
%%   R' = 最新测量的 RTT
%% '''
%%
%% == 延迟估计 (LEDBAT) ==
%% 计算单向延迟用于 LEDBAT 拥塞控制：
%% - their_delay: 对端到我方的延迟
%% - our_delay: 我方到对端的延迟
%% - clock_drift: 时钟漂移估计，用于惩罚机制
%%
%% == 时钟漂移检测 ==
%% 通过跟踪 average_delay 的变化来检测时钟漂移：
%% ```
%% clock_drift = (clock_drift * 7 + (average_delay - prev_average_delay)) / 8
%% '''
%% 如果 clock_drift < -200000 (微秒/5秒)，表明对端时钟可能在"作弊"，
%% 拥塞控制模块会应用惩罚延迟。
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @copyright (C) 2020, David Gao
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_rtt).
-include("aiutp.hrl").

%%==============================================================================
%% API 导出
%%==============================================================================
-export([
    calculate_delay/4,      %% 计算单向延迟
    calculate_rtt/4         %% 计算 RTT
]).

%% 向后兼容别名 (将在未来版本移除)
-export([
    caculate_delay/4,
    caculate_rtt/4
]).

%%==============================================================================
%% 常量定义
%%==============================================================================

%% 平均延迟采样间隔（毫秒）
%% libutp: 每 5 秒更新一次 average_delay
-define(AVERAGE_DELAY_SAMPLE_INTERVAL, 5000).

%% 延迟基准偏移阈值（微秒）
%% 当 delay_base 变化超过此值时不进行同步
-define(DELAY_BASE_SHIFT_THRESHOLD, 10000).

%%==============================================================================
%% API 函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 计算单向延迟并更新 PCB 延迟统计
%%
%% 从收到的数据包中提取时间戳信息，计算：
%% 1. their_delay: 对端到我方的传输延迟
%% 2. our_delay: 我方到对端的传输延迟（从 TSDiff 获取）
%% 3. clock_drift: 时钟漂移估计
%%
%% @param Now 当前毫秒时间戳
%% @param MicroNow 当前微秒时间戳
%% @param Packet 收到的数据包
%% @param PCB 协议控制块
%% @returns {OurDelay, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec calculate_delay(integer(), integer(), #aiutp_packet{}, #aiutp_pcb{}) ->
    {non_neg_integer(), #aiutp_pcb{}}.
calculate_delay(Now, MicroNow,
                #aiutp_packet{tv_usec = TheirTimestamp, reply_micro = TSDiff},
                PCB) ->
    %% 步骤 1: 计算对端到我方的延迟
    TheirDelay = compute_their_delay(TheirTimestamp, MicroNow),

    %% 步骤 2: 更新对端延迟历史，并同步我方延迟历史
    {TheirHist, OurHist} = update_delay_histories(
        TheirDelay, Now, PCB#aiutp_pcb.their_hist, PCB#aiutp_pcb.our_hist),

    %% 步骤 3: 提取我方到对端的延迟
    OurDelay = extract_our_delay(TSDiff),

    %% 步骤 4: 更新延迟统计和时钟漂移
    UpdatedPCB = update_delay_statistics(Now, TheirDelay, OurDelay, TheirHist, OurHist, PCB),

    {OurDelay, UpdatedPCB}.

%%------------------------------------------------------------------------------
%% @doc 计算 RTT 并更新 RTT 变化估计
%%
%% 使用 RFC 6298 定义的 EWMA 算法：
%% - 首次测量: SRTT = R', RTTVAR = R'/2
%% - 后续测量: SRTT = 7/8 * SRTT + 1/8 * R'
%%             RTTVAR = 3/4 * RTTVAR + 1/4 * |SRTT - R'|
%%
%% @param RTT 当前 RTT 估计值（毫秒）
%% @param RTTVar 当前 RTT 变化估计值（毫秒）
%% @param TimeSent 包发送时间（微秒）
%% @param MicroNow 当前微秒时间戳
%% @returns {NewRTT, NewRTTVar, MeasuredRTT}
%% @end
%%------------------------------------------------------------------------------
-spec calculate_rtt(non_neg_integer(), non_neg_integer(), integer(), integer()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
calculate_rtt(RTT, RTTVar, TimeSent, MicroNow) ->
    %% 计算测量的 RTT（转换为毫秒）
    MeasuredRTT = aiutp_util:bit32(MicroNow - TimeSent) div 1000,

    case RTT of
        0 ->
            %% 首次测量：直接使用测量值
            {MeasuredRTT, MeasuredRTT div 2, MeasuredRTT};
        _ ->
            %% 后续测量：使用 EWMA 平滑
            Delta = RTT - MeasuredRTT,
            NewRTTVar = RTTVar + (erlang:abs(Delta) - RTTVar) div 4,
            NewRTT = (RTT * 7 + MeasuredRTT) div 8,
            {NewRTT, NewRTTVar, MeasuredRTT}
    end.

%%==============================================================================
%% 向后兼容别名
%%==============================================================================

%% @doc calculate_delay/4 的旧名称
%% @deprecated 请使用 calculate_delay/4
-spec caculate_delay(integer(), integer(), #aiutp_packet{}, #aiutp_pcb{}) ->
    {non_neg_integer(), #aiutp_pcb{}}.
caculate_delay(Now, MicroNow, Packet, PCB) ->
    calculate_delay(Now, MicroNow, Packet, PCB).

%% @doc calculate_rtt/4 的旧名称
%% @deprecated 请使用 calculate_rtt/4
-spec caculate_rtt(non_neg_integer(), non_neg_integer(), integer(), integer()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
caculate_rtt(RTT, RTTVar, TimeSent, MicroNow) ->
    calculate_rtt(RTT, RTTVar, TimeSent, MicroNow).

%%==============================================================================
%% 内部函数 - 延迟计算
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算对端到我方的传输延迟
%%
%% 使用对端发送时的时间戳和我方接收时间计算。
%% 如果时间戳为 0（无效），返回 0。
%%------------------------------------------------------------------------------
-spec compute_their_delay(non_neg_integer(), integer()) -> non_neg_integer().
compute_their_delay(0, _MicroNow) ->
    0;
compute_their_delay(TheirTimestamp, MicroNow) ->
    MicroNow - TheirTimestamp.

%%------------------------------------------------------------------------------
%% @private
%% @doc 从 TSDiff 字段提取我方到对端的延迟
%%
%% TSDiff 是对端回显的我们发送包到达时的延迟。
%% TIMESTAMP_MASK 用于过滤无效值。
%%------------------------------------------------------------------------------
-spec extract_our_delay(non_neg_integer()) -> non_neg_integer().
extract_our_delay(TSDiff) ->
    case TSDiff band ?TIMESTAMP_MASK of
        ?TIMESTAMP_MASK ->
            %% 无效值（全 1）
            0;
        Delay ->
            Delay
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 更新延迟历史记录
%%
%% 1. 将新的 their_delay 样本添加到 their_hist
%% 2. 如果 delay_base 减小，同步调整 our_hist 的基准
%%    （用于补偿路由变化等导致的系统性延迟变化）
%%------------------------------------------------------------------------------
-spec update_delay_histories(non_neg_integer(), integer(),
                             aiutp_delay:delay(), aiutp_delay:delay()) ->
    {aiutp_delay:delay(), aiutp_delay:delay()}.
update_delay_histories(0, _Now, TheirHist, OurHist) ->
    %% 无效延迟，不更新
    {TheirHist, OurHist};
update_delay_histories(TheirDelay, Now, TheirHist, OurHist) ->
    %% 记录更新前的 delay_base
    PrevDelayBase = aiutp_delay:delay_base(TheirHist),

    %% 添加新样本到 their_hist
    TheirHist1 = aiutp_delay:add_sample(TheirDelay, Now, TheirHist),

    %% 检查 delay_base 是否减小，如果是则同步 our_hist
    NewDelayBase = aiutp_delay:delay_base(TheirHist1),
    OurHist1 = maybe_sync_our_hist(PrevDelayBase, NewDelayBase, OurHist),

    {TheirHist1, OurHist1}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 如果 delay_base 减小，同步调整 our_hist
%%
%% 当网络路径变化导致 their_hist 的 delay_base 减小时，
%% 我们的历史也应该相应调整，以保持两边的一致性。
%%------------------------------------------------------------------------------
-spec maybe_sync_our_hist(non_neg_integer(), non_neg_integer(), aiutp_delay:delay()) ->
    aiutp_delay:delay().
maybe_sync_our_hist(0, _NewDelayBase, OurHist) ->
    %% 没有之前的基准值，不同步
    OurHist;
maybe_sync_our_hist(PrevDelayBase, NewDelayBase, OurHist) ->
    %% 使用 32 位环绕比较检查 delay_base 是否减小
    case ?WRAPPING_DIFF_32(NewDelayBase, PrevDelayBase) < 0 of
        true ->
            %% delay_base 减小，计算偏移量
            Shift = PrevDelayBase - NewDelayBase,
            case Shift =< ?DELAY_BASE_SHIFT_THRESHOLD of
                true ->
                    %% 偏移量在合理范围内，同步调整
                    aiutp_delay:shift(Shift, OurHist);
                false ->
                    %% 偏移量太大（可能是时钟跳变），不调整
                    OurHist
            end;
        false ->
            %% delay_base 没有减小，不需要同步
            OurHist
    end.

%%==============================================================================
%% 内部函数 - 延迟统计更新
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 更新 PCB 中的延迟统计信息
%%
%% 根据 our_delay 是否有效，选择不同的更新路径：
%% - our_delay > 0: 更新完整统计（包括 average_delay 和 clock_drift）
%% - our_delay = 0: 只更新基本字段
%%------------------------------------------------------------------------------
-spec update_delay_statistics(integer(), non_neg_integer(), non_neg_integer(),
                              aiutp_delay:delay(), aiutp_delay:delay(),
                              #aiutp_pcb{}) -> #aiutp_pcb{}.
update_delay_statistics(Now, TheirDelay, 0, TheirHist, _OurHist, PCB) ->
    %% our_delay 无效，只更新基本字段
    PCB#aiutp_pcb{
        reply_micro = TheirDelay,
        last_measured_delay = Now,
        their_hist = TheirHist
    };
update_delay_statistics(Now, TheirDelay, OurDelay, TheirHist, OurHist, PCB) ->
    %% our_delay 有效，更新完整统计
    OurHist1 = aiutp_delay:add_sample(OurDelay, Now, OurHist),

    %% 更新 average_delay 相关统计
    update_average_delay_stats(Now, TheirDelay, OurDelay, TheirHist, OurHist1, PCB).

%%------------------------------------------------------------------------------
%% @private
%% @doc 更新 average_delay 统计
%%
%% average_delay 用于检测时钟漂移：
%% 1. 累积延迟样本（相对于 average_delay_base 的偏差）
%% 2. 每隔 AVERAGE_DELAY_SAMPLE_INTERVAL 计算一次平均值
%% 3. 通过 average_delay 的变化估计 clock_drift
%%------------------------------------------------------------------------------
-spec update_average_delay_stats(integer(), non_neg_integer(), non_neg_integer(),
                                  aiutp_delay:delay(), aiutp_delay:delay(),
                                  #aiutp_pcb{}) -> #aiutp_pcb{}.
update_average_delay_stats(Now, TheirDelay, OurDelay, TheirHist, OurHist,
                           #aiutp_pcb{average_delay_base = AverageDelayBase,
                                      current_delay_sum = CurrentDelaySum,
                                      current_delay_samples = CurrentDelaySamples,
                                      average_delay = PrevAverageDelay,
                                      average_sample_time = AverageSampleTime,
                                      clock_drift = ClockDrift} = PCB) ->

    %% 初始化或使用现有的 average_delay_base
    AverageDelayBase1 = case AverageDelayBase of
        0 -> OurDelay;
        _ -> AverageDelayBase
    end,

    %% 计算当前样本相对于基准的偏差
    DelaySample = compute_delay_sample(OurDelay, AverageDelayBase1),

    %% 累积样本
    CurrentDelaySum1 = CurrentDelaySum + DelaySample,

    %% 检查是否到了计算 average_delay 的时间
    case Now > AverageSampleTime of
        true ->
            %% 计算新的 average_delay
            {AverageDelayBase2, AverageDelay, ClockDrift1} =
                compute_average_delay(CurrentDelaySum1, CurrentDelaySamples + 1,
                                      AverageDelayBase1, PrevAverageDelay, ClockDrift),
            PCB#aiutp_pcb{
                reply_micro = TheirDelay,
                last_measured_delay = Now,
                their_hist = TheirHist,
                our_hist = OurHist,
                average_delay_base = AverageDelayBase2,
                average_delay = AverageDelay,
                average_sample_time = AverageSampleTime + ?AVERAGE_DELAY_SAMPLE_INTERVAL,
                current_delay_sum = 0,
                current_delay_samples = 0,
                clock_drift = ClockDrift1
            };
        false ->
            %% 继续累积样本
            PCB#aiutp_pcb{
                reply_micro = TheirDelay,
                last_measured_delay = Now,
                their_hist = TheirHist,
                our_hist = OurHist,
                average_delay_base = AverageDelayBase1,
                current_delay_sum = CurrentDelaySum1,
                current_delay_samples = CurrentDelaySamples + 1
            }
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算延迟样本的偏差值
%%
%% 计算 OurDelay 相对于 AverageDelayBase 的有符号偏差。
%% 使用距离比较来处理环绕情况。
%%------------------------------------------------------------------------------
-spec compute_delay_sample(non_neg_integer(), non_neg_integer()) -> integer().
compute_delay_sample(OurDelay, AverageDelayBase) ->
    DistDown = AverageDelayBase - OurDelay,
    DistUp = OurDelay - AverageDelayBase,
    case DistDown > DistUp of
        true -> DistUp;       %% OurDelay > AverageDelayBase
        false -> -DistDown    %% OurDelay <= AverageDelayBase
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算 average_delay 并更新 clock_drift
%%
%% 1. 计算累积样本的平均值
%% 2. 调整 average_delay_base 使 average_delay 接近 0
%% 3. 通过 EWMA 更新 clock_drift
%%------------------------------------------------------------------------------
-spec compute_average_delay(integer(), pos_integer(), non_neg_integer(),
                            integer(), integer()) ->
    {non_neg_integer(), integer(), integer()}.
compute_average_delay(CurrentDelaySum, SampleCount, AverageDelayBase,
                      PrevAverageDelay, ClockDrift) ->
    %% 计算平均延迟偏差
    AverageDelay = CurrentDelaySum div SampleCount,

    %% 计算基准调整值，使 average_delay 保持在 0 附近
    %% Reason: 这样可以更精确地检测时钟漂移
    Adjust = compute_base_adjust(PrevAverageDelay, AverageDelay),

    %% 应用调整
    {AverageDelayBase1, AverageDelay1} = case Adjust of
        0 -> {AverageDelayBase, AverageDelay};
        _ -> {AverageDelayBase - Adjust, AverageDelay + Adjust}
    end,

    %% 更新时钟漂移估计 (EWMA, alpha = 1/8)
    %% clock_drift 表示每 5 秒的时钟偏移量
    ClockDrift1 = (ClockDrift * 7 + (AverageDelay1 - PrevAverageDelay)) div 8,

    {AverageDelayBase1, AverageDelay1, ClockDrift1}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 计算 average_delay_base 的调整值
%%
%% 如果 average_delay 和 prev_average_delay 同号（都为正或都为负），
%% 调整基准使其回到 0 附近。
%%------------------------------------------------------------------------------
-spec compute_base_adjust(integer(), integer()) -> integer().
compute_base_adjust(PrevAverageDelay, AverageDelay) ->
    MinSample = erlang:min(PrevAverageDelay, AverageDelay),
    MaxSample = erlang:max(PrevAverageDelay, AverageDelay),
    if
        MinSample > 0 ->
            %% 两者都为正，向下调整
            -MinSample;
        MaxSample < 0 ->
            %% 两者都为负，向上调整
            -MaxSample;
        true ->
            %% 符号不同或有零，不调整
            0
    end.
