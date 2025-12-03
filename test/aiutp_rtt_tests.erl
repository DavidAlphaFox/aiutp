-module(aiutp_rtt_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%====================================================================
%% Test: caculate_rtt/4 - Initial RTT (RTT = 0)
%%====================================================================

caculate_rtt_initial_test() ->
    %% 当 RTT = 0 时（首次计算）
    TimeSent = 1000000,  %% 发送时间（微秒）
    MicroNow = 1100000,  %% 当前时间（微秒）
    %% 预期 ERTT = (1100000 - 1000000) / 1000 = 100 ms

    {RTT, RTTVar, ERTT} = aiutp_rtt:caculate_rtt(0, 0, TimeSent, MicroNow),

    ?assertEqual(100, ERTT),
    ?assertEqual(100, RTT),      %% 首次计算时 RTT = ERTT
    ?assertEqual(50, RTTVar).    %% 首次计算时 RTTVar = ERTT / 2

%%====================================================================
%% Test: caculate_rtt/4 - Subsequent RTT calculations
%%====================================================================

caculate_rtt_subsequent_test() ->
    %% 已有 RTT 值时的更新
    TimeSent = 2000000,
    MicroNow = 2080000,  %% ERTT = 80 ms
    OldRTT = 100,
    OldRTTVar = 50,

    {RTT, RTTVar, ERTT} = aiutp_rtt:caculate_rtt(OldRTT, OldRTTVar, TimeSent, MicroNow),

    ?assertEqual(80, ERTT),
    %% RTT = (OldRTT * 7 + ERTT) / 8 = (100 * 7 + 80) / 8 = 97.5 ≈ 97
    ?assertEqual(97, RTT),
    %% RTTVar = RTTVar + (|Delta| - RTTVar) / 4
    %% Delta = 100 - 80 = 20
    %% RTTVar = 50 + (20 - 50) / 4 = 50 - 7 = 43 (integer division)
    ?assertEqual(43, RTTVar).

caculate_rtt_increasing_delay_test() ->
    %% RTT 增加的情况
    TimeSent = 3000000,
    MicroNow = 3150000,  %% ERTT = 150 ms
    OldRTT = 100,
    OldRTTVar = 25,

    {RTT, RTTVar, ERTT} = aiutp_rtt:caculate_rtt(OldRTT, OldRTTVar, TimeSent, MicroNow),

    ?assertEqual(150, ERTT),
    %% RTT 应该增加
    ?assert(RTT > OldRTT),
    %% RTTVar 也应该因为更大的 Delta 而调整
    ?assert(RTTVar /= OldRTTVar).

caculate_rtt_decreasing_delay_test() ->
    %% RTT 减少的情况
    TimeSent = 4000000,
    MicroNow = 4030000,  %% ERTT = 30 ms
    OldRTT = 100,
    OldRTTVar = 25,

    {RTT, RTTVar, ERTT} = aiutp_rtt:caculate_rtt(OldRTT, OldRTTVar, TimeSent, MicroNow),

    ?assertEqual(30, ERTT),
    %% RTT 应该减少但平滑
    ?assert(RTT < OldRTT),
    ?assert(RTT > ERTT). %% 由于平滑，RTT 不会立即降到 ERTT

%%====================================================================
%% Test: RTT smoothing behavior
%%====================================================================

caculate_rtt_smoothing_test() ->
    %% 测试 RTT 平滑：相同的测量值应该使 RTT 趋向稳定
    TimeSent = 5000000,
    MicroNow = 5100000,  %% ERTT = 100 ms
    OldRTT = 100,
    OldRTTVar = 25,

    {RTT, RTTVar, ERTT} = aiutp_rtt:caculate_rtt(OldRTT, OldRTTVar, TimeSent, MicroNow),

    ?assertEqual(100, ERTT),
    ?assertEqual(100, RTT),  %% RTT 应该保持不变
    %% RTTVar 应该减少（因为 Delta = 0）
    ?assert(RTTVar < OldRTTVar).

%%====================================================================
%% Test: Edge cases
%%====================================================================

caculate_rtt_zero_delay_test() ->
    %% 零延迟情况（理论上不可能，但测试边界）
    TimeSent = 6000000,
    MicroNow = 6000000,  %% ERTT = 0 ms

    {RTT, RTTVar, ERTT} = aiutp_rtt:caculate_rtt(0, 0, TimeSent, MicroNow),

    ?assertEqual(0, ERTT),
    ?assertEqual(0, RTT),
    ?assertEqual(0, RTTVar).

caculate_rtt_large_delay_test() ->
    %% 大延迟情况
    TimeSent = 7000000,
    MicroNow = 12000000,  %% ERTT = 5000 ms

    {RTT, _RTTVar, ERTT} = aiutp_rtt:caculate_rtt(0, 0, TimeSent, MicroNow),

    ?assertEqual(5000, ERTT),
    ?assertEqual(5000, RTT).

%%====================================================================
%% Test: Sequence of RTT updates
%%====================================================================

caculate_rtt_sequence_test() ->
    %% 模拟一系列 RTT 更新，验证收敛行为
    InitialRTT = 0,
    InitialVar = 0,

    %% 第一次测量：100ms
    {RTT1, Var1, _} = aiutp_rtt:caculate_rtt(InitialRTT, InitialVar, 0, 100000),
    ?assertEqual(100, RTT1),

    %% 第二次测量：120ms
    {RTT2, Var2, _} = aiutp_rtt:caculate_rtt(RTT1, Var1, 0, 120000),
    ?assert(RTT2 > RTT1),

    %% 第三次测量：80ms
    {RTT3, _Var3, _} = aiutp_rtt:caculate_rtt(RTT2, Var2, 0, 80000),
    ?assert(RTT3 < RTT2),

    %% RTT 应该在合理范围内
    ?assert(RTT3 >= 80),
    ?assert(RTT3 =< 120).

%%====================================================================
%% Test: RTTVar behavior (variance tracking)
%%====================================================================

caculate_rtt_var_convergence_test() ->
    %% 稳定的 RTT 应该使 RTTVar 趋向于 0
    RTT = 100,
    Var0 = 50,

    %% 多次相同测量
    {_, Var1, _} = aiutp_rtt:caculate_rtt(RTT, Var0, 0, 100000),
    {_, Var2, _} = aiutp_rtt:caculate_rtt(RTT, Var1, 0, 100000),
    {_, Var3, _} = aiutp_rtt:caculate_rtt(RTT, Var2, 0, 100000),
    {_, Var4, _} = aiutp_rtt:caculate_rtt(RTT, Var3, 0, 100000),

    %% Variance 应该逐渐减小
    ?assert(Var1 < Var0),
    ?assert(Var2 < Var1),
    ?assert(Var3 < Var2),
    ?assert(Var4 < Var3).

caculate_rtt_var_increase_on_jitter_test() ->
    %% 抖动应该增加 RTTVar
    RTT = 100,
    Var0 = 10,

    %% 测量值与 RTT 差异较大
    {_, Var1, _} = aiutp_rtt:caculate_rtt(RTT, Var0, 0, 200000), %% ERTT = 200
    ?assert(Var1 > Var0).
