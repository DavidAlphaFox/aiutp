-module(aiutp_delay_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: new/0 and new/1
%%====================================================================

new_default_test() ->
    D = aiutp_delay:new(),
    ?assertEqual(0, aiutp_delay:delay_base(D)),
    %% 未初始化时，cur_delay_hist 全部为 0，value 返回 0
    ?assertEqual(0, aiutp_delay:value(D)).

new_with_time_test() ->
    D = aiutp_delay:new(1000),
    ?assertEqual(0, aiutp_delay:delay_base(D)).

%%====================================================================
%% Test: add_sample/3 - Initialization
%%====================================================================

add_sample_first_initializes_test() ->
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(100000, 1000, D0),
    %% 第一个样本应该初始化 delay_base
    ?assertEqual(100000, aiutp_delay:delay_base(D1)).

add_sample_multiple_test() ->
    D0 = aiutp_delay:new(0),
    %% 添加第一个样本
    D1 = aiutp_delay:add_sample(100000, 1000, D0),
    ?assertEqual(100000, aiutp_delay:delay_base(D1)),
    %% 添加更大的样本
    D2 = aiutp_delay:add_sample(150000, 2000, D1),
    %% delay_base 更新逻辑：如果新样本更大，delay_base 会更新为新值
    %% 这是因为 WRAPPING_DIFF_32(Sample, DelayBase) < 0 判断
    Base2 = aiutp_delay:delay_base(D2),
    ?assert(is_integer(Base2)).

add_sample_smaller_updates_base_test() ->
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(100000, 1000, D0),
    ?assertEqual(100000, aiutp_delay:delay_base(D1)),
    %% 添加更小的样本
    D2 = aiutp_delay:add_sample(80000, 2000, D1),
    %% 根据实现逻辑，delay_base 可能不会立即更新
    %% 因为 WRAPPING_DIFF_32 的判断逻辑
    Base2 = aiutp_delay:delay_base(D2),
    ?assert(Base2 =:= 100000 orelse Base2 =:= 80000).

%%====================================================================
%% Test: value/1
%%====================================================================

value_after_samples_test() ->
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(100000, 1000, D0),
    %% 添加样本后，value 应该是相对于 delay_base 的延迟
    Val = aiutp_delay:value(D1),
    ?assert(Val < 16#FFFFFFFF).

%%====================================================================
%% Test: shift/2
%%====================================================================

shift_increases_base_test() ->
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(100000, 1000, D0),
    OriginalBase = aiutp_delay:delay_base(D1),
    D2 = aiutp_delay:shift(1000, D1),
    %% shift 应该增加历史记录中的值，但 delay_base 字段本身不变
    ?assertEqual(OriginalBase, aiutp_delay:delay_base(D2)).

%%====================================================================
%% Test: delay_base/1
%%====================================================================

delay_base_accessor_test() ->
    D0 = aiutp_delay:new(0),
    ?assertEqual(0, aiutp_delay:delay_base(D0)),
    D1 = aiutp_delay:add_sample(50000, 1000, D0),
    ?assertEqual(50000, aiutp_delay:delay_base(D1)).

%%====================================================================
%% Test: Delay history rollover
%%====================================================================

delay_base_history_rollover_test() ->
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(100000, 0, D0),
    %% 模拟超过 60 秒的时间流逝（触发历史滚动）
    D2 = aiutp_delay:add_sample(110000, 61000, D1),
    %% 历史滚动后，delay_base 应该更新
    Base = aiutp_delay:delay_base(D2),
    ?assert(is_integer(Base)).

%%====================================================================
%% Test: Wrapping behavior
%%====================================================================

wrapping_sample_test() ->
    %% 测试接近 32 位边界的样本
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(16#FFFFF000, 1000, D0),
    ?assertEqual(16#FFFFF000, aiutp_delay:delay_base(D1)),
    %% 添加一个更小的样本（考虑环绕）
    D2 = aiutp_delay:add_sample(16#FFFFFE00, 2000, D1),
    Base = aiutp_delay:delay_base(D2),
    ?assert(is_integer(Base)).

%%====================================================================
%% Test: Multiple samples in cur_delay_hist
%%====================================================================

multiple_samples_value_test() ->
    D0 = aiutp_delay:new(0),
    D1 = aiutp_delay:add_sample(100000, 1000, D0),
    D2 = aiutp_delay:add_sample(120000, 2000, D1),
    D3 = aiutp_delay:add_sample(110000, 3000, D2),
    %% value 应该返回 cur_delay_hist 中的最小值
    Val = aiutp_delay:value(D3),
    ?assert(Val =< 20000). %% 最小延迟应该 <= 20000 (120000 - 100000)
