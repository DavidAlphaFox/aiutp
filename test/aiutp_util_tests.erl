-module(aiutp_util_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: bit16/1
%%====================================================================

bit16_zero_test() ->
    ?assertEqual(0, aiutp_util:bit16(0)).

bit16_within_range_test() ->
    ?assertEqual(1234, aiutp_util:bit16(1234)),
    ?assertEqual(16#FFFF, aiutp_util:bit16(16#FFFF)).

bit16_overflow_test() ->
    %% 测试超过16位的值会被截断
    ?assertEqual(0, aiutp_util:bit16(16#10000)),
    ?assertEqual(1, aiutp_util:bit16(16#10001)),
    ?assertEqual(16#1234, aiutp_util:bit16(16#FF1234)).

bit16_negative_test() ->
    %% 测试负数的位运算
    ?assertEqual(16#FFFF, aiutp_util:bit16(-1)),
    ?assertEqual(16#FFFE, aiutp_util:bit16(-2)).

%%====================================================================
%% Test: bit32/1
%%====================================================================

bit32_zero_test() ->
    ?assertEqual(0, aiutp_util:bit32(0)).

bit32_within_range_test() ->
    ?assertEqual(12345678, aiutp_util:bit32(12345678)),
    ?assertEqual(16#FFFFFFFF, aiutp_util:bit32(16#FFFFFFFF)).

bit32_overflow_test() ->
    %% 测试超过32位的值会被截断
    ?assertEqual(0, aiutp_util:bit32(16#100000000)),
    ?assertEqual(1, aiutp_util:bit32(16#100000001)),
    ?assertEqual(16#12345678, aiutp_util:bit32(16#FF12345678)).

bit32_negative_test() ->
    %% 测试负数的位运算
    ?assertEqual(16#FFFFFFFF, aiutp_util:bit32(-1)),
    ?assertEqual(16#FFFFFFFE, aiutp_util:bit32(-2)).

%%====================================================================
%% Test: clamp/3
%%====================================================================

clamp_within_range_test() ->
    ?assertEqual(50, aiutp_util:clamp(50, 0, 100)),
    ?assertEqual(0, aiutp_util:clamp(0, 0, 100)),
    ?assertEqual(100, aiutp_util:clamp(100, 0, 100)).

clamp_below_min_test() ->
    ?assertEqual(0, aiutp_util:clamp(-10, 0, 100)),
    ?assertEqual(10, aiutp_util:clamp(5, 10, 100)).

clamp_above_max_test() ->
    ?assertEqual(100, aiutp_util:clamp(150, 0, 100)),
    ?assertEqual(50, aiutp_util:clamp(100, 0, 50)).

%%====================================================================
%% Test: wrapping_compare_less/3
%%====================================================================

wrapping_compare_less_simple_test() ->
    Mask = 16#FFFF,
    %% 简单比较：1 < 2
    ?assertEqual(true, aiutp_util:wrapping_compare_less(1, 2, Mask)),
    %% 简单比较：2 > 1
    ?assertEqual(false, aiutp_util:wrapping_compare_less(2, 1, Mask)),
    %% 相等时不是小于
    ?assertEqual(false, aiutp_util:wrapping_compare_less(5, 5, Mask)).

wrapping_compare_less_wrap_around_test() ->
    Mask = 16#FFFF,
    %% 绕回情况：65535 < 0 (在模算术中，0 在 65535 之后)
    ?assertEqual(true, aiutp_util:wrapping_compare_less(16#FFFF, 0, Mask)),
    %% 65535 < 1
    ?assertEqual(true, aiutp_util:wrapping_compare_less(16#FFFF, 1, Mask)),
    %% 0 > 65535 (0 在 65535 之后)
    ?assertEqual(false, aiutp_util:wrapping_compare_less(0, 16#FFFF, Mask)).

wrapping_compare_less_32bit_test() ->
    Mask = 16#FFFFFFFF,
    ?assertEqual(true, aiutp_util:wrapping_compare_less(16#FFFFFFFF, 0, Mask)),
    ?assertEqual(false, aiutp_util:wrapping_compare_less(0, 16#FFFFFFFF, Mask)).

%%====================================================================
%% Test: getaddr/1
%%====================================================================

getaddr_tuple_test() ->
    %% 元组格式直接返回
    ?assertEqual({127, 0, 0, 1}, aiutp_util:getaddr({127, 0, 0, 1})),
    ?assertEqual({192, 168, 1, 1}, aiutp_util:getaddr({192, 168, 1, 1})).

getaddr_localhost_test() ->
    %% localhost 应该解析为 127.0.0.1
    ?assertEqual({127, 0, 0, 1}, aiutp_util:getaddr("localhost")).

%%====================================================================
%% Test: time functions
%%====================================================================

microsecond_test() ->
    T1 = aiutp_util:microsecond(),
    timer:sleep(1),
    T2 = aiutp_util:microsecond(),
    ?assert(is_integer(T1)),
    ?assert(T2 > T1).

millisecond_test() ->
    T1 = aiutp_util:millisecond(),
    timer:sleep(10),
    T2 = aiutp_util:millisecond(),
    ?assert(is_integer(T1)),
    ?assert(T2 >= T1).

%%====================================================================
%% Test: random functions
%%====================================================================

bit16_random_range_test() ->
    %% 生成多个随机数，验证都在16位范围内
    Results = [aiutp_util:bit16_random() || _ <- lists:seq(1, 100)],
    lists:foreach(fun(N) ->
        ?assert(N >= 0),
        ?assert(N =< 16#FFFF)
    end, Results).

bit32_random_range_test() ->
    %% 生成多个随机数，验证都在32位范围内
    Results = [aiutp_util:bit32_random() || _ <- lists:seq(1, 100)],
    lists:foreach(fun(N) ->
        ?assert(N >= 0),
        ?assert(N =< 16#FFFFFFFF)
    end, Results).

bit16_random_uniqueness_test() ->
    %% 验证随机数不全相同（概率测试）
    Results = [aiutp_util:bit16_random() || _ <- lists:seq(1, 10)],
    UniqueCount = length(lists:usort(Results)),
    ?assert(UniqueCount > 1).

bit32_random_uniqueness_test() ->
    %% 验证随机数不全相同（概率测试）
    Results = [aiutp_util:bit32_random() || _ <- lists:seq(1, 10)],
    UniqueCount = length(lists:usort(Results)),
    ?assert(UniqueCount > 1).
