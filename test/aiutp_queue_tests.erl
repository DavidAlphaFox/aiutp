-module(aiutp_queue_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: new/0
%%====================================================================

new_creates_empty_queue_test() ->
    Q = aiutp_queue:new(),
    ?assertEqual(0, aiutp_queue:size(Q)),
    ?assertEqual(true, aiutp_queue:empty(Q)).

%%====================================================================
%% Test: push_back/2 and push_front/2
%%====================================================================

push_back_single_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(1, Q0),
    ?assertEqual(1, aiutp_queue:size(Q1)),
    ?assertEqual(false, aiutp_queue:empty(Q1)),
    ?assertEqual(1, aiutp_queue:front(Q1)),
    ?assertEqual(1, aiutp_queue:back(Q1)).

push_back_multiple_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(1, Q0),
    Q2 = aiutp_queue:push_back(2, Q1),
    Q3 = aiutp_queue:push_back(3, Q2),
    ?assertEqual(3, aiutp_queue:size(Q3)),
    ?assertEqual(1, aiutp_queue:front(Q3)),
    ?assertEqual(3, aiutp_queue:back(Q3)),
    ?assertEqual([1, 2, 3], aiutp_queue:to_list(Q3)).

push_front_single_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_front(1, Q0),
    ?assertEqual(1, aiutp_queue:size(Q1)),
    ?assertEqual(1, aiutp_queue:front(Q1)),
    ?assertEqual(1, aiutp_queue:back(Q1)).

push_front_multiple_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_front(1, Q0),
    Q2 = aiutp_queue:push_front(2, Q1),
    Q3 = aiutp_queue:push_front(3, Q2),
    ?assertEqual(3, aiutp_queue:size(Q3)),
    ?assertEqual(3, aiutp_queue:front(Q3)),
    ?assertEqual(1, aiutp_queue:back(Q3)),
    ?assertEqual([3, 2, 1], aiutp_queue:to_list(Q3)).

push_mixed_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(2, Q0),
    Q2 = aiutp_queue:push_front(1, Q1),
    Q3 = aiutp_queue:push_back(3, Q2),
    ?assertEqual([1, 2, 3], aiutp_queue:to_list(Q3)).

%%====================================================================
%% Test: pop_front/1 and pop_back/1
%%====================================================================

pop_front_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(1, Q0),
    Q2 = aiutp_queue:push_back(2, Q1),
    Q3 = aiutp_queue:push_back(3, Q2),

    Q4 = aiutp_queue:pop_front(Q3),
    ?assertEqual(2, aiutp_queue:size(Q4)),
    ?assertEqual(2, aiutp_queue:front(Q4)),
    ?assertEqual([2, 3], aiutp_queue:to_list(Q4)).

pop_back_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(1, Q0),
    Q2 = aiutp_queue:push_back(2, Q1),
    Q3 = aiutp_queue:push_back(3, Q2),

    Q4 = aiutp_queue:pop_back(Q3),
    ?assertEqual(2, aiutp_queue:size(Q4)),
    ?assertEqual(2, aiutp_queue:back(Q4)),
    ?assertEqual([1, 2], aiutp_queue:to_list(Q4)).

pop_to_empty_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(1, Q0),
    Q2 = aiutp_queue:pop_front(Q1),
    ?assertEqual(0, aiutp_queue:size(Q2)),
    ?assertEqual(true, aiutp_queue:empty(Q2)).

%%====================================================================
%% Test: front/1 and back/1
%%====================================================================

front_back_consistency_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(only_element, Q0),
    %% 只有一个元素时，front 和 back 相同
    ?assertEqual(only_element, aiutp_queue:front(Q1)),
    ?assertEqual(only_element, aiutp_queue:back(Q1)).

%%====================================================================
%% Test: size/1 and empty/1
%%====================================================================

size_tracks_correctly_test() ->
    Q0 = aiutp_queue:new(),
    ?assertEqual(0, aiutp_queue:size(Q0)),

    Q1 = aiutp_queue:push_back(a, Q0),
    ?assertEqual(1, aiutp_queue:size(Q1)),

    Q2 = aiutp_queue:push_back(b, Q1),
    ?assertEqual(2, aiutp_queue:size(Q2)),

    Q3 = aiutp_queue:pop_front(Q2),
    ?assertEqual(1, aiutp_queue:size(Q3)),

    Q4 = aiutp_queue:pop_back(Q3),
    ?assertEqual(0, aiutp_queue:size(Q4)).

empty_returns_correct_value_test() ->
    Q0 = aiutp_queue:new(),
    ?assertEqual(true, aiutp_queue:empty(Q0)),

    Q1 = aiutp_queue:push_back(x, Q0),
    ?assertEqual(false, aiutp_queue:empty(Q1)),

    Q2 = aiutp_queue:pop_front(Q1),
    ?assertEqual(true, aiutp_queue:empty(Q2)).

%%====================================================================
%% Test: to_list/1
%%====================================================================

to_list_empty_test() ->
    Q = aiutp_queue:new(),
    ?assertEqual([], aiutp_queue:to_list(Q)).

to_list_preserves_order_test() ->
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(1, Q0),
    Q2 = aiutp_queue:push_back(2, Q1),
    Q3 = aiutp_queue:push_back(3, Q2),
    Q4 = aiutp_queue:push_back(4, Q3),
    Q5 = aiutp_queue:push_back(5, Q4),
    ?assertEqual([1, 2, 3, 4, 5], aiutp_queue:to_list(Q5)).

%%====================================================================
%% Test: Complex operations
%%====================================================================

fifo_behavior_test() ->
    %% 验证 FIFO 行为：push_back + pop_front
    Q0 = aiutp_queue:new(),
    Q1 = lists:foldl(fun(N, Q) -> aiutp_queue:push_back(N, Q) end,
                     Q0, [1, 2, 3, 4, 5]),

    %% 依次弹出，应该按 1, 2, 3, 4, 5 的顺序
    ?assertEqual(1, aiutp_queue:front(Q1)),
    Q2 = aiutp_queue:pop_front(Q1),
    ?assertEqual(2, aiutp_queue:front(Q2)),
    Q3 = aiutp_queue:pop_front(Q2),
    ?assertEqual(3, aiutp_queue:front(Q3)).

lifo_behavior_test() ->
    %% 验证 LIFO 行为：push_back + pop_back
    Q0 = aiutp_queue:new(),
    Q1 = lists:foldl(fun(N, Q) -> aiutp_queue:push_back(N, Q) end,
                     Q0, [1, 2, 3, 4, 5]),

    %% 从后面弹出，应该按 5, 4, 3... 的顺序
    ?assertEqual(5, aiutp_queue:back(Q1)),
    Q2 = aiutp_queue:pop_back(Q1),
    ?assertEqual(4, aiutp_queue:back(Q2)),
    Q3 = aiutp_queue:pop_back(Q2),
    ?assertEqual(3, aiutp_queue:back(Q3)).

deque_operations_test() ->
    %% 双端队列操作测试
    Q0 = aiutp_queue:new(),
    Q1 = aiutp_queue:push_back(middle, Q0),
    Q2 = aiutp_queue:push_front(first, Q1),
    Q3 = aiutp_queue:push_back(last, Q2),

    ?assertEqual([first, middle, last], aiutp_queue:to_list(Q3)),
    ?assertEqual(first, aiutp_queue:front(Q3)),
    ?assertEqual(last, aiutp_queue:back(Q3)).
