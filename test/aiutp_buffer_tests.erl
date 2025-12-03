-module(aiutp_buffer_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: new/1
%%====================================================================

new_creates_buffer_with_size_test() ->
    B = aiutp_buffer:new(10),
    ?assertEqual(10, aiutp_buffer:size(B)),
    ?assertEqual(0, aiutp_buffer:used(B)),
    ?assertEqual(10, aiutp_buffer:unused(B)).

new_different_sizes_test() ->
    B1 = aiutp_buffer:new(5),
    ?assertEqual(5, aiutp_buffer:size(B1)),

    B2 = aiutp_buffer:new(100),
    ?assertEqual(100, aiutp_buffer:size(B2)).

%%====================================================================
%% Test: head/1 and tail/1
%%====================================================================

head_tail_empty_buffer_test() ->
    B = aiutp_buffer:new(10),
    %% 空缓冲区的 head 和 tail 都应该是 -1
    ?assertEqual(-1, aiutp_buffer:head(B)),
    ?assertEqual(-1, aiutp_buffer:tail(B)).

%%====================================================================
%% Test: append/2
%%====================================================================

append_single_element_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(value1, B0),
    ?assertEqual(1, aiutp_buffer:used(B1)),
    ?assertEqual(9, aiutp_buffer:unused(B1)),
    Head = aiutp_buffer:head(B1),
    ?assertEqual(value1, aiutp_buffer:data(Head, B1)).

append_multiple_elements_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(a, B0),
    B2 = aiutp_buffer:append(b, B1),
    B3 = aiutp_buffer:append(c, B2),
    ?assertEqual(3, aiutp_buffer:used(B3)),
    ?assertEqual(7, aiutp_buffer:unused(B3)).

append_to_full_buffer_test() ->
    B0 = aiutp_buffer:new(3),
    B1 = aiutp_buffer:append(1, B0),
    B2 = aiutp_buffer:append(2, B1),
    B3 = aiutp_buffer:append(3, B2),
    ?assertEqual(0, aiutp_buffer:unused(B3)),
    %% 尝试添加到满缓冲区应该返回错误
    Result = aiutp_buffer:append(4, B3),
    ?assertEqual({error, buffer_overflow}, Result).

%%====================================================================
%% Test: insert/3
%%====================================================================

insert_at_head_test() ->
    B0 = aiutp_buffer:new(10),
    %% Prev = -1 表示插入到头部
    B1 = aiutp_buffer:insert(-1, first, B0),
    ?assertEqual(1, aiutp_buffer:used(B1)),
    Head = aiutp_buffer:head(B1),
    ?assertEqual(first, aiutp_buffer:data(Head, B1)).

insert_after_element_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(a, B0),
    Head = aiutp_buffer:head(B1),
    %% 在 head 之后插入
    B2 = aiutp_buffer:insert(Head, b, B1),
    ?assertEqual(2, aiutp_buffer:used(B2)),
    %% 验证链表结构
    Next = aiutp_buffer:next(Head, B2),
    ?assertEqual(b, aiutp_buffer:data(Next, B2)).

%%====================================================================
%% Test: replace/3
%%====================================================================

replace_value_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(old_value, B0),
    Head = aiutp_buffer:head(B1),
    B2 = aiutp_buffer:replace(Head, new_value, B1),
    ?assertEqual(new_value, aiutp_buffer:data(Head, B2)),
    %% 大小不变
    ?assertEqual(1, aiutp_buffer:used(B2)).

%%====================================================================
%% Test: pop/1
%%====================================================================

pop_single_element_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(only, B0),
    B2 = aiutp_buffer:pop(B1),
    ?assertEqual(0, aiutp_buffer:used(B2)),
    ?assertEqual(10, aiutp_buffer:unused(B2)),
    ?assertEqual(-1, aiutp_buffer:head(B2)).

pop_from_multiple_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(first, B0),
    B2 = aiutp_buffer:append(second, B1),
    B3 = aiutp_buffer:append(third, B2),

    %% pop 移除头部元素
    B4 = aiutp_buffer:pop(B3),
    ?assertEqual(2, aiutp_buffer:used(B4)),
    Head = aiutp_buffer:head(B4),
    ?assertEqual(second, aiutp_buffer:data(Head, B4)).

%%====================================================================
%% Test: delete/3
%%====================================================================

delete_head_element_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(a, B0),
    B2 = aiutp_buffer:append(b, B1),
    Head = aiutp_buffer:head(B2),
    %% 删除头部元素（Prev = -1）
    B3 = aiutp_buffer:delete(Head, -1, B2),
    ?assertEqual(1, aiutp_buffer:used(B3)),
    NewHead = aiutp_buffer:head(B3),
    ?assertEqual(b, aiutp_buffer:data(NewHead, B3)).

delete_middle_element_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(a, B0),
    B2 = aiutp_buffer:append(b, B1),
    B3 = aiutp_buffer:append(c, B2),

    Head = aiutp_buffer:head(B3),
    Middle = aiutp_buffer:next(Head, B3),
    %% 删除中间元素
    B4 = aiutp_buffer:delete(Middle, Head, B3),
    ?assertEqual(2, aiutp_buffer:used(B4)),
    %% 验证链表正确链接：head -> c
    NewNext = aiutp_buffer:next(Head, B4),
    ?assertEqual(c, aiutp_buffer:data(NewNext, B4)).

delete_tail_element_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(a, B0),
    B2 = aiutp_buffer:append(b, B1),

    Head = aiutp_buffer:head(B2),
    Tail = aiutp_buffer:tail(B2),
    %% 删除尾部元素
    B3 = aiutp_buffer:delete(Tail, Head, B2),
    ?assertEqual(1, aiutp_buffer:used(B3)),
    ?assertEqual(Head, aiutp_buffer:tail(B3)).

%%====================================================================
%% Test: next/2
%%====================================================================

next_traversal_test() ->
    B0 = aiutp_buffer:new(10),
    B1 = aiutp_buffer:append(1, B0),
    B2 = aiutp_buffer:append(2, B1),
    B3 = aiutp_buffer:append(3, B2),

    %% 遍历链表
    Head = aiutp_buffer:head(B3),
    ?assertEqual(1, aiutp_buffer:data(Head, B3)),

    Second = aiutp_buffer:next(Head, B3),
    ?assertEqual(2, aiutp_buffer:data(Second, B3)),

    Third = aiutp_buffer:next(Second, B3),
    ?assertEqual(3, aiutp_buffer:data(Third, B3)),

    %% 最后一个元素的 next 是 -1
    ?assertEqual(-1, aiutp_buffer:next(Third, B3)).

%%====================================================================
%% Test: Size tracking
%%====================================================================

size_tracking_comprehensive_test() ->
    B0 = aiutp_buffer:new(5),
    ?assertEqual(5, aiutp_buffer:size(B0)),
    ?assertEqual(0, aiutp_buffer:used(B0)),
    ?assertEqual(5, aiutp_buffer:unused(B0)),

    %% 添加元素
    B1 = aiutp_buffer:append(a, B0),
    ?assertEqual(1, aiutp_buffer:used(B1)),
    ?assertEqual(4, aiutp_buffer:unused(B1)),

    B2 = aiutp_buffer:append(b, B1),
    ?assertEqual(2, aiutp_buffer:used(B2)),
    ?assertEqual(3, aiutp_buffer:unused(B2)),

    %% 删除元素
    B3 = aiutp_buffer:pop(B2),
    ?assertEqual(1, aiutp_buffer:used(B3)),
    ?assertEqual(4, aiutp_buffer:unused(B3)),

    %% 总大小始终不变
    ?assertEqual(5, aiutp_buffer:size(B3)).

%%====================================================================
%% Test: Reuse of slots after deletion
%%====================================================================

slot_reuse_test() ->
    B0 = aiutp_buffer:new(3),
    B1 = aiutp_buffer:append(a, B0),
    B2 = aiutp_buffer:append(b, B1),
    B3 = aiutp_buffer:append(c, B2),
    ?assertEqual(0, aiutp_buffer:unused(B3)),

    %% 删除一个元素
    B4 = aiutp_buffer:pop(B3),
    ?assertEqual(1, aiutp_buffer:unused(B4)),

    %% 现在可以添加新元素
    B5 = aiutp_buffer:append(d, B4),
    ?assertEqual(0, aiutp_buffer:unused(B5)),
    ?assertEqual(3, aiutp_buffer:used(B5)).
