-module(run_bench).
-export([run/0]).

-include("../include/aiutp.hrl").

run() ->
    io:format("~n========================================~n"),
    io:format("     AIUTP Performance Benchmark~n"),
    io:format("========================================~n~n"),

    buffer_bench(),
    queue_bench(),
    delay_bench(),

    io:format("========================================~n"),
    io:format("     Benchmark Complete~n"),
    io:format("========================================~n").

buffer_bench() ->
    io:format("[Buffer Performance]~n"),

    %% Append benchmark
    B0 = aiutp_buffer:new(1024),
    {AppendTime, B1} = timer:tc(fun() ->
        do_append(B0, 1000)
    end),
    io:format("  - append 1000 ops: ~.2f ms (~.2f us/op)~n",
              [AppendTime/1000, AppendTime/1000]),

    %% Pop benchmark
    {PopTime, _} = timer:tc(fun() ->
        do_pop(B1, 1000)
    end),
    io:format("  - pop 1000 ops: ~.2f ms (~.2f us/op)~n~n",
              [PopTime/1000, PopTime/1000]).

do_append(B, 0) -> B;
do_append(B, N) ->
    case aiutp_buffer:append(N, B) of
        {error, _} -> B;
        NewB -> do_append(NewB, N - 1)
    end.

do_pop(B, 0) -> B;
do_pop(B, N) ->
    case aiutp_buffer:head(B) of
        -1 -> B;
        _ ->
            case aiutp_buffer:pop(B) of
                {ok, NewB} -> do_pop(NewB, N - 1);
                _ -> B
            end
    end.

queue_bench() ->
    io:format("[Queue Performance]~n"),

    %% Push benchmark
    Q0 = aiutp_queue:new(),
    {PushTime, Q1} = timer:tc(fun() ->
        do_push(Q0, 10000)
    end),
    io:format("  - push_back 10000 ops: ~.2f ms (~.2f us/op)~n",
              [PushTime/1000, PushTime/10000]),

    %% Pop benchmark
    {PopTime, _} = timer:tc(fun() ->
        do_queue_pop(Q1, 10000)
    end),
    io:format("  - pop_front 10000 ops: ~.2f ms (~.2f us/op)~n~n",
              [PopTime/1000, PopTime/10000]).

do_push(Q, 0) -> Q;
do_push(Q, N) ->
    do_push(aiutp_queue:push_back(N, Q), N - 1).

do_queue_pop(Q, 0) -> Q;
do_queue_pop(Q, N) ->
    case aiutp_queue:empty(Q) of
        true -> Q;
        false -> do_queue_pop(aiutp_queue:pop_front(Q), N - 1)
    end.

delay_bench() ->
    io:format("[Delay Estimation Performance]~n"),

    Now = erlang:system_time(microsecond),
    D0 = aiutp_delay:new(Now),

    %% Add sample benchmark
    {AddTime, D1} = timer:tc(fun() ->
        do_add_sample(D0, Now, 10000)
    end),
    io:format("  - add_sample 10000 ops: ~.2f ms (~.2f us/op)~n",
              [AddTime/1000, AddTime/10000]),

    %% Value lookup benchmark
    {ValueTime, _} = timer:tc(fun() ->
        do_value(D1, 100000)
    end),
    io:format("  - value lookup 100000 ops: ~.2f ms (~.2f us/op)~n~n",
              [ValueTime/1000, ValueTime/100000]).

do_add_sample(D, _, 0) -> D;
do_add_sample(D, Now, N) ->
    NewD = aiutp_delay:add_sample(N * 100, Now + N * 1000, D),
    do_add_sample(NewD, Now, N - 1).

do_value(_, 0) -> ok;
do_value(D, N) ->
    _ = aiutp_delay:value(D),
    do_value(D, N - 1).
