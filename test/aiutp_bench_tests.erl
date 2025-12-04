%%------------------------------------------------------------------------------
%% @doc aiutp 性能基准测试
%%
%% 测试核心数据结构和算法的性能特性。
%%------------------------------------------------------------------------------
-module(aiutp_bench_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/aiutp.hrl").

%%==============================================================================
%% 测试配置
%%==============================================================================

-define(ITERATIONS, 10000).
-define(WARMUP_ITERATIONS, 1000).

%%==============================================================================
%% EUnit 集成
%%==============================================================================

benchmark_test_() ->
    {timeout, 120, [
        {"aiutp_buffer 性能测试", fun buffer_benchmark/0},
        {"aiutp_queue 性能测试", fun queue_benchmark/0},
        {"aiutp_packet 编解码性能", fun packet_benchmark/0},
        {"aiutp_delay 性能测试", fun delay_benchmark/0},
        {"aiutp_mtu 二分搜索性能", fun mtu_benchmark/0}
    ]}.

%%==============================================================================
%% 基准测试实现
%%==============================================================================

%% @doc 缓冲区操作性能测试
buffer_benchmark() ->
    %% 预热
    warmup_buffer(?WARMUP_ITERATIONS),

    %% 测试 append 操作
    AppendTime = measure(fun() ->
        B = aiutp_buffer:new(1024),
        buffer_append_n(B, 1000)
    end, ?ITERATIONS div 10),

    %% 测试 pop 操作
    B1 = buffer_append_n(aiutp_buffer:new(1024), 1000),
    PopTime = measure(fun() ->
        buffer_pop_n(B1, 1000)
    end, ?ITERATIONS div 10),

    io:format("~n  Buffer append (1000 ops): ~.2f us~n", [AppendTime]),
    io:format("  Buffer pop (1000 ops): ~.2f us~n", [PopTime]),

    %% 验证性能在合理范围内
    ?assert(AppendTime < 10000),  % < 10ms for 1000 appends
    ?assert(PopTime < 10000).

warmup_buffer(0) -> ok;
warmup_buffer(N) ->
    B = aiutp_buffer:new(64),
    B1 = buffer_append_n(B, 50),
    _ = buffer_pop_n(B1, 50),
    warmup_buffer(N - 1).

buffer_append_n(B, 0) -> B;
buffer_append_n(B, N) ->
    case aiutp_buffer:append(N, B) of
        {error, buffer_overflow} -> B;
        B1 -> buffer_append_n(B1, N - 1)
    end.

buffer_pop_n(B, 0) -> B;
buffer_pop_n(B, N) ->
    case aiutp_buffer:head(B) of
        -1 -> B;
        _ ->
            case aiutp_buffer:pop(B) of
                {ok, B1} -> buffer_pop_n(B1, N - 1);
                _ -> B
            end
    end.

%% @doc 队列操作性能测试
queue_benchmark() ->
    %% 预热
    warmup_queue(?WARMUP_ITERATIONS),

    %% 测试 push_back 操作
    PushTime = measure(fun() ->
        Q = aiutp_queue:new(),
        queue_push_n(Q, 1000)
    end, ?ITERATIONS div 10),

    %% 测试 pop_front 操作
    Q1 = queue_push_n(aiutp_queue:new(), 1000),
    PopTime = measure(fun() ->
        queue_pop_n(Q1, 1000)
    end, ?ITERATIONS div 10),

    io:format("~n  Queue push_back (1000 ops): ~.2f us~n", [PushTime]),
    io:format("  Queue pop_front (1000 ops): ~.2f us~n", [PopTime]),

    ?assert(PushTime < 5000),  % < 5ms for 1000 pushes
    ?assert(PopTime < 5000).

warmup_queue(0) -> ok;
warmup_queue(N) ->
    Q = aiutp_queue:new(),
    Q1 = queue_push_n(Q, 50),
    _ = queue_pop_n(Q1, 50),
    warmup_queue(N - 1).

queue_push_n(Q, 0) -> Q;
queue_push_n(Q, N) ->
    queue_push_n(aiutp_queue:push_back(N, Q), N - 1).

queue_pop_n(Q, 0) -> Q;
queue_pop_n(Q, N) ->
    case aiutp_queue:empty(Q) of
        true -> Q;
        false -> queue_pop_n(aiutp_queue:pop_front(Q), N - 1)
    end.

%% @doc 数据包编解码性能测试
packet_benchmark() ->
    %% 创建测试包
    Packet = #aiutp_packet{
        type = ?ST_DATA,
        seq_nr = 1000,
        ack_nr = 999,
        conn_id = 12345,
        wnd = 65535,
        extension = [{sack, <<255, 255, 255, 255>>}],
        payload = crypto:strong_rand_bytes(1000)
    },

    %% 预热
    warmup_packet(Packet, ?WARMUP_ITERATIONS),

    %% 测试编码性能
    EncodeTime = measure(fun() ->
        aiutp_packet:encode(Packet)
    end, ?ITERATIONS),

    %% 测试解码性能
    Encoded = aiutp_packet:encode(Packet),
    DecodeTime = measure(fun() ->
        aiutp_packet:decode(Encoded)
    end, ?ITERATIONS),

    io:format("~n  Packet encode (1KB payload): ~.2f us~n", [EncodeTime]),
    io:format("  Packet decode (1KB payload): ~.2f us~n", [DecodeTime]),

    ?assert(EncodeTime < 100),  % < 100us per encode
    ?assert(DecodeTime < 100).

warmup_packet(_, 0) -> ok;
warmup_packet(Packet, N) ->
    Encoded = aiutp_packet:encode(Packet),
    _ = aiutp_packet:decode(Encoded),
    warmup_packet(Packet, N - 1).

%% @doc 延迟估计性能测试
delay_benchmark() ->
    Now = aiutp_util:microsecond(),
    Delay = aiutp_delay:new(Now),

    %% 预热
    warmup_delay(Delay, Now, ?WARMUP_ITERATIONS),

    %% 测试 add_sample 性能
    AddSampleTime = measure(fun() ->
        Sample = rand:uniform(100000),
        Ts = Now + rand:uniform(1000000),
        aiutp_delay:add_sample(Sample, Ts, Delay)
    end, ?ITERATIONS),

    %% 测试 value 查询性能
    Delay1 = lists:foldl(fun(I, D) ->
        aiutp_delay:add_sample(I * 1000, Now + I * 1000000, D)
    end, Delay, lists:seq(1, 100)),

    ValueTime = measure(fun() ->
        aiutp_delay:value(Delay1)
    end, ?ITERATIONS),

    io:format("~n  Delay add_sample: ~.2f us~n", [AddSampleTime]),
    io:format("  Delay value lookup: ~.2f us~n", [ValueTime]),

    ?assert(AddSampleTime < 50),  % < 50us per add
    ?assert(ValueTime < 10).      % < 10us per lookup

warmup_delay(_, _, 0) -> ok;
warmup_delay(Delay, Now, N) ->
    _ = aiutp_delay:add_sample(N * 100, Now + N * 1000, Delay),
    _ = aiutp_delay:value(Delay),
    warmup_delay(Delay, Now, N - 1).

%% @doc MTU 二分搜索性能测试
mtu_benchmark() ->
    Now = aiutp_util:microsecond(),
    PCB = create_test_pcb(Now),

    %% 预热
    warmup_mtu(PCB, ?WARMUP_ITERATIONS),

    %% 测试 should_probe 性能
    ShouldProbeTime = measure(fun() ->
        aiutp_mtu:should_probe(1000, PCB)
    end, ?ITERATIONS),

    %% 测试 on_probe_acked 性能
    OnAckedTime = measure(fun() ->
        aiutp_mtu:on_probe_acked(100, PCB)
    end, ?ITERATIONS),

    %% 测试 on_probe_timeout 性能
    OnTimeoutTime = measure(fun() ->
        aiutp_mtu:on_probe_timeout(PCB)
    end, ?ITERATIONS),

    io:format("~n  MTU should_probe: ~.2f us~n", [ShouldProbeTime]),
    io:format("  MTU on_probe_acked: ~.2f us~n", [OnAckedTime]),
    io:format("  MTU on_probe_timeout: ~.2f us~n", [OnTimeoutTime]),

    ?assert(ShouldProbeTime < 10),  % < 10us per call
    ?assert(OnAckedTime < 20),
    ?assert(OnTimeoutTime < 20).

warmup_mtu(_, 0) -> ok;
warmup_mtu(PCB, N) ->
    _ = aiutp_mtu:should_probe(1000, PCB),
    _ = aiutp_mtu:on_probe_acked(100, PCB),
    _ = aiutp_mtu:on_probe_timeout(PCB),
    warmup_mtu(PCB, N - 1).

create_test_pcb(Now) ->
    #aiutp_pcb{
        time = Now,
        state = ?CS_CONNECTED,
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_probe_seq = 0,
        mtu_probe_failures = 0,
        mtu_discover_time = Now,
        cur_window_packets = 0,
        our_hist = aiutp_delay:new(Now),
        their_hist = aiutp_delay:new(Now),
        rtt_hist = aiutp_delay:new(Now),
        inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        outbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        inque = aiutp_queue:new(),
        outque = aiutp_queue:new()
    }.

%%==============================================================================
%% 辅助函数
%%==============================================================================

%% @doc 测量函数执行时间（返回平均微秒数）
measure(Fun, Iterations) ->
    {Time, _} = timer:tc(fun() ->
        measure_loop(Fun, Iterations)
    end),
    Time / Iterations.

measure_loop(_, 0) -> ok;
measure_loop(Fun, N) ->
    _ = Fun(),
    measure_loop(Fun, N - 1).
