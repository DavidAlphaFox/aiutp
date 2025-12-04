%%------------------------------------------------------------------------------
%% @doc aiutp_mtu_integration_tests - MTU 发现集成测试
%%
%% 测试 MTU 发现的完整流程，包括：
%% - 探测发送和 ACK 处理
%% - 超时处理
%% - 丢包检测（SACK 跳过）
%% - 周期性重新探测
%% - 多模块协作
%% @end
%%------------------------------------------------------------------------------

-module(aiutp_mtu_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% 测试夹具
%%==============================================================================

%% 创建完整的测试 PCB
create_connected_pcb() ->
    Now = aiutp_util:millisecond(),
    PCB = #aiutp_pcb{
        state = ?CS_CONNECTED,
        time = Now,
        conn_id_recv = 12345,
        conn_id_send = 12346,
        seq_nr = 100,
        ack_nr = 50,
        max_window = ?PACKET_SIZE * 10,
        max_window_user = ?PACKET_SIZE * 255,
        cur_window = 0,
        cur_window_packets = 0,
        retransmit_timeout = 1000,
        rto_timeout = 0,
        inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        outbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        inque = aiutp_queue:new(),
        outque = aiutp_queue:new(),
        our_hist = aiutp_delay:new(Now),
        their_hist = aiutp_delay:new(Now),
        rtt_hist = aiutp_delay:new(Now),
        last_got_packet = Now,
        fin_sent = false,
        last_sent_packet = Now
    },
    %% 初始化 MTU 发现状态
    aiutp_mtu:reset(PCB).

%%==============================================================================
%% 完整流程集成测试
%%==============================================================================

full_discovery_flow_test_() ->
    {"MTU 发现完整流程测试",
     [{"成功的二分查找收敛",
       fun() ->
           PCB = create_connected_pcb(),
           %% 模拟目标 MTU = 1200
           TargetMTU = 1200,

           %% 运行完整的探测流程
           FinalPCB = run_discovery_simulation(PCB, TargetMTU),

           %% 验证收敛
           ?assert(FinalPCB#aiutp_pcb.mtu_floor =< TargetMTU),
           ?assert(FinalPCB#aiutp_pcb.mtu_floor >= TargetMTU - ?MTU_SEARCH_THRESHOLD),
           %% 搜索应该完成
           ?assertEqual(FinalPCB#aiutp_pcb.mtu_floor,
                        FinalPCB#aiutp_pcb.mtu_ceiling)
       end},
      {"探测失败导致 ceiling 降低",
       fun() ->
           PCB = create_connected_pcb(),
           %% 模拟目标 MTU = 800（较小值）
           TargetMTU = 800,

           FinalPCB = run_discovery_simulation(PCB, TargetMTU),

           %% 验证 ceiling 降到了目标附近
           ?assert(FinalPCB#aiutp_pcb.mtu_ceiling =< TargetMTU),
           ?assert(FinalPCB#aiutp_pcb.mtu_floor =< TargetMTU)
       end}
     ]}.

%%==============================================================================
%% 模块协作测试
%%==============================================================================

module_integration_test_() ->
    {"模块协作测试",
     [{"aiutp_mtu 和 aiutp_pcb_timeout 协作",
       fun() ->
           PCB = create_connected_pcb(),
           %% 设置在途探测
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000,
               rto_timeout = aiutp_util:millisecond() - 100  %% 已超时
           },
           %% 通过 check_timeouts 触发探测超时
           PCB2 = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 验证 MTU 模块被正确调用
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(999, PCB2#aiutp_pcb.mtu_ceiling)
       end},
      {"aiutp_mtu 和 aiutp_tx 协作",
       fun() ->
           PCB = create_connected_pcb(),
           %% 添加探测包到 outbuf
           PCB1 = add_mtu_probe_packet(PCB, 100, 1000),
           %% 添加普通包
           PCB2 = add_normal_packet(PCB1, 101),

           %% 设置探测状态
           PCB3 = PCB2#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000
           },

           %% 设置跳过计数达到阈值
           PCB4 = set_packet_skip_count(PCB3, 100, ?DUPLICATE_ACKS_BEFORE_RESEND - 1),

           %% SACK 101，跳过 100
           {_SkippedCount, PCB5} = aiutp_tx:update_skip_counts([101], PCB4),

           %% 验证探测丢失被处理
           ?assertEqual(0, PCB5#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(999, PCB5#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%%==============================================================================
%% 边界条件测试
%%==============================================================================

edge_cases_test_() ->
    {"边界条件测试",
     [{"连续失败后回退",
       fun() ->
           PCB = create_connected_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 600,
               mtu_ceiling = 1000,
               mtu_probe_failures = ?MTU_PROBE_FAILURE_THRESHOLD - 1
           },

           %% 设置探测并触发超时
           PCB2 = PCB1#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 800,
               rto_timeout = aiutp_util:millisecond() - 100
           },

           PCB3 = aiutp_pcb_timeout:check_timeouts(PCB2),

           %% 应该回退到 floor
           ?assertEqual(600, PCB3#aiutp_pcb.mtu_last),
           ?assertEqual(600, PCB3#aiutp_pcb.mtu_ceiling)
       end},
      {"floor 等于 ceiling 时停止探测",
       fun() ->
           PCB = create_connected_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 1000,
               mtu_ceiling = 1000
           },

           %% should_probe 应该返回 false
           ?assertNot(aiutp_mtu:should_probe(1000, PCB1)),
           ?assertNot(aiutp_mtu:should_probe(1200, PCB1))
       end},
      {"周期性重新探测时机正确",
       fun() ->
           NowMicro = aiutp_util:microsecond(),
           PCB = create_connected_pcb(),

           %% 搜索完成，时间已到
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 1000,
               mtu_ceiling = 1000,
               mtu_discover_time = NowMicro - 1000
           },

           PCB2 = aiutp_mtu:maybe_restart_discovery(PCB1),

           %% 应该重置
           ?assertEqual(?MTU_FLOOR_DEFAULT, PCB2#aiutp_pcb.mtu_floor),
           ?assertEqual(?MTU_CEILING_DEFAULT, PCB2#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%%==============================================================================
%% 状态一致性测试
%%==============================================================================

state_consistency_test_() ->
    {"状态一致性测试",
     [{"探测成功后状态一致",
       fun() ->
           PCB = create_connected_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 990,
               mtu_probe_failures = 1  %% 之前有失败
           },

           %% 探测成功
           PCB2 = aiutp_mtu:on_probe_acked(100, PCB1),

           %% 验证状态
           ?assertEqual(990, PCB2#aiutp_pcb.mtu_floor),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_size),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_failures)  %% 重置失败计数
       end},
      {"探测失败后状态一致",
       fun() ->
           PCB = create_connected_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000,
               mtu_ceiling = 1200
           },

           %% 探测失败
           PCB2 = aiutp_mtu:on_probe_timeout(PCB1),

           %% 验证状态
           ?assertEqual(999, PCB2#aiutp_pcb.mtu_ceiling),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_size),
           ?assertEqual(1, PCB2#aiutp_pcb.mtu_probe_failures)
       end}
     ]}.

%%==============================================================================
%% 辅助函数
%%==============================================================================

%% 模拟完整的 MTU 发现过程
run_discovery_simulation(PCB, TargetMTU) ->
    run_discovery_simulation(PCB, TargetMTU, 0).

run_discovery_simulation(PCB, _TargetMTU, Depth) when Depth > 20 ->
    PCB;
run_discovery_simulation(#aiutp_pcb{mtu_floor = Floor, mtu_ceiling = Ceiling} = PCB,
                         _TargetMTU, _Depth) when Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD ->
    aiutp_mtu:search_update(PCB);
run_discovery_simulation(PCB, TargetMTU, Depth) ->
    ProbeSize = (PCB#aiutp_pcb.mtu_floor + PCB#aiutp_pcb.mtu_ceiling) div 2,
    PCB1 = PCB#aiutp_pcb{
        mtu_probe_seq = 100 + Depth,
        mtu_probe_size = ProbeSize
    },
    PCB2 = if
        ProbeSize =< TargetMTU ->
            aiutp_mtu:on_probe_acked(100 + Depth, PCB1);
        true ->
            aiutp_mtu:on_probe_timeout(PCB1)
    end,
    run_discovery_simulation(PCB2, TargetMTU, Depth + 1).

%% 添加 MTU 探测包到 outbuf
add_mtu_probe_packet(#aiutp_pcb{outbuf = OutBuf} = PCB, SeqNR, PayloadSize) ->
    Packet = #aiutp_packet{seq_nr = SeqNR, type = ?ST_DATA},
    WrapPacket = #aiutp_packet_wrap{
        packet = Packet,
        transmissions = 1,
        payload = PayloadSize,
        skip_count = 0,
        is_mtu_probe = true
    },
    OutBuf1 = aiutp_buffer:append(WrapPacket, OutBuf),
    PCB#aiutp_pcb{outbuf = OutBuf1, cur_window_packets = PCB#aiutp_pcb.cur_window_packets + 1}.

%% 添加普通包到 outbuf
add_normal_packet(#aiutp_pcb{outbuf = OutBuf} = PCB, SeqNR) ->
    Packet = #aiutp_packet{seq_nr = SeqNR, type = ?ST_DATA},
    WrapPacket = #aiutp_packet_wrap{
        packet = Packet,
        transmissions = 1,
        payload = 100,
        skip_count = 0,
        is_mtu_probe = false
    },
    OutBuf1 = aiutp_buffer:append(WrapPacket, OutBuf),
    PCB#aiutp_pcb{outbuf = OutBuf1, cur_window_packets = PCB#aiutp_pcb.cur_window_packets + 1}.

%% 设置指定包的 skip_count
set_packet_skip_count(#aiutp_pcb{outbuf = OutBuf} = PCB, SeqNR, SkipCount) ->
    OutBuf1 = set_skip_count_loop(SeqNR, SkipCount, aiutp_buffer:head(OutBuf), OutBuf),
    PCB#aiutp_pcb{outbuf = OutBuf1}.

set_skip_count_loop(_SeqNR, _SkipCount, -1, OutBuf) ->
    OutBuf;
set_skip_count_loop(SeqNR, SkipCount, Iter, OutBuf) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Packet = WrapPacket#aiutp_packet_wrap.packet,
    case Packet#aiutp_packet.seq_nr of
        SeqNR ->
            WrapPacket1 = WrapPacket#aiutp_packet_wrap{skip_count = SkipCount},
            aiutp_buffer:replace(Iter, WrapPacket1, OutBuf);
        _ ->
            Next = aiutp_buffer:next(Iter, OutBuf),
            set_skip_count_loop(SeqNR, SkipCount, Next, OutBuf)
    end.

%%==============================================================================
%% 性能验证测试
%%==============================================================================

performance_test_() ->
    {"性能验证测试",
     [{"收敛步数验证 - 目标 MTU 1200",
       fun() ->
           PCB = create_connected_pcb(),
           {_FinalPCB, Steps} = run_discovery_with_count(PCB, 1200),
           %% 二分查找: log2(1452 - 528) ≈ log2(924) ≈ 10 步
           ?assert(Steps =< 15),
           io:format("收敛步数 (MTU=1200): ~p~n", [Steps])
       end},
      {"收敛步数验证 - 目标 MTU 600",
       fun() ->
           PCB = create_connected_pcb(),
           {_FinalPCB, Steps} = run_discovery_with_count(PCB, 600),
           ?assert(Steps =< 15),
           io:format("收敛步数 (MTU=600): ~p~n", [Steps])
       end},
      {"收敛步数验证 - 目标 MTU 1400",
       fun() ->
           PCB = create_connected_pcb(),
           {_FinalPCB, Steps} = run_discovery_with_count(PCB, 1400),
           ?assert(Steps =< 15),
           io:format("收敛步数 (MTU=1400): ~p~n", [Steps])
       end},
      {"收敛精度验证",
       fun() ->
           %% 测试目标 MTU 在 floor 范围内的情况
           %% 当 Target < MTU_FLOOR_DEFAULT 时，二分查找无法收敛到 Target
           %% 所以只测试 Target >= MTU_FLOOR_DEFAULT 的情况
           TargetMTUs = [800, 1000, 1200, 1400],
           lists:foreach(fun(Target) ->
               PCB = create_connected_pcb(),
               {FinalPCB, Steps} = run_discovery_with_count(PCB, Target),
               Floor = FinalPCB#aiutp_pcb.mtu_floor,
               io:format("Target: ~p, Floor: ~p, Ceiling: ~p, Steps: ~p~n",
                         [Target, Floor, FinalPCB#aiutp_pcb.mtu_ceiling, Steps]),
               %% 验证 floor <= target（因为探测失败会降低 ceiling）
               ?assert(Floor =< Target),
               %% 验证收敛完成
               ?assertEqual(Floor, FinalPCB#aiutp_pcb.mtu_ceiling)
           end, TargetMTUs)
       end},
      {"探测开销验证 - 操作复杂度",
       fun() ->
           %% 验证各操作都是 O(1) 复杂度
           PCB = create_connected_pcb(),

           %% on_probe_acked 应该是 O(1)
           PCB1 = PCB#aiutp_pcb{mtu_probe_seq = 100, mtu_probe_size = 1000},
           {Time1, _} = timer:tc(fun() ->
               lists:foreach(fun(_) ->
                   aiutp_mtu:on_probe_acked(100, PCB1)
               end, lists:seq(1, 1000))
           end),

           %% on_probe_timeout 应该是 O(1)
           {Time2, _} = timer:tc(fun() ->
               lists:foreach(fun(_) ->
                   aiutp_mtu:on_probe_timeout(PCB1)
               end, lists:seq(1, 1000))
           end),

           %% should_probe 应该是 O(1)
           {Time3, _} = timer:tc(fun() ->
               lists:foreach(fun(_) ->
                   aiutp_mtu:should_probe(1000, PCB)
               end, lists:seq(1, 1000))
           end),

           %% 每次操作应该 < 1ms
           ?assert(Time1 div 1000 < 1000),  %% 1000 次 < 1000ms
           ?assert(Time2 div 1000 < 1000),
           ?assert(Time3 div 1000 < 1000),

           io:format("1000次 on_probe_acked: ~p us~n", [Time1]),
           io:format("1000次 on_probe_timeout: ~p us~n", [Time2]),
           io:format("1000次 should_probe: ~p us~n", [Time3])
       end}
     ]}.

%% 带计数的发现模拟
run_discovery_with_count(PCB, TargetMTU) ->
    run_discovery_with_count(PCB, TargetMTU, 0).

run_discovery_with_count(#aiutp_pcb{mtu_floor = Floor, mtu_ceiling = Ceiling} = PCB,
                         _TargetMTU, Steps) when Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD ->
    {aiutp_mtu:search_update(PCB), Steps};
run_discovery_with_count(PCB, _TargetMTU, Steps) when Steps > 20 ->
    {PCB, Steps};
run_discovery_with_count(PCB, TargetMTU, Steps) ->
    ProbeSize = (PCB#aiutp_pcb.mtu_floor + PCB#aiutp_pcb.mtu_ceiling) div 2,
    PCB1 = PCB#aiutp_pcb{
        mtu_probe_seq = 100 + Steps,
        mtu_probe_size = ProbeSize
    },
    PCB2 = if
        ProbeSize =< TargetMTU ->
            aiutp_mtu:on_probe_acked(100 + Steps, PCB1);
        true ->
            aiutp_mtu:on_probe_timeout(PCB1)
    end,
    run_discovery_with_count(PCB2, TargetMTU, Steps + 1).
