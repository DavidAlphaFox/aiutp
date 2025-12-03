%%------------------------------------------------------------------------------
%% @doc aiutp_mtu_tests - MTU 发现模块单元测试
%%
%% 测试 aiutp_mtu 模块的所有功能:
%% - reset/1: 重置 MTU 发现状态
%% - search_update/1: 二分查找更新
%% - should_probe/2: 探测条件判断
%% - on_probe_acked/2: ACK 处理
%% - on_probe_timeout/1: 超时处理
%% - on_probe_lost/1: 丢包处理
%% - packet_size/1: 获取当前包大小
%% - is_probing/1: 检查是否在探测中
%% - maybe_restart_discovery/1: 周期性重新探测
%% @end
%%------------------------------------------------------------------------------

-module(aiutp_mtu_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% 测试夹具
%%==============================================================================

%% 创建用于测试的 PCB 记录
make_pcb() ->
    #aiutp_pcb{
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_last = ?PACKET_SIZE,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_discover_time = 0,
        mtu_probe_failures = 0
    }.

%%==============================================================================
%% reset/1 测试
%%==============================================================================

reset_test_() ->
    {"reset/1 测试",
     [{"重置状态到默认值",
       fun() ->
           PCB = make_pcb(),
           PCB1 = aiutp_mtu:reset(PCB),
           ?assertEqual(?MTU_FLOOR_DEFAULT, PCB1#aiutp_pcb.mtu_floor),
           ?assertEqual(?MTU_CEILING_DEFAULT, PCB1#aiutp_pcb.mtu_ceiling),
           ?assertEqual(?PACKET_SIZE, PCB1#aiutp_pcb.mtu_last),
           ?assertEqual(0, PCB1#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(0, PCB1#aiutp_pcb.mtu_probe_size),
           ?assertEqual(0, PCB1#aiutp_pcb.mtu_probe_failures),
           ?assert(PCB1#aiutp_pcb.mtu_discover_time > 0)
       end},
      {"重置后设置下次探测时间",
       fun() ->
           PCB = make_pcb(),
           Now = aiutp_util:microsecond(),
           PCB1 = aiutp_mtu:reset(PCB),
           %% discover_time 应该在 Now + MTU_PROBE_INTERVAL 附近
           ?assert(PCB1#aiutp_pcb.mtu_discover_time > Now),
           ?assert(PCB1#aiutp_pcb.mtu_discover_time =< Now + ?MTU_PROBE_INTERVAL + 1000000)
       end}
     ]}.

%%==============================================================================
%% search_update/1 测试
%%==============================================================================

search_update_test_() ->
    {"search_update/1 测试",
     [{"继续二分查找",
       fun() ->
           PCB = make_pcb(),
           PCB1 = aiutp_mtu:search_update(PCB),
           %% 新的 mtu_last 应该是 (floor + ceiling) / 2
           Expected = (?MTU_FLOOR_DEFAULT + ?MTU_CEILING_DEFAULT) div 2,
           ?assertEqual(Expected, PCB1#aiutp_pcb.mtu_last)
       end},
      {"搜索完成时使用 floor",
       fun() ->
           PCB = make_pcb(),
           %% 设置 ceiling - floor <= MTU_SEARCH_THRESHOLD
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 1000,
               mtu_ceiling = 1010  %% 差值 10 <= 16
           },
           PCB2 = aiutp_mtu:search_update(PCB1),
           ?assertEqual(1000, PCB2#aiutp_pcb.mtu_last),
           ?assertEqual(1000, PCB2#aiutp_pcb.mtu_ceiling),
           ?assert(PCB2#aiutp_pcb.mtu_discover_time > 0)
       end},
      {"边界情况: 刚好等于阈值",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 1000,
               mtu_ceiling = 1016  %% 差值 16 = MTU_SEARCH_THRESHOLD
           },
           PCB2 = aiutp_mtu:search_update(PCB1),
           %% 等于阈值时仍然完成搜索
           ?assertEqual(1000, PCB2#aiutp_pcb.mtu_last)
       end}
     ]}.

%%==============================================================================
%% should_probe/2 测试
%%==============================================================================

should_probe_test_() ->
    {"should_probe/2 测试",
     [{"满足所有条件时返回 true",
       fun() ->
           PCB = make_pcb(),
           %% floor=528, ceiling=1452, probe_seq=0
           %% 包大小在 (floor, ceiling] 范围内
           ?assert(aiutp_mtu:should_probe(1000, PCB))
       end},
      {"包大小等于 ceiling 时返回 true",
       fun() ->
           PCB = make_pcb(),
           ?assert(aiutp_mtu:should_probe(?MTU_CEILING_DEFAULT, PCB))
       end},
      {"包大小等于 floor 时返回 false",
       fun() ->
           PCB = make_pcb(),
           ?assertNot(aiutp_mtu:should_probe(?MTU_FLOOR_DEFAULT, PCB))
       end},
      {"包大小小于 floor 时返回 false",
       fun() ->
           PCB = make_pcb(),
           ?assertNot(aiutp_mtu:should_probe(500, PCB))
       end},
      {"包大小大于 ceiling 时返回 false",
       fun() ->
           PCB = make_pcb(),
           ?assertNot(aiutp_mtu:should_probe(1500, PCB))
       end},
      {"已有在途探测时返回 false",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{mtu_probe_seq = 12345},
           ?assertNot(aiutp_mtu:should_probe(1000, PCB1))
       end},
      {"floor >= ceiling 时返回 false",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{mtu_floor = 1000, mtu_ceiling = 1000},
           ?assertNot(aiutp_mtu:should_probe(1000, PCB1))
       end}
     ]}.

%%==============================================================================
%% on_probe_acked/2 测试
%%==============================================================================

on_probe_acked_test_() ->
    {"on_probe_acked/2 测试",
     [{"探测成功提高 floor",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000
           },
           PCB2 = aiutp_mtu:on_probe_acked(100, PCB1),
           ?assertEqual(1000, PCB2#aiutp_pcb.mtu_floor),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_size),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_failures)
       end},
      {"ACK 序列号不匹配时不处理",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000
           },
           PCB2 = aiutp_mtu:on_probe_acked(99, PCB1),
           %% PCB 应该不变
           ?assertEqual(100, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(1000, PCB2#aiutp_pcb.mtu_probe_size)
       end},
      {"无探测包时不处理",
       fun() ->
           PCB = make_pcb(),
           PCB1 = aiutp_mtu:on_probe_acked(100, PCB),
           ?assertEqual(PCB, PCB1)
       end},
      {"成功后继续二分查找",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 528,
               mtu_ceiling = 1452,
               mtu_probe_seq = 100,
               mtu_probe_size = 990  %% (528 + 1452) / 2 = 990
           },
           PCB2 = aiutp_mtu:on_probe_acked(100, PCB1),
           %% floor 提高到 990
           ?assertEqual(990, PCB2#aiutp_pcb.mtu_floor),
           %% mtu_last 继续二分: (990 + 1452) / 2 = 1221
           ?assertEqual(1221, PCB2#aiutp_pcb.mtu_last)
       end}
     ]}.

%%==============================================================================
%% on_probe_timeout/1 测试
%%==============================================================================

on_probe_timeout_test_() ->
    {"on_probe_timeout/1 测试",
     [{"超时降低 ceiling",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000
           },
           PCB2 = aiutp_mtu:on_probe_timeout(PCB1),
           ?assertEqual(999, PCB2#aiutp_pcb.mtu_ceiling),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_size),
           ?assertEqual(1, PCB2#aiutp_pcb.mtu_probe_failures)
       end},
      {"无探测包时不处理",
       fun() ->
           PCB = make_pcb(),
           PCB1 = aiutp_mtu:on_probe_timeout(PCB),
           ?assertEqual(PCB, PCB1)
       end},
      {"连续失败达到阈值时回退",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 600,
               mtu_ceiling = 1000,
               mtu_probe_seq = 100,
               mtu_probe_size = 800,
               mtu_probe_failures = ?MTU_PROBE_FAILURE_THRESHOLD - 1
           },
           PCB2 = aiutp_mtu:on_probe_timeout(PCB1),
           %% 达到阈值，回退到 floor
           ?assertEqual(600, PCB2#aiutp_pcb.mtu_last),
           ?assertEqual(600, PCB2#aiutp_pcb.mtu_ceiling),
           ?assert(PCB2#aiutp_pcb.mtu_discover_time > 0)
       end},
      {"超时后继续二分查找",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 528,
               mtu_ceiling = 1452,
               mtu_probe_seq = 100,
               mtu_probe_size = 990
           },
           PCB2 = aiutp_mtu:on_probe_timeout(PCB1),
           %% ceiling 降低到 989
           ?assertEqual(989, PCB2#aiutp_pcb.mtu_ceiling),
           %% mtu_last 继续二分: (528 + 989) / 2 = 758
           ?assertEqual(758, PCB2#aiutp_pcb.mtu_last)
       end}
     ]}.

%%==============================================================================
%% on_probe_lost/1 测试
%%==============================================================================

on_probe_lost_test_() ->
    {"on_probe_lost/1 测试",
     [{"丢包与超时处理相同",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000
           },
           PCB_timeout = aiutp_mtu:on_probe_timeout(PCB1),

           PCB2 = PCB#aiutp_pcb{
               mtu_probe_seq = 100,
               mtu_probe_size = 1000
           },
           PCB_lost = aiutp_mtu:on_probe_lost(PCB2),

           ?assertEqual(PCB_timeout#aiutp_pcb.mtu_ceiling,
                        PCB_lost#aiutp_pcb.mtu_ceiling),
           ?assertEqual(PCB_timeout#aiutp_pcb.mtu_probe_failures,
                        PCB_lost#aiutp_pcb.mtu_probe_failures)
       end}
     ]}.

%%==============================================================================
%% packet_size/1 测试
%%==============================================================================

packet_size_test_() ->
    {"packet_size/1 测试",
     [{"返回 mtu_last 值",
       fun() ->
           PCB = make_pcb(),
           ?assertEqual(?PACKET_SIZE, aiutp_mtu:packet_size(PCB)),

           PCB1 = PCB#aiutp_pcb{mtu_last = 1000},
           ?assertEqual(1000, aiutp_mtu:packet_size(PCB1))
       end}
     ]}.

%%==============================================================================
%% is_probing/1 测试
%%==============================================================================

is_probing_test_() ->
    {"is_probing/1 测试",
     [{"无探测包时返回 false",
       fun() ->
           PCB = make_pcb(),
           ?assertNot(aiutp_mtu:is_probing(PCB))
       end},
      {"有探测包时返回 true",
       fun() ->
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{mtu_probe_seq = 100},
           ?assert(aiutp_mtu:is_probing(PCB1))
       end}
     ]}.

%%==============================================================================
%% maybe_restart_discovery/1 测试
%%==============================================================================

maybe_restart_discovery_test_() ->
    {"maybe_restart_discovery/1 测试",
     [{"搜索完成且时间到时重新开始",
       fun() ->
           Now = aiutp_util:microsecond(),
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 1000,
               mtu_ceiling = 1000,  %% floor >= ceiling 表示搜索完成
               mtu_discover_time = Now - 1000  %% 过去的时间
           },
           PCB2 = aiutp_mtu:maybe_restart_discovery(PCB1),
           %% 应该重置状态
           ?assertEqual(?MTU_FLOOR_DEFAULT, PCB2#aiutp_pcb.mtu_floor),
           ?assertEqual(?MTU_CEILING_DEFAULT, PCB2#aiutp_pcb.mtu_ceiling)
       end},
      {"时间未到时不重新开始",
       fun() ->
           Now = aiutp_util:microsecond(),
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 1000,
               mtu_ceiling = 1000,
               mtu_discover_time = Now + ?MTU_PROBE_INTERVAL
           },
           PCB2 = aiutp_mtu:maybe_restart_discovery(PCB1),
           %% 不应该改变
           ?assertEqual(1000, PCB2#aiutp_pcb.mtu_floor)
       end},
      {"搜索未完成时不重新开始",
       fun() ->
           Now = aiutp_util:microsecond(),
           PCB = make_pcb(),
           PCB1 = PCB#aiutp_pcb{
               mtu_floor = 528,
               mtu_ceiling = 1452,  %% floor < ceiling
               mtu_discover_time = Now - 1000
           },
           PCB2 = aiutp_mtu:maybe_restart_discovery(PCB1),
           %% 不应该改变
           ?assertEqual(528, PCB2#aiutp_pcb.mtu_floor),
           ?assertEqual(1452, PCB2#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%%==============================================================================
%% 收敛性测试
%%==============================================================================

convergence_test_() ->
    {"收敛性测试",
     [{"二分查找收敛到目标 MTU",
       fun() ->
           %% 模拟目标 MTU 为 1200
           TargetMTU = 1200,
           PCB = make_pcb(),
           PCB1 = aiutp_mtu:reset(PCB),

           %% 模拟探测过程
           FinalPCB = simulate_probing(PCB1, TargetMTU, 0),

           %% 最终 floor 应该在 (TargetMTU - MTU_SEARCH_THRESHOLD, TargetMTU] 范围内
           ?assert(FinalPCB#aiutp_pcb.mtu_floor =< TargetMTU),
           ?assert(FinalPCB#aiutp_pcb.mtu_floor >= TargetMTU - ?MTU_SEARCH_THRESHOLD)
       end},
      {"收敛在有限步数内完成",
       fun() ->
           TargetMTU = 1000,
           PCB = make_pcb(),
           PCB1 = aiutp_mtu:reset(PCB),

           {FinalPCB, Steps} = simulate_probing_count(PCB1, TargetMTU, 0),

           %% 二分查找应该在 log2(1452 - 528) ≈ 10 步内收敛
           ?assert(Steps =< 15),
           ?assertEqual(FinalPCB#aiutp_pcb.mtu_floor,
                        FinalPCB#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%% 模拟探测过程
simulate_probing(PCB, TargetMTU, Depth) when Depth > 20 ->
    %% 防止无限循环
    PCB;
simulate_probing(#aiutp_pcb{mtu_floor = Floor, mtu_ceiling = Ceiling} = PCB,
                 TargetMTU, Depth) when Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD ->
    %% 搜索完成
    PCB;
simulate_probing(PCB, TargetMTU, Depth) ->
    %% 计算探测大小
    ProbeSize = (PCB#aiutp_pcb.mtu_floor + PCB#aiutp_pcb.mtu_ceiling) div 2,

    %% 设置探测状态
    PCB1 = PCB#aiutp_pcb{
        mtu_probe_seq = 100 + Depth,
        mtu_probe_size = ProbeSize
    },

    %% 模拟探测结果
    PCB2 = if
        ProbeSize =< TargetMTU ->
            %% 探测成功
            aiutp_mtu:on_probe_acked(100 + Depth, PCB1);
        true ->
            %% 探测失败
            aiutp_mtu:on_probe_timeout(PCB1)
    end,

    simulate_probing(PCB2, TargetMTU, Depth + 1).

%% 模拟探测过程并计数
simulate_probing_count(PCB, TargetMTU, Steps) ->
    simulate_probing_count_loop(PCB, TargetMTU, Steps).

simulate_probing_count_loop(#aiutp_pcb{mtu_floor = Floor, mtu_ceiling = Ceiling} = PCB,
                            _TargetMTU, Steps) when Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD ->
    {aiutp_mtu:search_update(PCB), Steps};
simulate_probing_count_loop(PCB, _TargetMTU, Steps) when Steps > 20 ->
    {PCB, Steps};
simulate_probing_count_loop(PCB, TargetMTU, Steps) ->
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

    simulate_probing_count_loop(PCB2, TargetMTU, Steps + 1).
