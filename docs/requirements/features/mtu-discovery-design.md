# MTU 发现详细设计文档

> 版本: 1.0
> 日期: 2025-12-03
> 状态: 草案

## 1. 架构概述

### 1.1 模块结构

```
┌─────────────────────────────────────────────────────────────┐
│                      aiutp_pcb.erl                          │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                MTU Discovery State                   │   │
│  │  - mtu_floor, mtu_ceiling, mtu_last                 │   │
│  │  - mtu_probe_seq, mtu_probe_size                    │   │
│  │  - mtu_discover_time                                │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│  aiutp_mtu.erl  │  │  aiutp_net.erl  │  │ aiutp_pcb_cc.erl│
│  (新模块)        │  │  (修改)          │  │  (修改)         │
│                 │  │                 │  │                 │
│ - mtu_reset/1   │  │ - 探测包发送     │  │ - ACK 处理      │
│ - mtu_search/1  │  │ - packet_size/1 │  │ - 超时处理      │
│ - mtu_probe/2   │  │                 │  │                 │
└─────────────────┘  └─────────────────┘  └─────────────────┘
```

### 1.2 数据流

```
连接建立
    │
    ▼
mtu_reset() ─────────────────┐
    │                        │
    ▼                        │
┌──────────────────────┐     │
│ 探测循环开始          │     │
│ mtu_last = (f+c)/2   │     │
└──────────────────────┘     │
    │                        │
    ▼                        │
选择探测包 ◄─────────────────┘
    │
    ├─── 有合适大小的数据包？
    │         │
    │    是   │   否
    │    │    │    │
    │    ▼    │    ▼
    │  标记为  │  等待
    │  探测包  │
    │    │    │
    │    ▼    │
    │  发送   │
    │    │    │
    └────┼────┘
         │
         ▼
    等待结果
         │
    ┌────┴────┐
    │         │
    ▼         ▼
  超时      ACK 成功
    │         │
    ▼         ▼
ceiling =  floor =
probe-1   probe_size
    │         │
    └────┬────┘
         │
         ▼
    c - f <= 16?
         │
    ┌────┴────┐
    │ 是      │ 否
    ▼         ▼
  完成     继续探测
    │
    ▼
设置定时器
(30分钟后重新探测)
```

## 2. 数据结构设计

### 2.1 PCB 记录扩展

```erlang
%% 在 aiutp.hrl 中添加 MTU 相关字段

-record(aiutp_pcb, {
    %% ... 现有字段 ...

    %% === MTU Discovery ===
    %% MTU 二分查找范围
    mtu_floor = ?MTU_FLOOR_DEFAULT :: non_neg_integer(),
    mtu_ceiling = ?MTU_CEILING_DEFAULT :: non_neg_integer(),

    %% 当前使用的 MTU (payload size)
    mtu_last = ?PACKET_SIZE :: non_neg_integer(),

    %% 探测包跟踪
    mtu_probe_seq = 0 :: non_neg_integer(),      %% 探测包的 seq_nr，0 表示无在途探测
    mtu_probe_size = 0 :: non_neg_integer(),     %% 探测包的大小

    %% 下次重新探测时间 (microseconds)
    mtu_discover_time = 0 :: non_neg_integer(),

    %% 探测失败计数
    mtu_probe_failures = 0 :: non_neg_integer()
}).
```

### 2.2 新增常量

```erlang
%% 在 aiutp.hrl 中添加

%% === MTU Discovery Constants ===

%% MTU 下限 (TCP 最小 MTU - IP/UDP/uTP 头部)
%% 576 - 20 (IP) - 8 (UDP) - 20 (uTP) = 528
-define(MTU_FLOOR_DEFAULT, 528).

%% MTU 上限 (以太网 MTU - 头部)
%% 1500 - 20 (IP) - 8 (UDP) - 20 (uTP) = 1452
-define(MTU_CEILING_DEFAULT, 1452).

%% 二分查找终止阈值
-define(MTU_SEARCH_THRESHOLD, 16).

%% 重新探测间隔 (30 分钟，微秒)
-define(MTU_PROBE_INTERVAL, 30 * 60 * 1000000).

%% 探测失败回退阈值
-define(MTU_PROBE_FAILURE_THRESHOLD, 3).

%% 探测失败后延长间隔倍数
-define(MTU_PROBE_BACKOFF_MULTIPLIER, 2).
```

### 2.3 包装记录扩展

```erlang
%% 在 aiutp_packet_wrap 中添加探测标记
-record(aiutp_packet_wrap, {
    packet :: #aiutp_packet{},
    payload = 0 :: integer(),
    transmissions = 0 :: integer(),
    need_resend = false :: boolean(),
    skip_count = 0 :: integer(),
    time_sent = 0 :: integer(),

    %% 新增: MTU 探测标记
    is_mtu_probe = false :: boolean()
}).
```

## 3. 模块设计

### 3.1 aiutp_mtu.erl (新模块)

```erlang
-module(aiutp_mtu).

-export([
    reset/1,              %% 重置 MTU 发现状态
    search_update/1,      %% 更新二分查找
    should_probe/2,       %% 判断是否应该使用此包作为探测包
    on_probe_acked/2,     %% 探测包被 ACK
    on_probe_timeout/1,   %% 探测包超时
    on_probe_lost/1,      %% 探测包丢失（重复 ACK）
    packet_size/1,        %% 获取当前有效 packet size
    is_probing/1          %% 是否有在途探测
]).

%% @doc 重置 MTU 发现状态
-spec reset(#aiutp_pcb{}) -> #aiutp_pcb{}.
reset(PCB) ->
    Now = aiutp_util:microseconds(),
    PCB#aiutp_pcb{
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_last = ?PACKET_SIZE,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_discover_time = Now + ?MTU_PROBE_INTERVAL,
        mtu_probe_failures = 0
    }.

%% @doc 更新二分查找，计算下一个探测大小
-spec search_update(#aiutp_pcb{}) -> #aiutp_pcb{}.
search_update(#aiutp_pcb{mtu_floor = Floor, mtu_ceiling = Ceiling} = PCB) ->
    case Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD of
        true ->
            %% 搜索完成，使用 floor 作为最终值
            Now = aiutp_util:microseconds(),
            PCB#aiutp_pcb{
                mtu_last = Floor,
                mtu_ceiling = Floor,
                mtu_discover_time = Now + ?MTU_PROBE_INTERVAL
            };
        false ->
            %% 继续二分查找
            NewLast = (Floor + Ceiling) div 2,
            PCB#aiutp_pcb{mtu_last = NewLast}
    end.

%% @doc 判断是否应该将此包作为 MTU 探测包
-spec should_probe(non_neg_integer(), #aiutp_pcb{}) -> boolean().
should_probe(PacketSize, #aiutp_pcb{
    mtu_floor = Floor,
    mtu_ceiling = Ceiling,
    mtu_probe_seq = ProbeSeq
} = _PCB) ->
    %% 条件:
    %% 1. 存在搜索空间 (floor < ceiling)
    %% 2. 包大小在范围内 (floor < size <= ceiling)
    %% 3. 无在途探测包 (probe_seq == 0)
    (Floor < Ceiling) andalso
    (PacketSize > Floor) andalso
    (PacketSize =< Ceiling) andalso
    (ProbeSeq == 0).

%% @doc 探测包被成功 ACK
-spec on_probe_acked(non_neg_integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
on_probe_acked(AckedSeq, #aiutp_pcb{
    mtu_probe_seq = ProbeSeq,
    mtu_probe_size = ProbeSize
} = PCB) when AckedSeq == ProbeSeq, ProbeSeq =/= 0 ->
    %% 探测成功，提高 floor
    PCB1 = PCB#aiutp_pcb{
        mtu_floor = ProbeSize,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_probe_failures = 0
    },
    search_update(PCB1);
on_probe_acked(_AckedSeq, PCB) ->
    PCB.

%% @doc 探测包超时
-spec on_probe_timeout(#aiutp_pcb{}) -> #aiutp_pcb{}.
on_probe_timeout(#aiutp_pcb{
    mtu_probe_seq = ProbeSeq,
    mtu_probe_size = ProbeSize,
    mtu_probe_failures = Failures
} = PCB) when ProbeSeq =/= 0 ->
    %% 探测失败，降低 ceiling
    NewFailures = Failures + 1,
    PCB1 = PCB#aiutp_pcb{
        mtu_ceiling = ProbeSize - 1,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_probe_failures = NewFailures
    },
    case NewFailures >= ?MTU_PROBE_FAILURE_THRESHOLD of
        true ->
            %% 连续失败过多，回退到 floor
            Now = aiutp_util:microseconds(),
            BackoffInterval = ?MTU_PROBE_INTERVAL * ?MTU_PROBE_BACKOFF_MULTIPLIER,
            PCB1#aiutp_pcb{
                mtu_last = PCB1#aiutp_pcb.mtu_floor,
                mtu_ceiling = PCB1#aiutp_pcb.mtu_floor,
                mtu_discover_time = Now + BackoffInterval
            };
        false ->
            search_update(PCB1)
    end;
on_probe_timeout(PCB) ->
    PCB.

%% @doc 探测包因重复 ACK 判定丢失
-spec on_probe_lost(#aiutp_pcb{}) -> #aiutp_pcb{}.
on_probe_lost(PCB) ->
    %% 与超时处理相同
    on_probe_timeout(PCB).

%% @doc 获取当前有效的 packet size
-spec packet_size(#aiutp_pcb{}) -> non_neg_integer().
packet_size(#aiutp_pcb{mtu_last = Last}) ->
    Last.

%% @doc 检查是否有在途探测包
-spec is_probing(#aiutp_pcb{}) -> boolean().
is_probing(#aiutp_pcb{mtu_probe_seq = Seq}) ->
    Seq =/= 0.
```

### 3.2 aiutp_net.erl 修改

```erlang
%% 修改 send_data_packet/3，支持 MTU 探测

send_data_packet(Iter, BytesToSend, PCB) ->
    %% ... 现有逻辑 ...

    %% 获取当前有效 packet size
    MaxPayload = aiutp_mtu:packet_size(PCB),
    ActualPayload = erlang:min(Payload, MaxPayload),

    %% 检查是否应该作为探测包
    IsMtuProbe = aiutp_mtu:should_probe(ActualPayload, PCB),

    %% 更新 wrap 记录
    WrapPacket1 = WrapPacket#aiutp_packet_wrap{
        is_mtu_probe = IsMtuProbe,
        %% ... 其他字段 ...
    },

    %% 如果是探测包，记录探测状态
    PCB1 = case IsMtuProbe of
        true ->
            SeqNR = (WrapPacket#aiutp_packet_wrap.packet)#aiutp_packet.seq_nr,
            PCB#aiutp_pcb{
                mtu_probe_seq = SeqNR,
                mtu_probe_size = ActualPayload
            };
        false ->
            PCB
    end,

    %% ... 继续发送 ...
```

### 3.3 aiutp_pcb_cc.erl 修改

```erlang
%% 修改 ack_packet/3，处理 MTU 探测 ACK

ack_packet(WrapPacket, AckNR, PCB) ->
    SeqNR = (WrapPacket#aiutp_packet_wrap.packet)#aiutp_packet.seq_nr,

    %% 检查是否是探测包 ACK
    PCB1 = case WrapPacket#aiutp_packet_wrap.is_mtu_probe of
        true ->
            aiutp_mtu:on_probe_acked(SeqNR, PCB);
        false ->
            PCB
    end,

    %% ... 继续现有 ACK 处理 ...
```

### 3.4 aiutp_pcb_timeout.erl 修改

```erlang
%% 修改超时处理，检测 MTU 探测超时

check_timeouts(PCB) ->
    %% ... 现有逻辑 ...

    %% 检查是否唯一在途包是探测包
    case is_only_probe_in_flight(PCB) of
        true ->
            %% 探测包超时，不触发拥塞控制
            PCB1 = aiutp_mtu:on_probe_timeout(PCB),
            %% 重发探测包但不重置窗口
            resend_probe_packet(PCB1);
        false ->
            %% 正常超时处理
            handle_normal_timeout(PCB)
    end.

%% 检查是否唯一在途包是探测包
is_only_probe_in_flight(#aiutp_pcb{
    mtu_probe_seq = ProbeSeq,
    cur_window_packets = 1
} = PCB) when ProbeSeq =/= 0 ->
    %% 只有一个在途包，且是探测包
    true;
is_only_probe_in_flight(_) ->
    false.
```

### 3.5 aiutp_tx.erl 修改

```erlang
%% 修改 SACK 处理，检测探测包丢失

update_skip_counts_loop(...) ->
    %% ... 现有逻辑 ...

    %% 检查是否探测包被判定丢失
    PCB1 = case WrapPacket#aiutp_packet_wrap.is_mtu_probe andalso
                NewSkipCount >= ?DUPLICATE_ACKS_BEFORE_RESEND of
        true ->
            aiutp_mtu:on_probe_lost(PCB);
        false ->
            PCB
    end,

    %% ... 继续处理 ...
```

## 4. 状态机

### 4.1 MTU 发现状态

```
                    ┌─────────────┐
                    │   IDLE      │
                    │ (无探测)    │
                    └──────┬──────┘
                           │
                    连接建立/重新探测时间到
                           │
                           ▼
                    ┌─────────────┐
                    │  SEARCHING  │◄────────────────┐
                    │ (二分查找)  │                  │
                    └──────┬──────┘                  │
                           │                         │
                    发送探测包                        │
                           │                         │
                           ▼                         │
                    ┌─────────────┐                  │
                    │  PROBING    │                  │
                    │ (等待ACK)   │                  │
                    └──────┬──────┘                  │
                           │                         │
              ┌────────────┼────────────┐            │
              │            │            │            │
           ACK成功      超时/丢失    c-f<=16        │
              │            │            │            │
              ▼            ▼            ▼            │
         floor=size   ceiling=        完成           │
              │        size-1          │            │
              │            │           │            │
              └────────────┴───────────┘            │
                           │                         │
                    c - f > 16?                      │
                           │                         │
                      ┌────┴────┐                    │
                      │ 是      │ 否                 │
                      └────┬────┘                    │
                           │    │                    │
                           │    ▼                    │
                           │  ┌─────────────┐       │
                           │  │  COMPLETE   │       │
                           │  │ (探测完成)  │       │
                           │  └──────┬──────┘       │
                           │         │              │
                           │    30分钟后             │
                           │         │              │
                           └─────────┴──────────────┘
```

### 4.2 状态转换表

| 当前状态 | 事件 | 动作 | 下一状态 |
|----------|------|------|----------|
| IDLE | 连接建立 | mtu_reset() | SEARCHING |
| IDLE | 重新探测时间到 | mtu_reset() | SEARCHING |
| SEARCHING | 有合适包 | 标记为探测包 | PROBING |
| PROBING | ACK 成功 | floor = probe_size | SEARCHING/COMPLETE |
| PROBING | 超时 | ceiling = probe_size - 1 | SEARCHING/COMPLETE |
| PROBING | 重复 ACK | ceiling = probe_size - 1 | SEARCHING/COMPLETE |
| COMPLETE | 30 分钟 | mtu_reset() | SEARCHING |

## 5. 算法详解

### 5.1 二分查找算法

```
初始: floor = 528, ceiling = 1452

第 1 次探测: size = (528 + 1452) / 2 = 990
  - ACK 成功 → floor = 990
  - 超时/丢失 → ceiling = 989

第 2 次探测: size = (990 + 1452) / 2 = 1221 (假设第1次成功)
  - ACK 成功 → floor = 1221
  - 超时/丢失 → ceiling = 1220

... 继续直到 ceiling - floor <= 16 ...

最终: 约 7-10 次探测后收敛
```

### 5.2 探测包选择策略

```erlang
%% 探测包选择条件
should_use_as_probe(Packet, PCB) ->
    PacketSize = byte_size(Packet#aiutp_packet.data),

    %% 1. 有搜索空间
    HasSearchSpace = PCB#aiutp_pcb.mtu_floor < PCB#aiutp_pcb.mtu_ceiling,

    %% 2. 包大小在搜索范围内
    InRange = (PacketSize > PCB#aiutp_pcb.mtu_floor) andalso
              (PacketSize =< PCB#aiutp_pcb.mtu_ceiling),

    %% 3. 无在途探测
    NoProbeInFlight = PCB#aiutp_pcb.mtu_probe_seq == 0,

    %% 4. 是首次传输（非重传）
    FirstTransmission = WrapPacket#aiutp_packet_wrap.transmissions == 0,

    HasSearchSpace andalso InRange andalso NoProbeInFlight andalso FirstTransmission.
```

### 5.3 超时特殊处理

```erlang
%% 探测包超时时的特殊处理
handle_probe_timeout(PCB) ->
    %% 1. 不触发拥塞控制（不减半窗口）
    %% 2. 降低 ceiling
    %% 3. 立即重发（用更小的 MTU）

    PCB1 = aiutp_mtu:on_probe_timeout(PCB),

    %% 重发时使用新的 mtu_last
    resend_with_new_mtu(PCB1).
```

## 6. 配置接口

### 6.1 应用配置

```erlang
%% sys.config 或 application:set_env/3

[
    {aiutp, [
        %% 是否启用 MTU 发现
        {mtu_discovery_enabled, true},

        %% MTU 下限 (字节)
        {mtu_floor, 528},

        %% MTU 上限 (字节)
        {mtu_ceiling, 1452},

        %% 重新探测间隔 (毫秒)
        {mtu_probe_interval, 1800000},  %% 30 分钟

        %% 探测失败回退阈值
        {mtu_probe_failure_threshold, 3}
    ]}
].
```

### 6.2 运行时 API

```erlang
%% 查询当前 MTU
aiutp:get_mtu(Connection) -> {ok, MTU} | {error, Reason}.

%% 手动触发 MTU 重新探测
aiutp:probe_mtu(Connection) -> ok | {error, Reason}.

%% 禁用/启用 MTU 发现
aiutp:set_mtu_discovery(Connection, boolean()) -> ok.
```

## 7. 测试策略

### 7.1 单元测试

```erlang
%% test/aiutp_mtu_tests.erl

-module(aiutp_mtu_tests).
-include_lib("eunit/include/eunit.hrl").

reset_test() ->
    PCB = aiutp_mtu:reset(#aiutp_pcb{}),
    ?assertEqual(?MTU_FLOOR_DEFAULT, PCB#aiutp_pcb.mtu_floor),
    ?assertEqual(?MTU_CEILING_DEFAULT, PCB#aiutp_pcb.mtu_ceiling).

search_update_converges_test() ->
    %% 测试二分查找收敛
    PCB0 = #aiutp_pcb{mtu_floor = 528, mtu_ceiling = 1452},
    PCB1 = simulate_search(PCB0, 10),
    Diff = PCB1#aiutp_pcb.mtu_ceiling - PCB1#aiutp_pcb.mtu_floor,
    ?assert(Diff =< ?MTU_SEARCH_THRESHOLD).

probe_ack_raises_floor_test() ->
    PCB0 = #aiutp_pcb{mtu_floor = 528, mtu_ceiling = 1452,
                     mtu_probe_seq = 100, mtu_probe_size = 990},
    PCB1 = aiutp_mtu:on_probe_acked(100, PCB0),
    ?assertEqual(990, PCB1#aiutp_pcb.mtu_floor).

probe_timeout_lowers_ceiling_test() ->
    PCB0 = #aiutp_pcb{mtu_floor = 528, mtu_ceiling = 1452,
                     mtu_probe_seq = 100, mtu_probe_size = 990},
    PCB1 = aiutp_mtu:on_probe_timeout(PCB0),
    ?assertEqual(989, PCB1#aiutp_pcb.mtu_ceiling).
```

### 7.2 集成测试场景

| 场景 | 描述 | 预期结果 |
|------|------|----------|
| 标准以太网 | MTU=1500 | 发现 ~1452 |
| VPN 隧道 | MTU=1400 | 发现 ~1352 |
| PPPoE | MTU=1492 | 发现 ~1444 |
| 极小 MTU | MTU=600 | 发现 ~552，回退正常 |

## 8. 风险和缓解

| 风险 | 缓解措施 |
|------|----------|
| 网络抖动导致误判 | 连续失败阈值，保守回退 |
| 探测包影响正常传输 | 低优先级，与数据包合并 |
| 路径 MTU 变化 | 周期性重新探测 |
| 性能开销 | 探测频率限制 |

## 9. 相关文档

- [需求文档](./mtu-discovery.md)
- [开发计划](./mtu-discovery-plan.md)
- [libutp 参考](../../development/libutp-mtu-analysis.md)
