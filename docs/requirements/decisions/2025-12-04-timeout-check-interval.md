# 超时检查间隔 (TIMEOUT_CHECK_INTERVAL) 决策

> 日期: 2025-12-04
> 状态: **已实现**

## 背景

在对比 aiutp 与 libutp 实现时，需要确定超时检查定时器的正确设计。

## libutp 的两层设计

### 架构

```
┌─────────────────────────────────────────────────────────────┐
│                      应用层                                  │
│   定时器触发频率: 50ms (建议)                                │
│                    │                                        │
│                    ▼                                        │
│            utp_check_timeouts()                             │
└────────────────────┼────────────────────────────────────────┘
                     │
┌────────────────────┼────────────────────────────────────────┐
│                 libutp 库内部                                │
│                    │                                        │
│         ┌──────────▼──────────┐                             │
│         │ 节流检查 (500ms)     │                             │
│         │ if (now - last_check │                             │
│         │     < TIMEOUT_CHECK_ │                             │
│         │       INTERVAL)      │                             │
│         │   return;           │                             │
│         └──────────┬──────────┘                             │
│                    │ (通过节流)                              │
│                    ▼                                        │
│         ┌──────────────────────┐                            │
│         │ RST 信息清理          │                            │
│         │ 遍历所有 socket       │                            │
│         │ conn->check_timeouts()│                            │
│         └──────────────────────┘                            │
└─────────────────────────────────────────────────────────────┘
```

### libutp 代码

```c
void utp_check_timeouts(utp_context *ctx)
{
    ctx->current_ms = utp_call_get_milliseconds(ctx, NULL);

    // 节流：不到 500ms 就直接返回
    if (ctx->current_ms - ctx->last_check < TIMEOUT_CHECK_INTERVAL)
        return;

    ctx->last_check = ctx->current_ms;

    // RST 信息清理
    for (size_t i = 0; i < ctx->rst_info.GetCount(); i++) { ... }

    // 遍历所有 socket 检查超时
    while ((keyData = ctx->utp_sockets->Iterate(it))) {
        conn->check_timeouts();
        if (conn->state == CS_DESTROY) delete conn;
    }
}
```

### 设计原因

1. **解耦调用频率和检查频率**：应用层可以高频调用，但不会造成过大开销
2. **及时刷新发送队列**：每次调用都可以刷新待发送的包
3. **节流完整检查**：RTO/Keepalive 等检查最多每 500ms 一次

## aiutp 实现

### 两层设计（与 libutp 一致）

```
┌─────────────────────────────────────────────────────────────┐
│ aiutp_channel (gen_statem)                                  │
│                                                             │
│  tick_timer (CHANNEL_TICK_INTERVAL = 50ms)                  │
│       │                                                     │
│       ▼                                                     │
│  aiutp_pcb:check_timeouts(PCB)                              │
└───────┼─────────────────────────────────────────────────────┘
        │
┌───────┼─────────────────────────────────────────────────────┐
│ aiutp_pcb_timeout                                           │
│       │                                                     │
│       ├─ 每次: flush_packets (刷新待发送的包)               │
│       │                                                     │
│       ├─ 节流检查 (TIMEOUT_CHECK_INTERVAL = 500ms)          │
│       │   if (Now - LastCheck < 500ms) return              │
│       │                                                     │
│       └─ [通过节流] do_check_timeouts()                     │
│           - RTO 超时检查                                    │
│           - Keepalive 检查                                  │
│           - 状态转换检查                                    │
└─────────────────────────────────────────────────────────────┘
```

### 常量定义

```erlang
%% include/aiutp.hrl

%% Channel tick 定时器间隔（毫秒）
%% 相当于 libutp 建议的应用层调用频率 (50ms)
-define(CHANNEL_TICK_INTERVAL, 50).

%% 超时检查节流阈值（毫秒）
%% 与 libutp TIMEOUT_CHECK_INTERVAL 一致
%% check_timeouts 内部使用此值节流，实际超时检查最多每 500ms 执行一次
-define(TIMEOUT_CHECK_INTERVAL, 500).
```

### PCB 字段

```erlang
%% include/aiutp.hrl  #aiutp_pcb{}

%% 上次超时检查的时间（毫秒）
%% 用于节流，防止过于频繁的检查（与 libutp TIMEOUT_CHECK_INTERVAL 对应）
last_timeout_check = 0 :: non_neg_integer(),
```

### 节流实现

```erlang
%% src/aiutp_pcb_timeout.erl

check_timeouts(#aiutp_pcb{last_timeout_check = LastCheck} = PCB) ->
    Now = aiutp_util:millisecond(),
    %% 每次调用都刷新待发送的包
    PCB0 = aiutp_net:flush_packets(PCB),
    %% 节流检查：与 libutp TIMEOUT_CHECK_INTERVAL 一致
    case Now - LastCheck < ?TIMEOUT_CHECK_INTERVAL of
        true ->
            %% 不到 500ms，跳过完整的超时检查
            PCB0;
        false ->
            %% 执行完整超时检查，更新 last_timeout_check
            PCB1 = PCB0#aiutp_pcb{time = Now, last_timeout_check = Now},
            do_check_timeouts(PCB1)
    end.
```

## 行为对比

| 层级 | libutp | aiutp |
|------|--------|-------|
| 外层定时器 | 应用层 50ms | `CHANNEL_TICK_INTERVAL` 50ms |
| 每次调用 | - | `flush_packets` |
| 节流阈值 | `TIMEOUT_CHECK_INTERVAL` 500ms | `TIMEOUT_CHECK_INTERVAL` 500ms |
| 完整检查 | RTO/Keepalive/状态转换 | 相同 |

## 好处

1. **与 libutp 设计一致**：两层架构，节流逻辑相同
2. **flush_packets 更及时**：每 50ms 刷新一次，发送更及时
3. **完整检查有节流**：避免过度 CPU 开销
4. **灵活扩展**：tick handler 可用于其他高频操作

## 相关文档

- [数据发送流程](../../development/data-sending-flow.md) - 详细描述 flush_queue 和 flush_packets 的作用

## 修改的文件

| 文件 | 修改内容 |
|------|----------|
| `include/aiutp.hrl` | 添加 `CHANNEL_TICK_INTERVAL`、`TIMEOUT_CHECK_INTERVAL`、`last_timeout_check` 字段 |
| `src/aiutp_pcb_timeout.erl` | 添加节流逻辑 |
| `src/aiutp_channel.erl` | 更新注释说明 |

## 参考资料

- [libutp utp_internal.cpp](https://github.com/bittorrent/libutp/blob/master/utp_internal.cpp)
- [BEP-29: uTP Micro Transport Protocol](https://www.bittorrent.org/beps/bep_0029.html)
