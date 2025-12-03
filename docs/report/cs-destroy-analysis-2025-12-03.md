# CS_DESTROY 状态转换分析报告

> 日期: 2025-12-03
> 参考: [BEP-29](https://www.bittorrent.org/beps/bep_0029.html), [libutp](https://github.com/bittorrent/libutp)

## 概述

本报告分析了 aiutp 项目中进入 `CS_DESTROY` 状态的所有代码路径，并与 libutp 参考实现进行对比，识别出需要修复的问题。

## libutp 中的 CS_DESTROY 触发条件

根据 libutp 源码分析，以下情况会导致连接进入 `CS_DESTROY` 状态：

1. **SYN_RECV 超时**: 重传 >= 4 次后，发送 RESET，进入 CS_DESTROY
2. **重传次数超限**: `retransmit_count >= 4` 且 `close_requested = true`
3. **收到 RST 包**: 且 `close_requested = true`
4. **FIN 被确认**: `fin_sent = true` 且 `cur_window_packets == 0` 且 `close_requested = true`
5. **ICMP 错误**: 且 `close_requested = true`
6. **close() 在无效状态调用**: 如 `utp_connect()` 在非 UNINITIALIZED 状态

## aiutp 当前实现

### 进入 CS_DESTROY 的代码路径

| 文件 | 行号 | 触发条件 | 说明 |
|------|------|---------|------|
| aiutp_pcb_timeout.erl | 122 | SYN_RECV 超时 | 直接进入，未发 RESET |
| aiutp_pcb_timeout.erl | 131 | keepalive 超时 + close_requested | 正确 |
| aiutp_pcb_timeout.erl | 140 | RTT > 6000 + close_requested | 正确 |
| aiutp_pcb_timeout.erl | 153 | 重传超限 + close_requested | 正确 |
| aiutp_pcb.erl | 168 | 收到 RST + close_requested | 正确 |
| aiutp_pcb.erl | 368 | FIN ACKed + close_requested | 正确 |
| aiutp_pcb.erl | 517 | close() 在 UNINIT/IDLE/DESTROY | 正确 |
| aiutp_pcb.erl | 522 | close() 在 SYN_SENT | 逻辑待确认 |
| aiutp_pcb.erl | 526 | close() 在 SYN_RECV | 正确 |
| aiutp_pcb.erl | 532 | close() 且 fin_sent_acked | 正确 |

## 发现的问题

### 问题 1: SYN_RECV 超时未发送 RESET

**位置**: `aiutp_pcb_timeout.erl:120-122`

**当前代码**:
```erlang
check_timeouts_1(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
    {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};
```

**问题**: 直接进入 CS_DESTROY，没有发送 RESET 通知对端。

**libutp 行为**: SYN_RECV 超时后会发送 RESET，然后进入 CS_DESTROY。

**修复方案**: 在状态转换前调用 `aiutp_net:send_reset/1`。

### 问题 2: closed/1 函数对 got_fin_reached 的处理

**位置**: `aiutp_pcb.erl:113-114`

**当前代码**:
```erlang
closed(#aiutp_pcb{got_fin = true, got_fin_reached = true}) ->
    {closed, normal};
```

**问题**:
- `got_fin_reached = true` 只表示收到了对端的 FIN 且所有包已到达
- 这应该触发 EOF 通知，而不是表示连接已关闭
- 连接应该等待本地也发送 FIN 并被确认后才真正关闭

**libutp 行为**:
- `got_fin_reached` 只触发 `UTP_STATE_EOF` 回调通知上层
- 不会自动进入 CS_DESTROY
- 只有当 `close_requested && fin_sent_acked` 时才进入 CS_DESTROY

**修复方案**: 移除这个子句，让上层通过其他方式（如回调）得知 EOF。

### 问题 3: close() 在 SYN_SENT 状态的处理

**位置**: `aiutp_pcb.erl:518-523`

**当前代码**:
```erlang
close(#aiutp_pcb{state = State, rto = RTO} = PCB)
  when State == ?CS_SYN_SENT ->
    PCB#aiutp_pcb{
        rto_timeout = erlang:min(RTO * 2, 60) + aiutp_util:millisecond(),
        state = ?CS_DESTROY
    };
```

**问题**: 直接进入 CS_DESTROY，但又设置了 rto_timeout，逻辑不一致。

**修复方案**: 设置 `close_requested = true`，让超时处理来完成状态转换。

## 修复计划

1. **修复 SYN_RECV 超时**: 添加 `send_reset` 调用
2. **修复 closed/1**: 移除 `got_fin_reached` 子句
3. **修复 close() 在 SYN_SENT**: 统一为设置 `close_requested`，等待超时

## 状态转换图

```
正常连接建立:
  CS_IDLE → CS_SYN_SENT → CS_CONNECTED
  CS_IDLE → CS_SYN_RECV → CS_CONNECTED

正常关闭 (主动):
  CS_CONNECTED → [send FIN, fin_sent=true] → [recv ACK, fin_sent_acked=true]
               → [close_requested=true] → CS_DESTROY

正常关闭 (被动):
  CS_CONNECTED → [recv FIN, got_fin=true] → [all pkts, got_fin_reached=true]
               → [user close, send FIN] → [recv ACK] → CS_DESTROY

异常关闭:
  ANY → [recv RST] → CS_RESET (或 CS_DESTROY if close_requested)
  ANY → [timeout] → CS_RESET (或 CS_DESTROY if close_requested)
```

## 参考资料

- [BEP-29: uTorrent transport protocol](https://www.bittorrent.org/beps/bep_0029.html)
- [libutp source code](https://github.com/bittorrent/libutp/blob/master/utp_internal.cpp)
