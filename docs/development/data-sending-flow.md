# 数据发送流程

> 本文档描述 aiutp 的数据发送机制，包括用户调用 `aiutp:send()` 后的完整数据流和定时器触发的刷新机制。

## 概述

aiutp 的数据发送分为两个路径：

1. **主动发送**：用户调用 `aiutp:send()` 时立即尝试发送
2. **定时刷新**：tick 定时器触发 `flush_packets` 发送待发送/重传的包

## 数据结构

```
┌─────────────────────────────────────────────────────────────┐
│                         PCB                                 │
│                                                             │
│  outque (发送队列)          outbuf (发送缓冲区)              │
│  ┌─────────────────┐       ┌─────────────────┐              │
│  │ {ST_DATA, Bin1} │       │ packet_wrap #1  │ ← 已打包     │
│  │ {ST_DATA, Bin2} │       │ packet_wrap #2  │   等待发送   │
│  │ {ST_FIN,  <<>>} │       │ packet_wrap #3  │   或重传     │
│  └─────────────────┘       └─────────────────┘              │
│       原始数据                   已打包的 packet              │
└─────────────────────────────────────────────────────────────┘
```

| 结构 | 内容 | 说明 |
|------|------|------|
| `outque` | `{Type, Binary}` | 原始数据队列，等待打包 |
| `outbuf` | `#aiutp_packet_wrap{}` | 已打包的数据包，等待发送或重传 |

## 主动发送：aiutp:send() 调用链

```
aiutp:send(Conn, Data)
        │
        ▼
aiutp_channel:send(Pid, Data)           [gen_statem:call, 同步调用]
        │
        ▼
aiutp_pcb:write(Data, PCB)
        │
        ▼
aiutp_tx:in(Data, PCB)
        │
        ├─1─▶ aiutp_queue:push_back({ST_DATA, Data}, OutQue)  [放入 outque]
        │
        └─2─▶ aiutp_net:flush_queue(PCB)                      [立即尝试发送]
                    │
                    ▼
              do_flush_queue(PCB)
                    │
                    ├─ is_full? ──true──▶ 返回（数据留在队列，等待窗口打开）
                    │
                    └─ false ──▶ send_queued_data(PCB)
                                        │
                                        ▼
                                send_data_chunk(...)          [分片处理]
                                        │
                                        ▼
                                send_new_packet(...)          [打包放入 outbuf]
                                        │
                                        ▼
                                do_send(Socket, Content)      [UDP 发送]
```

### 关键代码

```erlang
%% aiutp_tx.erl:70-74
in(Data, #aiutp_pcb{outque = OutQue} = PCB) ->
    OutQue1 = aiutp_queue:push_back({?ST_DATA, Data}, OutQue),
    PCB1 = aiutp_net:flush_queue(PCB#aiutp_pcb{outque = OutQue1}),
    maybe_transition_to_full(PCB1).

%% aiutp_net.erl:429-432
{IsFull, PCB2} = is_full(-1, PCB1),
case IsFull of
    true -> PCB2;                    %% 窗口满，数据留在队列
    false -> send_queued_data(PCB2)  %% 窗口未满，立即发送
end.
```

### 发送条件

| 窗口状态 | 行为 | 数据位置 |
|----------|------|----------|
| **未满** | 立即打包并通过 UDP 发送 | outque → outbuf → UDP |
| **已满** | 数据留在队列等待 | 停留在 outque |

**结论**：`aiutp:send()` 会**立即发送**数据（如果窗口允许），不需要等待定时器。

## 定时刷新：flush_packets

### 触发时机

```erlang
%% aiutp_pcb_timeout.erl - check_timeouts
%% 每次 tick (50ms) 都会调用 flush_packets
PCB0 = aiutp_net:flush_packets(PCB),
```

### 作用

`flush_packets` 遍历 **outbuf**（发送缓冲区），发送所有待发送的数据包。

```erlang
%% aiutp_net.erl:390-391
ShouldSend = (WrapPacket#aiutp_packet_wrap.transmissions == 0) orelse
             (WrapPacket#aiutp_packet_wrap.need_resend == true),
```

发送两类包：

| 条件 | 说明 |
|------|------|
| `transmissions == 0` | **新包**：已打包但因窗口满未发送 |
| `need_resend == true` | **重传包**：被标记需要重发（RTO 超时或快速重传） |

### 数据流

```
                    outbuf
                      │
        ┌─────────────┼─────────────┐
        │             │             │
        ▼             ▼             ▼
   packet_wrap    packet_wrap   packet_wrap
   transmissions=0  transmissions=2  transmissions=1
   (新包，未发送)    need_resend=true  (已发送，等待ACK)
        │             │
        └──────┬──────┘
               │
               ▼
         flush_packets
               │
               ▼
           do_send()
               │
               ▼
             UDP
```

## flush_queue vs flush_packets

| 函数 | 数据源 | 触发时机 | 作用 |
|------|--------|----------|------|
| `flush_queue` | outque | `aiutp:send()` 调用时 | 将原始数据打包并发送 |
| `flush_packets` | outbuf | tick 定时器 (50ms) | 发送新包或重传包 |

### 协作关系

```
用户调用 send():
    outque ──flush_queue──▶ outbuf ──send──▶ UDP
                              │
                              │ 如果窗口满，包留在 outbuf
                              ▼
tick 定时器 (50ms):
    outbuf ──flush_packets──▶ UDP

    场景1: 窗口打开后发送之前因窗口满而延迟的新包
    场景2: 发送被标记为 need_resend 的重传包
```

## 设计与 libutp 的对应

### libutp 架构

```c
// 用户调用
utp_write(socket, data, len);  // 放入队列并尝试发送

// 定时器调用
utp_check_timeouts(ctx);       // 节流后调用 flush_packets
```

### aiutp 架构

```erlang
%% 用户调用
aiutp:send(Conn, Data)  %% → flush_queue → 立即发送

%% 定时器调用 (50ms)
check_timeouts(PCB)     %% → flush_packets → 发送待发送/重传包
```

### 行为一致性

| 场景 | libutp | aiutp |
|------|--------|-------|
| 用户发送数据 | 立即尝试发送 | ✅ 立即尝试发送 |
| 窗口满 | 数据留在队列 | ✅ 数据留在队列 |
| 定时刷新 | flush_packets | ✅ flush_packets |
| 重传触发 | 标记 need_resend | ✅ 标记 need_resend |

## 总结

1. **`aiutp:send()` 会立即发送**（如果窗口允许）
2. **窗口满时数据留在队列**，等待 `flush_packets` 发送
3. **`flush_packets` 每 50ms 调用一次**，发送：
   - 因窗口满而延迟的新包
   - 被标记为需要重传的包
4. **设计与 libutp 一致**，确保协议行为正确
