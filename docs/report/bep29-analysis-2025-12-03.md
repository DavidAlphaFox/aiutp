# BEP-29 uTP 协议分析报告

> 日期: 2025-12-03
> 作者: Claude Code 分析

## 目录

- [Part A: BEP-29 标准核心机制理解](#part-a-bep-29-标准核心机制理解)
- [Part B: 项目实现分析](#part-b-项目实现分析)

---

# Part A: BEP-29 标准核心机制理解

> 本部分为对 BEP-29 规范的纯理论分析，不涉及具体项目实现

## A.1 丢包重传机制

### A.1.1 丢包检测方式

uTP 采用两种丢包检测机制：

**快速重传 (Fast Retransmit)**
- 当一个包之后有 **3 个或更多的后续包被确认**，但该包本身未收到 ACK 时，判定该包丢失
- 这与 TCP 的 "三次重复 ACK" 机制类似，但 uTP 基于 Selective ACK 扩展实现

**超时重传 (Timeout Retransmit)**
- 超时公式：`timeout = max(rtt + rtt_var * 4, 500ms)`
- 初始超时 1000ms
- 连续超时时采用指数退避（翻倍）

### A.1.2 Selective ACK 扩展

这是 uTP 的关键设计：

```
+--------+--------+--------+--------+
|  0x01  |  len   |   bitmask...    |
+--------+--------+--------+--------+
```

- 允许接收方报告非连续收到的包
- 每个 bit 代表 `ack_nr + 2 + bit_index` 位置的包是否收到
- 避免因单个丢包导致整个窗口重传

### A.1.3 拥塞响应

丢包发生时：
- `max_window = max_window * 0.5`
- 类似 TCP 的乘性减少（Multiplicative Decrease）
- 超时时窗口重置为最小值（150 字节）

---

## A.2 如何处理对端崩溃

### A.2.1 检测机制

uTP 没有专门的心跳机制，依赖以下方式检测对端崩溃：

**发送方视角**
- 持续发送数据但收不到 ACK
- 超时不断翻倍，最终达到极限
- 连续多次超时后判定连接失效

**接收方视角**
- 长时间收不到任何数据包
- 依赖应用层或系统超时

### A.2.2 ST_RESET 包

当收到针对**不存在连接**的包时：

```
发送 ST_RESET → 立即终止连接
```

场景：
- 对端崩溃后重启，丢失所有连接状态
- 本端发送的包到达后，对端发现无此连接
- 对端回复 ST_RESET，本端清理资源

### A.2.3 状态不一致处理

- 收到 RST 后必须立即销毁连接
- 不等待任何确认，不发送任何响应
- 这是一种"硬终止"

---

## A.3 如何处理断开连接

### A.3.1 优雅关闭 (Graceful Close)

使用 **ST_FIN** 包实现四次挥手：

```
端A                          端B
  |                            |
  |------- ST_FIN(seq=X) ----->|  A发送FIN
  |                            |
  |<------ ST_STATE(ack=X) ----|  B确认收到
  |                            |
  |<------ ST_FIN(seq=Y) ------|  B发送FIN
  |                            |
  |------- ST_STATE(ack=Y) --->|  A确认收到
  |                            |
```

### A.3.2 eof_pkt 机制

收到 ST_FIN 时：
1. 记录 `eof_pkt = fin_packet.seq_nr`
2. **不立即关闭**，继续等待乱序包
3. 只有当 `seq_nr <= eof_pkt` 的所有包都收到后，才真正关闭

这解决了一个关键问题：**FIN 可能先于数据包到达**

### A.3.3 强制关闭 (Forced Close)

使用 **ST_RESET** 包：
- 无需等待确认
- 立即释放资源
- 用于异常场景（超时过多、资源耗尽等）

### A.3.4 半关闭状态

uTP 支持半关闭：
- 一方发送 FIN 后仍可接收数据
- 但规范中未详细说明具体行为
- 实际实现中通常简化为全关闭

---

## A.4 核心设计思想总结

| 机制 | 设计目标 | 与TCP对比 |
|------|----------|-----------|
| seq_nr 按包计数 | 简化实现，避免重组 | TCP按字节计数 |
| Selective ACK | 精确丢包定位 | TCP是可选扩展 |
| 微秒级时间戳 | 精确延迟测量 | TCP毫秒级 |
| 最小窗口150字节 | 防止完全阻塞 | TCP可降为0 |
| RST无需确认 | 快速清理死连接 | 相同 |

uTP 本质上是在 UDP 之上实现了 TCP 的可靠性语义，同时针对 P2P 场景做了优化（如更激进的拥塞退避以避免占用过多带宽）。

---

# Part B: 项目实现分析

> 以下为 aiutp 项目实现与 BEP-29 标准的对比分析

1. [协议概述](#1-协议概述)
2. [丢包重传机制](#2-丢包重传机制)
3. [对端崩溃处理](#3-对端崩溃处理)
4. [断开连接处理](#4-断开连接处理)
5. [项目实现与标准差异](#5-项目实现与标准差异)

---

## 1. 协议概述

uTP (Micro Transport Protocol) 是一个基于 UDP 的可靠传输协议，定义于 [BEP-29](https://www.bittorrent.org/beps/bep_0029.html)。核心设计目标：

- **可靠传输**: 基于序列号和确认机制实现数据可靠传输
- **拥塞控制**: 实现 LEDBAT 算法，最小化对网络延迟的影响
- **低延迟**: 目标延迟约 100ms，避免饱和网络链路

### 1.1 数据包类型

| 类型 | 值 | 用途 |
|------|-----|------|
| ST_DATA | 0 | 携带数据的包 |
| ST_FIN | 1 | 优雅关闭连接 |
| ST_STATE | 2 | 状态/ACK 包（不携带数据） |
| ST_RESET | 3 | 强制重置连接 |
| ST_SYN | 4 | 建立连接 |

### 1.2 连接状态机

```
发起方:  IDLE → SYN_SENT → CONNECTED → DESTROY
接收方:  IDLE → SYN_RECV → CONNECTED → DESTROY
```

---

## 2. 丢包重传机制

### 2.1 BEP-29 标准定义

#### 丢包检测方式

1. **重复 ACK 检测 (Fast Retransmit)**
   - 当收到 3 个或更多重复的 ACK 时，认为包丢失
   - 触发快速重传，无需等待超时

2. **超时检测 (Timeout Retransmit)**
   - RTO = max(RTT + RTT_var * 4, 500ms)
   - 初始 RTO 默认为 1000ms
   - 连续超时时，RTO 翻倍（指数退避）

3. **选择性确认 (SACK)**
   - 通过位图标识已收到的离散包
   - 允许精确识别丢失的包，避免不必要的重传

#### 丢包后处理

- 窗口减半: `max_window = max_window * 0.5`
- 退出慢启动模式
- 重传丢失的包

### 2.2 项目实现分析

**代码位置**: `aiutp_pcb.erl`, `aiutp_tx.erl`, `aiutp_net.erl`

#### 2.2.1 重复 ACK 检测 (aiutp_pcb.erl:147-177)

```erlang
process_packet_1(#aiutp_packet{type = PktType,ack_nr = PktAckNR } = Packet,
                 #aiutp_pcb{cur_window_packets = CurWindowPackets,
                            duplicate_ack = DuplicateAck,
                            seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0->
  Seq = aiutp_util:bit16(SeqNR - CurWindowPackets -1),
  if (PktAckNR == Seq) and (PktType == ?ST_STATE) ->
      if DuplicateAck + 1 == ?DUPLICATE_ACKS_BEFORE_RESEND ->
          %% 达到阈值，触发快速重传
          PCB0 = aiutp_net:send_packet(aiutp_buffer:head(PCB#aiutp_pcb.outbuf),
                                       PCB#aiutp_pcb{duplicate_ack = 0}),
          process_packet_2(Packet,PCB0);
         true ->
          process_packet_2(Packet,PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1})
      end;
     true -> process_packet_2(Packet,PCB#aiutp_pcb{duplicate_ack = 0})
  end;
```

**关键参数**: `DUPLICATE_ACKS_BEFORE_RESEND = 4` (aiutp.hrl:22)

#### 2.2.2 超时重传 (aiutp_pcb.erl:533-565)

```erlang
check_timeouts_2({true,#aiutp_pcb{...} = PCB}) ->
  NewTimeout = RetransmitTimeout * 1.5,  %% 指数退避
  PCB0 = PCB#aiutp_pcb{
    max_window = erlang:max((MaxWindow div 2), ?MIN_WINDOW_SIZE),  %% 窗口减半
    slow_start = true  %% 重新进入慢启动
  },
  %% 标记所有未确认的包需要重传
  {CurWindow0,OutBuf0} = mark_need_resend(CurWindowPackets,CurWindow,Iter,OutBuf),
  %% 立即发送第一个需要重传的包
  {true,aiutp_net:send_packet(aiutp_buffer:head(OutBuf0), PCB1)};
```

#### 2.2.3 SACK 处理 (aiutp_tx.erl:27-44)

```erlang
%% 从 SACK 位图解析出已确认的序列号
map_sack_to_seq(<<Bits/big-unsigned-integer,Rest/bits>>,Index,Base,Acc) ->
  Offset = Index * 8,
  Acc1 = lists:foldl(
    fun(I,Acc0) ->
        Hint = Bits band (1 bsl I),
        if Hint > 0 -> [aiutp_util:bit16(Base + Offset + I)|Acc0];
           true -> Acc0
        end
    end,Acc,lists:seq(0, 7)),
  map_sack_to_seq(Rest,Index+1,Base,Acc1).
```

### 2.3 机制总结图

```
                    ┌─────────────────┐
                    │   发送数据包     │
                    └────────┬────────┘
                             │
                             ▼
              ┌──────────────────────────────┐
              │  等待 ACK / 超时检查          │
              └──────────────────────────────┘
                     │              │
           收到 ACK  │              │ 超时
                     ▼              ▼
        ┌────────────────┐   ┌─────────────────┐
        │ 是重复 ACK?    │   │ RTO 超时?       │
        └───────┬────────┘   └────────┬────────┘
           Yes  │  No                 │ Yes
                │   │                 │
                ▼   │                 ▼
    ┌──────────────────┐     ┌──────────────────┐
    │ 重复次数 >= 4?   │     │ 1. 窗口减半      │
    └────────┬─────────┘     │ 2. RTO *= 1.5   │
        Yes  │               │ 3. 标记重传      │
             ▼               │ 4. 发送首包      │
    ┌──────────────────┐     └──────────────────┘
    │ 快速重传首个未   │
    │ 确认包           │
    └──────────────────┘
             │
             ▼
    ┌──────────────────┐
    │ SACK 分析        │
    │ 批量重传丢失包   │
    └──────────────────┘
```

---

## 3. 对端崩溃处理

### 3.1 BEP-29 标准定义

协议通过以下机制检测对端不可达：

1. **Keep-alive 超时**
   - 周期: 29 秒（针对 NAT 设备优化）
   - 连续无响应超过阈值则认为连接丢失

2. **RTO 超时累积**
   - 连续多次 RTO 超时表明对端可能已崩溃
   - 达到最大重传次数后放弃连接

3. **ST_RESET 处理**
   - 收到 RESET 包表明对端状态不一致
   - 立即终止连接

### 3.2 项目实现分析

**代码位置**: `aiutp_pcb.erl:503-529`

#### 3.2.1 Keep-alive 超时检测

```erlang
check_timeouts_1(#aiutp_pcb{time = Now,
                            last_got_packet = LastGotPacket,
                            close_requested = CloseRequested} = PCB)
  when (LastGotPacket > 0),(Now - LastGotPacket > ?KEEPALIVE_INTERVAL * 2) ->
  %% 超过 58 秒 (29000ms * 2) 没有收到任何包
  io:format("CLOSED due to MAX Keepalive: ~p~n",[LastGotPacket]),
  if CloseRequested == true ->
      {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};
     true ->
      {false, PCB#aiutp_pcb{state = ?CS_RESET}}
  end;
```

#### 3.2.2 RTT 异常检测

```erlang
check_timeouts_1(#aiutp_pcb{rtt = RTT,close_requested = CloseRequested} = PCB)
  when (RTT > 6000) ->
  %% RTT 超过 6 秒，认为连接已不可用
  io:format("CLOSED due to MAX RTT: ~p~n",[RTT]),
  if CloseRequested == true ->
      {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};
     true ->
      {false, PCB#aiutp_pcb{state = ?CS_RESET}}
  end;
```

#### 3.2.3 最大重传次数检测

```erlang
check_timeouts_1(#aiutp_pcb{state = State,
                            close_requested = CloseRequested,
                            retransmit_count = RetransmitCount,
                            brust = false} = PCB)
  when (RetransmitCount >= 4);
       ((State == ?CS_SYN_SENT) and RetransmitCount > 2) ->
  %% 重传超过 4 次，或 SYN 重传超过 2 次
  io:format("CLOSED due to MAX retransmit: ~p~n",[RetransmitCount]),
  ...
```

#### 3.2.4 RESET 包处理

```erlang
process(?ST_RESET,
        #aiutp_packet{conn_id = ConnId},
        #aiutp_pcb{conn_id_send = ConnIdSend,
                   conn_id_recv = ConnIdRecv,
                   close_requested = CloseRequested} = PCB)->
  if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
      if CloseRequested == true ->
          PCB#aiutp_pcb{state = ?CS_DESTROY};
         true ->
          PCB#aiutp_pcb{state = ?CS_RESET}
      end;
     true -> PCB  %% 忽略无效的 RESET
  end;
```

### 3.3 崩溃检测状态图

```
                    ┌─────────────────┐
                    │   正常连接状态   │
                    │   CS_CONNECTED  │
                    └────────┬────────┘
                             │
         ┌───────────────────┼───────────────────┐
         │                   │                   │
         ▼                   ▼                   ▼
┌────────────────┐  ┌────────────────┐  ┌────────────────┐
│ 收到 ST_RESET  │  │ Keep-alive 超时│  │ 重传次数 >= 4  │
│                │  │ (58秒无响应)   │  │ 或 RTT > 6s    │
└───────┬────────┘  └───────┬────────┘  └───────┬────────┘
        │                   │                   │
        └───────────────────┼───────────────────┘
                            │
                            ▼
              ┌──────────────────────────┐
              │    close_requested?      │
              └──────────────────────────┘
                    │            │
              true  │            │ false
                    ▼            ▼
           ┌─────────────┐  ┌─────────────┐
           │ CS_DESTROY  │  │  CS_RESET   │
           │ (清理资源)  │  │ (发送RESET) │
           └─────────────┘  └─────────────┘
```

---

## 4. 断开连接处理

### 4.1 BEP-29 标准定义

#### 优雅关闭 (Graceful Close)

1. 发送方发送 ST_FIN 包，包含最终序列号
2. 接收方确认所有数据后发送 FIN 响应
3. 双方都确认对方 FIN 后释放资源

#### 强制关闭 (Forced Close)

1. 发送 ST_RESET 包
2. 立即释放本地资源
3. 对端收到 RESET 后也释放资源

### 4.2 项目实现分析

**代码位置**: `aiutp_pcb.erl:576-594`, `aiutp_net.erl:99-101`

#### 4.2.1 发起优雅关闭

```erlang
close(#aiutp_pcb{fin_sent_acked = FinSentAcked,fin_sent = FinSent} = PCB)->
  PCB0 = PCB#aiutp_pcb{
    read_shutdown = true,      %% 禁止读取
    close_requested = true     %% 标记关闭请求
  },
  if FinSent == false ->
      %% 发送 FIN 包
      aiutp_net:send_fin(PCB0#aiutp_pcb{fin_sent = true});
     FinSentAcked == true ->
      %% FIN 已被确认，可以销毁
      PCB0#aiutp_pcb{state = ?CS_DESTROY};
     true ->
      PCB0
  end.
```

#### 4.2.2 发送 FIN 包

```erlang
send_fin(#aiutp_pcb{outque = OutQue} = PCB)->
  %% FIN 作为特殊数据包加入发送队列
  OutQue0 = aiutp_queue:push_back({?ST_FIN,<<>>}, OutQue),
  flush_queue(PCB#aiutp_pcb{outque = OutQue0}).
```

#### 4.2.3 接收 FIN 处理

```erlang
process_packet_4(#aiutp_packet{type = PktType,seq_nr = PktSeqNR} = Packet,
                 #aiutp_pcb{got_fin = GotFin} = PCB)->
  PCB0 =
    if (PktType == ?ST_FIN) and (GotFin == false)->
        %% 记录收到 FIN 及其序列号
        PCB#aiutp_pcb{got_fin = true, eof_pkt = PktSeqNR};
       true -> PCB
    end,
  aiutp_rx:in(Packet, PCB0).
```

#### 4.2.4 连接关闭状态判断

```erlang
closed(#aiutp_pcb{state = State,
                  fin_sent = FinSent,
                  fin_sent_acked = FinSentAcked,
                  got_fin = GotFin,
                  got_fin_reached = GotFinReached,
                  cur_window_packets = CurWindowPackets})
  when State == ?CS_DESTROY->
  if (FinSent and FinSentAcked) or
     (GotFin and GotFinReached) ->
      {closed, normal};        %% 正常关闭
     FinSent and CurWindowPackets == 1 ->
      {closed, normal};
     (FinSent == false) and (GotFin == false) ->
      {closed, timeout};       %% 超时关闭
     true ->
      {closed, crash}          %% 异常关闭
  end;
```

### 4.3 关闭流程图

```
发起方                                    接收方
   │                                        │
   │  close() 调用                          │
   ├────────────────────────────────────────┤
   │                                        │
   │  fin_sent = true                       │
   │  发送 ST_FIN (seq=N)                   │
   │ ──────────────────────────────────────>│
   │                                        │ got_fin = true
   │                                        │ eof_pkt = N
   │                                        │
   │                   ST_STATE (ack=N)     │
   │ <──────────────────────────────────────│
   │  fin_sent_acked = true                 │
   │                                        │
   │                                        │  close() 调用
   │                                        ├──────────────
   │                   ST_FIN (seq=M)       │
   │ <──────────────────────────────────────│
   │  got_fin = true                        │
   │                                        │
   │  ST_STATE (ack=M)                      │
   │ ──────────────────────────────────────>│
   │                                        │ fin_sent_acked = true
   │                                        │
   │  state = CS_DESTROY                    │ state = CS_DESTROY
   │  {closed, normal}                      │ {closed, normal}
   │                                        │
   ▼                                        ▼
```

---

## 5. 项目实现与标准差异

### 5.1 差异汇总表

| 特性 | BEP-29 标准 | 项目实现 | 差异程度 | 说明 |
|------|-------------|----------|----------|------|
| RTO 计算 | max(RTT + 4*RTTVar, 500ms) | clamp(RTT + 4*RTTVar, 600, 6000) | ⚠️ 轻微 | 下限 600ms vs 500ms |
| 重复 ACK 阈值 | 3 | 4 | ⚠️ 轻微 | 更保守，减少误判 |
| 超时退避 | RTO * 2 | RTO * 1.5 | ⚠️ 轻微 | 更激进的重试 |
| Keep-alive | 29s 发送探测 | 58s 超时判死 | ✅ 兼容 | 实现思路一致 |
| 最大重传次数 | 未明确规定 | 4 次 (SYN: 2次) | ✅ 合理 | 实现合理 |
| RTT 异常阈值 | 未明确规定 | 6000ms | ✅ 合理 | 自定义扩展 |
| SACK 支持 | 最大 4 字节 (32包) | 最大 4 字节 (32包) | ✅ 一致 | 完全符合 |
| 窗口衰减 | 0.5 | 0.5 (丢包) / 0.8 (空闲) | ⚠️ 扩展 | 增加了空闲衰减 |

### 5.2 详细差异分析

#### 5.2.1 RTO 下限差异

**标准**: 500ms
**实现**: 600ms (aiutp_pcb.erl:243)

```erlang
{RTT0,RTTVar0,aiutp_util:clamp((RTT0 + RTTVar0 * 4),600,6000),RTTHist0};
```

**影响**: 在高延迟网络中可能稍慢重传，但减少了不必要的重传

#### 5.2.2 重复 ACK 阈值

**标准**: 3 个重复 ACK
**实现**: 4 个重复 ACK

```erlang
-define(DUPLICATE_ACKS_BEFORE_RESEND,4).
```

**影响**: 更保守，减少因网络抖动导致的误判，但丢包检测稍慢

#### 5.2.3 超时退避因子

**标准**: RTO 翻倍 (×2)
**实现**: RTO ×1.5

```erlang
NewTimeout = RetransmitTimeout * 1.5,
```

**影响**: 更激进地重试，适合低延迟场景

#### 5.2.4 窗口衰减扩展

项目增加了空闲窗口衰减机制 (aiutp_pcb.erl:253-271):

```erlang
maybe_decay_win(#aiutp_pcb{time = Now, max_window = MaxWindow,
                            last_rwin_decay = LastRWinDecay} = PCB)->
  if (Now - LastRWinDecay) < ?MAX_WINDOW_DECAY -> PCB;
     true ->
      MaxWindow0 = erlang:trunc(MaxWindow * 0.8),  %% 0.8 衰减
      ...
```

**目的**: 避免长时间空闲后突发大量数据

### 5.3 已知问题

#### 5.3.1 代码注释指出的问题

```erlang
%% 此处实现和C++版本有差异，会浪费带宽，但是不是Bug
%% 需要在后期进行优化
selective_ack_packet(...)
```

SACK 处理可能导致不必要的重传。

#### 5.3.2 拼写错误导致的潜在 Bug

- `aiutp_pcb.erl:505`: `{fasle,PCB#...}` 应为 `{false,PCB#...}`
- 这会导致返回 atom `fasle` 而非 `false`，可能影响模式匹配

### 5.4 改进建议

1. **修复拼写错误**: `fasle` → `false`
2. **考虑调整 RTO 下限**: 500ms 符合标准
3. **优化 SACK 处理**: 减少不必要的重传
4. **添加日志**: 用 logger 替代 io:format

---

## 附录: 关键常量定义

```erlang
%% aiutp.hrl
-define(DUPLICATE_ACKS_BEFORE_RESEND, 4).    %% 快速重传阈值
-define(KEEPALIVE_INTERVAL, 29000).           %% Keep-alive 间隔 (ms)
-define(PACKET_SIZE, 1296).                   %% 数据包大小
-define(MIN_WINDOW_SIZE, 2906).               %% 最小窗口
-define(OUTGOING_BUFFER_MAX_SIZE, 1024).      %% 发送缓冲区大小
-define(REORDER_BUFFER_MAX_SIZE, 1024).       %% 重排序缓冲区大小
```

---

## 总结

aiutp 项目整体上正确实现了 BEP-29 uTP 协议的核心功能：

1. **丢包重传**: 实现了快速重传 (4 重复 ACK) 和超时重传，支持 SACK
2. **对端崩溃**: 通过 Keep-alive 超时、RTT 异常、最大重传次数三重检测
3. **连接关闭**: 支持优雅关闭 (FIN) 和强制关闭 (RESET)

存在的差异主要是参数调优层面，不影响协议兼容性。建议优先修复拼写错误问题。
