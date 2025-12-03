# uTP 快速重传机制详解

本文档详细说明 uTP 协议中的快速重传机制，包括重复 ACK 触发和 SACK 触发两种方式，
并提供具体场景示例。

## 1. 快速重传概述

快速重传是一种丢包恢复机制，无需等待 RTO 超时即可重传丢失的数据包。
uTP 支持两种快速重传触发方式：

1. **重复 ACK 触发**：连续收到 3 个相同 ack_nr 的 ST_STATE 包
2. **SACK 触发**：通过选择性确认位图检测到包被跳过 3 次

## 2. 关键常量

```erlang
%% 触发快速重传的阈值
-define(DUPLICATE_ACKS_BEFORE_RESEND, 3).

%% 每次 SACK 事件最多重传的包数
-define(MAX_FAST_RESEND_PACKETS, 4).
```

## 3. 场景 1：重复 ACK 触发快速重传

### 场景描述
发送方发送了包 10, 11, 12, 13，但包 10 丢失。

### 时序图

```
发送方                                      接收方
   |                                           |
   |----[seq=10, DATA]---X (丢失)              |
   |----[seq=11, DATA]------------------------>|
   |                                           | 收到 11，但期望 10
   |<---[ack=9, ST_STATE]----------------------| 发送重复 ACK
   |----[seq=12, DATA]------------------------>|
   |                                           | 收到 12，仍期望 10
   |<---[ack=9, ST_STATE]----------------------| 发送重复 ACK (dup=1)
   |----[seq=13, DATA]------------------------>|
   |                                           | 收到 13，仍期望 10
   |<---[ack=9, ST_STATE]----------------------| 发送重复 ACK (dup=2)
   |                                           |
   | [检测到 dup_ack=3，触发快速重传]          |
   |----[seq=10, DATA, 重传]------------------>|
   |                                           | 收到 10，现在可以确认所有
   |<---[ack=13, ST_STATE]---------------------|
```

### 代码处理流程

```erlang
%% handle_duplicate_acks/2 中的检测逻辑
handle_duplicate_acks(#aiutp_packet{type = PktType, ack_nr = PktAckNR} = Packet,
                      #aiutp_pcb{cur_window_packets = CurWindowPackets,
                                 duplicate_ack = DuplicateAck,
                                 seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0 ->
    %% 计算最旧未确认包的序列号
    Seq = aiutp_util:bit16(SeqNR - CurWindowPackets - 1),

    if (PktAckNR == Seq) and (PktType == ?ST_STATE) ->
        %% 收到重复 ACK
        if DuplicateAck + 1 == ?DUPLICATE_ACKS_BEFORE_RESEND ->
            %% 达到阈值，触发快速重传
            PCB0 = aiutp_net:send_packet(
                       aiutp_buffer:head(PCB#aiutp_pcb.outbuf),
                       PCB#aiutp_pcb{duplicate_ack = 0}),
            process_ack_and_sack(Packet, PCB0);
           true ->
            %% 继续计数
            process_ack_and_sack(Packet, PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1})
        end;
       true ->
        %% 不是重复 ACK，重置计数器
        process_ack_and_sack(Packet, PCB#aiutp_pcb{duplicate_ack = 0})
    end.
```

### 关键点
- 只有 ST_STATE 包（纯 ACK）才计入重复 ACK
- ack_nr 必须等于最旧未确认包的前一个序列号
- 达到 3 次后立即重传最旧的未确认包
- 重传后重置 duplicate_ack 计数器

---

## 4. 场景 2：SACK 触发单包快速重传

### 场景描述
发送方发送了包 10-15，包 11 丢失，其他包都到达。

### 时序图

```
发送方                                      接收方
   |                                           |
   |----[seq=10, DATA]------------------------>| 收到 10
   |----[seq=11, DATA]---X (丢失)              |
   |----[seq=12, DATA]------------------------>| 收到 12 (乱序)
   |----[seq=13, DATA]------------------------>| 收到 13 (乱序)
   |----[seq=14, DATA]------------------------>| 收到 14 (乱序)
   |                                           |
   |<---[ack=10, SACK={12,13,14}]--------------|
   |                                           |
   | [解析 SACK，更新 skip_count]              |
   | [包 11 的 skip_count 增加]                |
   |                                           |
   |----[seq=15, DATA]------------------------>| 收到 15 (乱序)
   |<---[ack=10, SACK={12,13,14,15}]-----------|
   | [包 11 的 skip_count = 2]                 |
   |                                           |
   |----[seq=16, DATA]------------------------>| 收到 16 (乱序)
   |<---[ack=10, SACK={12,13,14,15,16}]--------|
   | [包 11 的 skip_count = 3，触发快速重传]   |
   |                                           |
   |----[seq=11, DATA, 重传]------------------>| 收到 11
   |<---[ack=16, ST_STATE]---------------------| 确认所有包
```

### SACK 位图结构

```
SACK 扩展格式（4 字节 = 32 位）：

覆盖范围: [ack_nr + 2, ack_nr + 33]

ack_nr = 10 时:
  位 0 = seq 12 (ack_nr + 2)
  位 1 = seq 13 (ack_nr + 3)
  位 2 = seq 14 (ack_nr + 4)
  ...

示例：收到 12, 13, 14 时
  SACK = 0b00000111 = 0x07

  Byte 0: 0x07 = 0b00000111
          位 0 (seq 12) = 1 ✓
          位 1 (seq 13) = 1 ✓
          位 2 (seq 14) = 1 ✓
```

### 代码处理流程

```erlang
%% update_skip_counts/2 中的处理逻辑
update_skip_counts(SAckedSeqs, #aiutp_pcb{outbuf = OutBuf, cur_window = CurWindow} = PCB) ->
    MaxSAckedSeq = lists:max(SAckedSeqs),
    Iter = aiutp_buffer:head(OutBuf),
    SAckSet = sets:from_list(SAckedSeqs),
    {SkippedCount, CurWindow1, OutBuf1} =
        update_skip_counts_loop(MaxSAckedSeq, SAckSet, Iter, 0, CurWindow, OutBuf),
    {SkippedCount, PCB#aiutp_pcb{outbuf = OutBuf1, cur_window = CurWindow1}}.

%% 循环检查每个未确认的包
update_skip_counts_loop(MaxSAckedSeq, SAckSet, Iter, SkippedCount, CurWindow, OutBuf) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    SeqNR = WrapPacket#aiutp_packet_wrap.packet#aiutp_packet.seq_nr,

    %% 判断是否应该增加跳过计数：
    %% 1. 已至少传输一次
    %% 2. 尚未标记为重发
    %% 3. seq_nr < 最大 SACK 序列号（表示被跳过）
    %% 4. 不在 SACK 集合中（未被确认）
    ShouldIncrement = (Transmissions > 0) andalso
                      (NeedResend == false) andalso
                      (?WRAPPING_DIFF_16(MaxSAckedSeq, SeqNR) > 0) andalso
                      (not sets:is_element(SeqNR, SAckSet)),

    case ShouldIncrement andalso (NewSkipCount >= ?DUPLICATE_ACKS_BEFORE_RESEND) of
        true ->
            %% 达到阈值，标记为快速重传
            WrapPacket1 = WrapPacket#aiutp_packet_wrap{
                skip_count = NewSkipCount,
                need_resend = true
            },
            %% 从 cur_window 中减去载荷大小
            ...
    end.
```

---

## 5. 场景 3：SACK 触发批量快速重传

### 场景描述
发送方发送了包 10-20，包 11, 13, 15 丢失。

### 发送缓冲区状态

```
发送前:
  outbuf: [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
                ↑       ↑       ↑
              丢失    丢失    丢失

收到 SACK {12, 14, 16, 17, 18, 19, 20} 后:
  包 11: skip_count = 1 (被 12 跳过)
  包 13: skip_count = 1 (被 14 跳过)
  包 15: skip_count = 1 (被 16 跳过)

再次收到类似 SACK:
  包 11: skip_count = 2
  包 13: skip_count = 2
  包 15: skip_count = 2

第三次收到 SACK:
  包 11: skip_count = 3 → need_resend = true
  包 13: skip_count = 3 → need_resend = true
  包 15: skip_count = 3 → need_resend = true
```

### selective_ack_packet 中的批量重传

```erlang
%% selective_ack_packet/3 处理 SACK 确认并触发快速重传
selective_ack_packet(SAckedPackets, MicroNow, PCB) ->
    %% 计算需要重传的序列号范围
    MinSeq = aiutp_util:bit16(SeqNR - CurWindowPackets),  % 最旧未确认包
    MaxSeq = aiutp_util:bit16(SAckedPacket.seq_nr - 1),   % SACK 前一个包

    %% 如果跨度超过阈值，触发快速重传
    if ?WRAPPING_DIFF_16(MaxSeq, MinSeq) > ?DUPLICATE_ACKS_BEFORE_RESEND ->
        %% 最多重传 4 个包
        {Sent, LastSeq, PCB1} = aiutp_net:send_n_packets(MinSeq, MaxSeq, 4, PCB0),

        %% 更新 fast_resend_seq_nr 防止重复重传
        PCB2 = PCB1#aiutp_pcb{
            fast_resend_seq_nr = aiutp_util:bit16(LastSeq + 1)
        },

        %% 如果有包被重传，触发窗口衰减
        if Sent > 0 -> maybe_decay_win(PCB2);
           true -> PCB2
        end
    end.
```

### 重传限制机制

```
fast_resend_seq_nr 的作用:

  初始: fast_resend_seq_nr = 10

  收到 SACK {15, 16, 17}，需要重传 11, 12, 13, 14:
    - 检查: 11 >= fast_resend_seq_nr? 是 → 可以重传
    - 重传后: fast_resend_seq_nr = 15

  再次收到相同 SACK:
    - 检查: 11 >= fast_resend_seq_nr? 否 (11 < 15)
    - 不重复重传
```

---

## 6. 场景 4：快速超时恢复（fast_timeout）

### 场景描述
RTO 超时后进入快速超时模式，逐个重传未确认的包。

### 处理流程

```erlang
%% process_ack_and_sack 中的 fast_timeout 处理
PCB5 =
    if FastTimeout ->
        %% 检查最旧未确认包是否已改变
        if ?WRAPPING_DIFF_16(PCB4#aiutp_pcb.seq_nr,
                             PCB4#aiutp_pcb.cur_window_packets) /= FastResendSeqNR0 ->
            %% 最旧的包已被确认，退出快速超时模式
            PCB4#aiutp_pcb{fast_timeout = false};
           true ->
            %% 继续重传下一个包
            aiutp_net:send_packet(
                aiutp_buffer:head(PCB4#aiutp_pcb.outbuf),
                PCB4#aiutp_pcb{fast_resend_seq_nr = aiutp_util:bit16(FastResendSeqNR0 + 1)})
        end;
       true -> PCB4
    end.
```

### 时序示例

```
发送方                                      接收方
   |                                           |
   |----[seq=10, DATA]---X (丢失)              |
   |----[seq=11, DATA]------------------------>|
   |----[seq=12, DATA]------------------------>|
   |                                           |
   | [RTO 超时，进入 fast_timeout 模式]        |
   | [fast_resend_seq_nr = 10]                 |
   |                                           |
   |----[seq=10, DATA, 重传]------------------>|
   |<---[ack=10, SACK={11,12}]-----------------|
   |                                           |
   | [收到 ACK，最旧未确认包仍是 10]           |
   | [fast_resend_seq_nr = 11]                 |
   |                                           |
   |----[seq=10, DATA, 再次重传]-------------->| 终于收到
   |<---[ack=12, ST_STATE]---------------------| 确认所有
   |                                           |
   | [最旧未确认包改变，退出 fast_timeout]     |
```

---

## 7. 窗口衰减机制

当通过 SACK 检测到丢包并重传时，需要降低拥塞窗口：

```erlang
%% maybe_decay_win/1 实现
maybe_decay_win(#aiutp_pcb{time = Now,
                           max_window = MaxWindow,
                           last_rwin_decay = LastRWinDecay} = PCB) ->
    %% 确保不会过于频繁地衰减
    if (Now - LastRWinDecay) < ?MAX_WINDOW_DECAY ->
        PCB;
       true ->
        %% 窗口减半，但不低于最小值
        MaxWindow0 = erlang:max(MaxWindow div 2, ?MIN_WINDOW_SIZE),
        PCB#aiutp_pcb{
            slow_start = false,
            ssthresh = MaxWindow0,
            max_window = MaxWindow0,
            last_rwin_decay = Now
        }
    end.
```

---

## 8. 与 libutp 实现的对比

| 特性 | libutp | aiutp |
|------|--------|-------|
| 重复 ACK 阈值 | 3 | 3 |
| 每次 SACK 最大重传数 | 4 | 4 |
| SACK 位图大小 | 4 字节 (32 位) | 4 字节 (32 位) |
| fast_resend_seq_nr | 有 | 有 |
| fast_resend_seq_nr 检查 | MinSeq = max(oldest, fast_resend_seq_nr) | MinSeq = max(OldestUnackedSeq, FastResendSeqNR) |
| 精确包选择 | 只重传被 SACK 跳过的包 | send_skipped_packets 只重传未被 SACK 确认的包 |
| 窗口衰减 | max_window * 0.5 | max_window / 2 |
| skip_count 机制 | 有 | 有 |

### 实现细节对齐

1. **防止重复重传**: `selective_ack_packet` 中 `MinSeq = max(OldestUnackedSeq, FastResendSeqNR)`
2. **精确包选择**: `send_skipped_packets` 使用 `SAckedSeqs` 集合过滤已被 SACK 确认的包
3. **重传条件**: 只重传 `transmissions > 0` 且不在 SACK 集合中的包

---

## 9. 注意事项

1. **不要重复重传**：使用 `fast_resend_seq_nr` 跟踪已重传的包
2. **只对已发送的包计数**：`transmissions > 0` 检查
3. **窗口管理**：重传时从 `cur_window` 减去载荷大小
4. **计数器重置**：成功确认后重置 `duplicate_ack` 和 `retransmit_count`
5. **Karn 算法**：重传的包不用于 RTT 计算

## 10. 参考资料

- [BEP-29: uTP Micro Transport Protocol](http://www.bittorrent.org/beps/bep_0029.html)
- [RFC 6817: LEDBAT Congestion Control](https://tools.ietf.org/html/rfc6817)
- [libutp 源码](https://github.com/bittorrent/libutp)
