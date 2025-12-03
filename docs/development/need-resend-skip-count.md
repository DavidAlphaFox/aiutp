# need_resend 和 skip_count 字段详解

本文档详细说明 `aiutp_packet_wrap` 记录中 `need_resend` 和 `skip_count` 字段的用途和应用位置。

## 1. 字段定义

```erlang
-record(aiutp_packet_wrap, {
    packet :: #aiutp_packet{},
    payload = 0 :: integer(),
    transmissions = 0 :: integer(),
    need_resend = false :: boolean(),    %% 是否需要重传
    skip_count = 0 :: integer(),         %% 被 SACK 跳过的次数
    time_sent = 0 :: integer()
}).
```

---

## 2. need_resend 字段

### 2.1 作用

`need_resend` 是一个布尔标志，表示该包需要被重传。当设置为 `true` 时，下次调用 `flush_packets` 会重新发送该包。

### 2.2 设置位置

#### (1) SACK 触发快速重传 - `aiutp_tx.erl:update_skip_counts_loop/6`

当 `skip_count >= 3` 时设置：

```erlang
%% aiutp_tx.erl 中的逻辑
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

#### (2) RTO 超时 - `aiutp_pcb_timeout.erl:mark_need_resend/4`

当重传超时发生时，遍历所有未确认的包：

```erlang
%% aiutp_pcb_timeout.erl 中的逻辑
mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf) ->
    ...
    if (NeedResend == true) orelse (Transmissions == 0) ->
        %% 跳过已标记或未发送的包
        mark_need_resend(CurWindowPackets - 1, CurWindow, Next, OutBuf);
       true ->
        %% 标记为需要重发，并从 cur_window 中扣除载荷大小
        WrapPacket0 = WrapPacket#aiutp_packet_wrap{need_resend = true},
        OutBuf0 = aiutp_buffer:replace(Iter, WrapPacket0, OutBuf),
        mark_need_resend(CurWindowPackets - 1, CurWindow - Payload, Next, OutBuf0)
    end.
```

### 2.3 读取位置

#### (1) 发送决策 - `aiutp_net.erl:367`

在 `send_data_packet/3` 中检查是否应该发送包：

```erlang
%% aiutp_net.erl 中的逻辑
send_data_packet(Iter, BytesToSend, PCB) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    #aiutp_packet_wrap{need_resend = NeedResend, ...} = WrapPacket,

    if NeedResend == true ->
        %% 需要重传，直接发送
        do_send_packet(...)
    end.
```

#### (2) 发送后重置 - `aiutp_net.erl:682`

包发送后重置标志：

```erlang
%% 发送后更新包状态
WrapPacket1 = WrapPacket#aiutp_packet_wrap{
    need_resend = false,
    time_sent = Now,
    transmissions = Transmissions + 1
},
```

#### (3) 窗口计算 - `aiutp_pcb_cc.erl:170`

在 `ack_packet/3` 中用于正确计算 `cur_window`：

```erlang
%% 只有当 need_resend == false 时才从 cur_window 扣除
%% 因为标记为 need_resend = true 的包已经在标记时扣除过了
if NeedResend == false ->
    PCB#aiutp_pcb{cur_window = CurWindow - Payload};
   true ->
    PCB
end.
```

#### (4) 避免重复标记 - `aiutp_pcb_timeout.erl:104`

```erlang
%% 如果已经是 need_resend = true，跳过
if (NeedResend == true) orelse (Transmissions == 0) ->
    mark_need_resend(CurWindowPackets - 1, CurWindow, Next, OutBuf);
```

---

## 3. skip_count 字段

### 3.1 作用

`skip_count` 记录包被 SACK（选择性确认）跳过的次数。当收到的 SACK 确认了序列号更大的包，但没有确认该包时，说明该包可能丢失，`skip_count` 递增。

### 3.2 设置位置

#### `aiutp_tx.erl:update_skip_counts_loop/6`

```erlang
update_skip_counts_loop(MaxSAckedSeq, SAckSet, Iter, SkippedCount, CurWindow, OutBuf) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    SeqNR = WrapPacket#aiutp_packet_wrap.packet#aiutp_packet.seq_nr,

    %% 判断是否应该增加跳过计数：
    %% 1. 已至少传输一次 (Transmissions > 0)
    %% 2. 尚未标记为重发 (NeedResend == false)
    %% 3. seq_nr < 最大 SACK 序列号（表示被跳过）
    %% 4. 不在 SACK 集合中（未被确认）
    ShouldIncrement = (Transmissions > 0) andalso
                      (NeedResend == false) andalso
                      (?WRAPPING_DIFF_16(MaxSAckedSeq, SeqNR) > 0) andalso
                      (not sets:is_element(SeqNR, SAckSet)),

    NewSkipCount = if ShouldIncrement -> SkipCount + 1; true -> SkipCount end,
    ...
```

### 3.3 读取位置

#### (1) 触发快速重传 - `aiutp_tx.erl`

当 `skip_count >= DUPLICATE_ACKS_BEFORE_RESEND (3)` 时触发：

```erlang
case ShouldIncrement andalso (NewSkipCount >= ?DUPLICATE_ACKS_BEFORE_RESEND) of
    true ->
        %% 达到阈值，设置 need_resend = true
        WrapPacket1 = WrapPacket#aiutp_packet_wrap{
            skip_count = NewSkipCount,
            need_resend = true
        },
        ...
end.
```

#### (2) 统计跳过包数量 - `aiutp_pcb.erl:handle_fast_retransmit/3`

用于决定是否需要进行快速重传处理。

---

## 4. 工作流程

### 4.1 SACK 触发快速重传流程

```
收到包含 SACK 的 ACK
    │
    ▼
调用 update_skip_counts/2
    │
    ▼
遍历 outbuf 中的未确认包
    │
    ├─── 包在 SACK 中？
    │         │
    │         └─→ 已确认，处理 ACK
    │
    └─── 包不在 SACK 中但 seq < max_sack_seq？
              │
              ▼
         skip_count += 1
              │
              ▼
         skip_count >= 3？
              │
         ┌────┴────┐
         │ 是      │ 否
         ▼         ▼
    need_resend = true    继续等待
         │
         ▼
    从 cur_window 扣除载荷
         │
         ▼
    下次 flush_packets 时重传
```

### 4.2 RTO 超时重传流程

```
RTO 超时触发
    │
    ▼
调用 mark_need_resend/4
    │
    ▼
遍历所有未确认的包
    │
    ├─── need_resend == true？ ──→ 跳过（已标记）
    │
    ├─── transmissions == 0？ ──→ 跳过（未发送）
    │
    └─── 其他情况
              │
              ▼
         need_resend = true
              │
              ▼
         从 cur_window 扣除载荷
              │
              ▼
         继续下一个包
    │
    ▼
立即发送第一个需要重传的包
```

---

## 5. 关键点总结

| 字段 | 类型 | 作用 | 设置时机 | 检查时机 |
|------|------|------|----------|----------|
| `skip_count` | integer | 记录被 SACK 跳过的次数 | 每次收到 SACK | 达到阈值时触发 `need_resend` |
| `need_resend` | boolean | 标记包需要重传 | 1) skip_count>=3<br>2) RTO超时 | 发送包时检查 |

### 5.1 为什么需要两个字段？

1. **`skip_count`** 实现了 **快速重传** 机制
   - 基于 SACK 信息检测丢包
   - 无需等待 RTO 超时
   - 更精确地识别丢失的包

2. **`need_resend`** 是 **统一的重传标志**
   - 无论是快速重传还是 RTO 超时，都通过这个标志触发
   - 发送逻辑只需检查这一个标志

### 5.2 窗口管理

当包被标记为 `need_resend = true` 时，其载荷大小会从 `cur_window` 中扣除。这是因为：

- 该包将被重传，不应该占用当前窗口
- 避免在 ACK 时重复扣除

### 5.3 与 libutp 的对应关系

| aiutp | libutp | 说明 |
|-------|--------|------|
| `skip_count` | `OutgoingPacket::skip_count` | 相同语义 |
| `need_resend` | `OutgoingPacket::need_resend` | 相同语义 |
| `DUPLICATE_ACKS_BEFORE_RESEND (3)` | `DUPLICATE_ACKS_BEFORE_RESEND (3)` | 相同阈值 |

---

## 6. 相关文件

- `src/aiutp_tx.erl` - SACK 处理和 skip_count 更新
- `src/aiutp_pcb_timeout.erl` - RTO 超时和 need_resend 标记
- `src/aiutp_net.erl` - 包发送和 need_resend 检查
- `src/aiutp_pcb_cc.erl` - 窗口计算
- `docs/development/fast-retransmit-scenarios.md` - 快速重传场景详解

---

## 7. 参考资料

- [BEP-29: uTP Micro Transport Protocol](http://www.bittorrent.org/beps/bep_0029.html)
- [libutp 源码](https://github.com/bittorrent/libutp)
- [RFC 2018: TCP Selective Acknowledgment Options](https://tools.ietf.org/html/rfc2018)
