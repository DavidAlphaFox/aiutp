# PCB Packet Processing 重构设计文档

> 日期: 2025-12-03
> 状态: 已完成
> 完成日期: 2025-12-03

## 1. 问题分析

### 1.1 当前 process 函数链问题

当前 `aiutp_pcb.erl` 中的 packet 处理使用了难以理解的命名:

```
process/2
  └── process_by_type/3
        └── process_packet/2
              └── process_packet_1/2
                    └── process_packet_2/2
                          └── process_packet_3/2
                                └── process_packet_4/2
```

**问题:**
1. 函数名不表达其职责
2. 处理流程不清晰
3. 难以单独测试每个阶段

### 1.2 连接断开处理不完整

**BEP-29 要求的优雅关闭流程:**

```
端A                              端B
 │  close() 调用                   │
 │──────── ST_FIN(seq=X) ────────>│  A 发送 FIN
 │                                 │  B 记录 eof_pkt = X
 │<──────── ST_STATE(ack=X) ──────│  B 确认 FIN
 │  fin_sent_acked = true          │
 │                                 │  close() 调用
 │<──────── ST_FIN(seq=Y) ────────│  B 发送 FIN
 │  got_fin = true                 │
 │  eof_pkt = Y                    │
 │──────── ST_STATE(ack=Y) ───────>│  A 确认 FIN
 │                                 │  fin_sent_acked = true
 │  等待 ack_nr 达到 eof_pkt       │
 │  got_fin_reached = true         │
 │  state = CS_DESTROY             │  state = CS_DESTROY
```

**当前实现问题:**

| 方面 | BEP-29 要求 | 当前实现 | 问题 |
|------|-------------|----------|------|
| FIN 接收 | 记录 eof_pkt, 继续接收直到所有数据到达 | 部分实现 | 正确 |
| FIN 发送 | 发送后等待 ACK | 部分实现 | 正确 |
| 超时发送 RESET | 超时后应发送 RESET 通知对端 | 只改状态 | **缺失** |
| Keep-alive 超时 | 超时后发送 RESET | 只改状态 | **缺失** |
| 状态转换 | 明确的 FIN_WAIT 状态 | 隐含在标志中 | 不清晰 |

### 1.3 SACK 处理问题

**BEP-29 SACK 规范:**

```
SACK Extension:
+--------+--------+--------+--------+
|  0x01  |  len   |   bitmask...    |
+--------+--------+--------+--------+

- bitmask 每个 bit 代表 ack_nr + 2 + bit_index 位置的包
- 如果一个包被后续 3+ 个包 SACK 确认，应触发快速重传
```

**当前实现问题:**

| 方面 | BEP-29 要求 | 当前实现 | 问题 |
|------|-------------|----------|------|
| SACK 解析 | 正确解析 bitmask | ✅ 实现 | - |
| SACK 确认 | 从 outbuf 移除 SACK'd 包 | ✅ 实现 | - |
| 基于 SACK 快速重传 | 被跳过 3+ 次的包应重传 | ❌ 未实现 | **缺失** |
| 重复 ACK 阈值 | 3 个 | 4 个 | 偏保守 |
| SACK 触发重传 | SACK 表明丢包时立即重传 | 只用于拥塞控制 | **不完整** |

---

## 2. 重构设计

### 2.1 函数重命名方案

| 原函数名 | 新函数名 | 职责 |
|----------|----------|------|
| `process/2` | `process_incoming/2` | 入口点，调度 ACK |
| `process_by_type/3` | `dispatch_by_type/3` | 按包类型分发处理 |
| `process_packet/2` | `validate_and_init/2` | 验证序列号范围，初始化处理 |
| `process_packet_1/2` | `handle_duplicate_acks/2` | 检测重复 ACK，触发快速重传 |
| `process_packet_2/2` | `process_ack_and_sack/2` | 处理 ACK/SACK，拥塞控制 |
| `process_packet_3/2` | `update_connection_state/2` | 更新连接状态 |
| `process_packet_4/2` | `handle_data_and_fin/2` | 处理数据和 FIN |

### 2.2 新增处理函数

```erlang
%% SACK 触发的快速重传
-spec handle_sack_retransmit([#aiutp_packet_wrap{}], #aiutp_pcb{}) -> #aiutp_pcb{}.

%% 发送 RESET 包 (用于连接异常终止)
-spec send_reset(#aiutp_pcb{}) -> #aiutp_pcb{}.

%% 判断是否需要发送 keep-alive
-spec maybe_send_keepalive(#aiutp_pcb{}) -> #aiutp_pcb{}.
```

### 2.3 连接断开处理改进

#### 2.3.1 超时时发送 RESET

```erlang
%% aiutp_pcb_timeout.erl 修改
check_timeouts_1(...) when RTT > ?RTO_MAX ->
    %% 发送 RESET 通知对端
    PCB0 = send_reset(PCB),
    if CloseRequested -> {false, PCB0#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB0#aiutp_pcb{state = ?CS_RESET}}
    end;
```

#### 2.3.2 Keep-alive 超时发送 RESET

```erlang
check_timeouts_1(...) when (Now - LastGotPacket) > ?KEEPALIVE_INTERVAL * 2 ->
    %% 发送 RESET 通知对端连接已死
    PCB0 = send_reset(PCB),
    ...
```

### 2.4 SACK 处理改进

#### 2.4.1 基于 SACK 的快速重传

当检测到 SACK 跳过某个包 3 次或更多时，应该立即重传该包:

```erlang
%% 新增: 检测 SACK 跳过的包并标记重传
-spec detect_sack_gaps([non_neg_integer()], #aiutp_pcb{}) -> #aiutp_pcb{}.
detect_sack_gaps(SAckedSeqs, #aiutp_pcb{outbuf = OutBuf} = PCB) ->
    %% 遍历 outbuf，检查哪些包被 SACK 跳过了 3 次以上
    ...
```

#### 2.4.2 SACK 处理流程优化

```
收到包含 SACK 的 ACK
    │
    ├── 1. 解析 SACK bitmask -> SAckedSeqs
    │
    ├── 2. 从 outbuf 移除已 SACK 确认的包
    │
    ├── 3. 检测被跳过的包 (gap detection)
    │       │
    │       └── 对于每个未确认的包:
    │             如果 seq < min(SAckedSeqs) 且未标记重传
    │             → 增加 skip_count
    │             如果 skip_count >= 3
    │             → 标记 need_resend = true
    │
    └── 4. 触发重传标记的包
```

---

## 3. 处理流程图

### 3.1 新的 Packet 处理流程

```
process_incoming/2
    │
    └── dispatch_by_type/3
            │
            ├── [ST_RESET] → 直接处理，设置状态
            │
            ├── [ST_SYN] → 处理握手
            │
            └── [其他] → validate_and_init/2
                            │
                            └── handle_duplicate_acks/2
                                    │
                                    └── process_ack_and_sack/2
                                            │
                                            ├── 处理普通 ACK
                                            ├── 处理 SACK
                                            ├── 拥塞控制
                                            │
                                            └── update_connection_state/2
                                                    │
                                                    └── handle_data_and_fin/2
                                                            │
                                                            ├── [ST_FIN] → 记录 eof_pkt
                                                            │
                                                            └── [ST_DATA] → aiutp_rx:in
```

### 3.2 连接关闭流程

```
close/1 调用
    │
    ├── [IDLE/UNINIT] → state = DESTROY
    │
    ├── [SYN_SENT/SYN_RECV] → state = DESTROY
    │
    └── [CONNECTED] → fin_sent = true
                       发送 ST_FIN
                       等待 fin_sent_acked

收到对端 FIN
    │
    └── got_fin = true
        eof_pkt = seq_nr
        继续处理直到 ack_nr == eof_pkt
        got_fin_reached = true
        发送最终 ACK

超时/异常
    │
    └── 发送 ST_RESET
        state = DESTROY/RESET
```

---

## 4. 实现计划

### 阶段 1: 函数重命名 (不改变逻辑) ✅ 已完成

1. ✅ 重命名所有 process_packet_N 函数
   - `process/2` → `process_incoming/2` (保留兼容别名)
   - `process_by_type/3` → `dispatch_by_type/3`
   - `process_packet/2` → `validate_and_init/2`
   - `process_packet_1/2` → `handle_duplicate_acks/2`
   - `process_packet_2/2` → `process_ack_and_sack/2`
   - `process_packet_3/2` → `update_connection_state/2`
   - `process_packet_4/2` → `handle_data_and_fin/2`
2. ✅ 添加详细的 edoc 文档
3. ✅ 更新 aiutp_channel.erl 使用新函数名
4. ✅ 验证所有测试通过

### 阶段 2: 连接断开处理改进 ✅ 已完成

1. ✅ 在 aiutp_net.erl 添加 `send_reset/1` 函数
2. ✅ 修改 aiutp_pcb_timeout.erl 超时处理，添加 RESET 发送
   - Keepalive 超时时发送 RESET
   - RTT 超时时发送 RESET
   - 重传次数超限时发送 RESET
3. ✅ 添加相关测试用例 (aiutp_net_tests.erl)

### 阶段 3: SACK 处理改进 ✅ 已完成

1. ✅ 添加 `skip_count` 字段到 `aiutp_packet_wrap` 记录
2. ✅ 实现 `aiutp_tx:update_skip_counts/2` 函数
   - 检测被 SACK 跳过的包
   - 跳过 3 次后标记 need_resend
3. ✅ 集成到 `process_ack_and_sack/2` 处理流程
4. ✅ 添加相关测试用例 (aiutp_tx_tests.erl)

### 阶段 4: 测试和验证 ✅ 已完成

1. ✅ 单元测试全部通过 (146 个测试)
2. ✅ 更新文档
3. 代码审查 (待进行)

---

## 5. 测试用例设计

### 5.1 函数重命名测试

```erlang
%% 确保处理流程不变
process_syn_test() -> ...
process_data_test() -> ...
process_fin_test() -> ...
process_reset_test() -> ...
```

### 5.2 连接断开测试

```erlang
%% 超时发送 RESET
timeout_sends_reset_test() -> ...

%% 优雅关闭流程
graceful_close_test() -> ...

%% FIN 后继续接收数据
fin_before_data_test() -> ...
```

### 5.3 SACK 测试

```erlang
%% SACK gap 检测
sack_gap_detection_test() -> ...

%% 基于 SACK 的快速重传
sack_fast_retransmit_test() -> ...

%% SACK bitmask 解析
sack_bitmask_parsing_test() -> ...
```

---

## 6. 风险评估

| 风险 | 影响 | 缓解措施 |
|------|------|----------|
| 重命名导致遗漏 | 编译失败 | 逐步重命名，每步验证 |
| RESET 发送时机不当 | 误断开连接 | 添加保守的条件判断 |
| SACK 改动影响性能 | 延迟增加 | 性能测试 |
| 向后兼容性 | API 变化 | 保持公共 API 不变 |

---

## 附录 A: 当前代码结构

```
aiutp_pcb.erl (585 行)
├── new/3              - 创建 PCB
├── state/1            - 获取状态
├── closed/1           - 检查关闭状态
├── process/2          - 处理入包 (入口)
│   ├── process_by_type/3
│   ├── process_packet/2
│   ├── process_packet_1/2
│   ├── process_packet_2/2
│   ├── process_packet_3/2
│   └── process_packet_4/2
├── check_timeouts/1   - 超时检查
├── write/2            - 写数据
├── close/1            - 关闭连接
├── read/1             - 读数据
├── connect/2          - 发起连接
├── accept/2           - 接受连接
└── flush/1            - 刷新队列
```

## 附录 B: BEP-29 相关章节

- **Section 3.1**: Connection Setup (SYN 握手)
- **Section 3.2**: Data Transfer (数据传输)
- **Section 3.3**: Connection Teardown (连接关闭)
- **Section 4.1**: Selective ACK (SACK 扩展)
- **Section 5.1**: Retransmission (重传机制)
