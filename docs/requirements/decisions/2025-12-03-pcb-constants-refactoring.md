# PCB 和常量重构决策记录

> 日期: 2025-12-03
> 状态: 已完成

## 背景

根据 BEP-29 uTP 协议规范，对项目中的常量定义和 PCB (Protocol Control Block) 记录进行标准化重构，以提高代码可读性、可维护性，并修复已知的拼写错误。

## 决策内容

### 1. 常量分组标准化

将 `include/aiutp.hrl` 中的常量按 BEP-29 标准分为 13 个 Section：

| Section | 内容 | 说明 |
|---------|------|------|
| 1 | 协议版本 | `UTP_VERSION = 1` |
| 2 | 包类型 | ST_DATA, ST_FIN, ST_STATE, ST_RESET, ST_SYN |
| 3 | 扩展类型 | EXT_NONE, EXT_SACK, EXT_EXT_BITS |
| 4 | 连接状态 | CS_UNINITIALIZED, CS_IDLE, CS_SYN_SENT, 等 |
| 5 | 超时参数 | RTO_MIN, RTO_MAX, RTO_INITIAL, KEEPALIVE_INTERVAL |
| 6 | 窗口和缓冲区 | PACKET_SIZE, MIN_WINDOW_SIZE, BUFFER_MAX_SIZE |
| 7 | 拥塞控制 | TARGET_DELAY, MAX_CWND_INCREASE_BYTES_PER_RTT |
| 8 | 重传参数 | DUPLICATE_ACKS_BEFORE_RESEND, ACK_NR_ALLOWED_WINDOW |
| 9 | 位掩码 | SEQ_NR_MASK, ACK_NR_MASK, TIMESTAMP_MASK |
| 10 | 工具宏 | WRAPPING_DIFF_16, WRAPPING_DIFF_32 |
| 11 | Packet 记录 | aiutp_packet |
| 12 | PacketWrap 记录 | aiutp_packet_wrap |
| 13 | PCB 记录 | aiutp_pcb |

### 2. 新增 BEP-29 标准常量

```erlang
%% 协议版本
-define(UTP_VERSION, 1).

%% 扩展类型
-define(EXT_NONE, 0).
-define(EXT_SACK, 1).
-define(EXT_EXT_BITS, 2).

%% 超时参数 (BEP-29 标准)
-define(RTO_MIN, 500).           %% 最小 RTO
-define(RTO_MAX, 6000).          %% 最大 RTO
-define(RTO_INITIAL, 1000).      %% 初始 RTO
-define(RTT_VAR_INITIAL, 800).   %% 初始 RTT 方差

%% 拥塞控制 (BEP-29 标准)
-define(TARGET_DELAY, 100000).   %% 目标延迟 100ms (微秒)
-define(MIN_WINDOW_SIZE_BEP29, 150). %% BEP-29 标准最小窗口

%% 重传参数
-define(DUPLICATE_ACKS_BEFORE_RESEND_BEP29, 3). %% BEP-29 标准
-define(DUPLICATE_ACKS_BEFORE_RESEND, 4).       %% 实际使用 (更保守)
```

### 3. PCB 记录字段分组

将 PCB 的 40+ 个字段按功能分为 12 个组：

1. **连接标识** - conn_id_recv, conn_id_send, socket
2. **连接状态** - state, close_requested, read_shutdown
3. **序列号管理** - seq_nr, ack_nr, timeout_seq_nr, fast_resend_seq_nr
4. **FIN 处理** - got_fin, got_fin_reached, fin_sent, fin_sent_acked, eof_pkt
5. **窗口管理** - cur_window, cur_window_packets, max_window, max_window_user, last_rcv_win
6. **RTT/RTO 管理** - rtt, rtt_var, rto, rtt_hist, retransmit_timeout, rto_timeout
7. **拥塞控制** - target_delay, ssthresh, slow_start, last_maxed_out_window
8. **延迟统计** - our_hist, their_hist, average_delay, current_delay_sum, clock_drift
9. **重传管理** - retransmit_count, duplicate_ack, fast_timeout, reorder_count
10. **时间戳** - time, recv_time, last_got_packet, last_sent_packet, reply_micro
11. **缓冲区** - inbuf, outbuf, inque, outque
12. **特殊模式** - ida, burst

### 4. 修复的拼写错误

| 位置 | 原值 | 修正值 |
|------|------|--------|
| aiutp.hrl PCB 记录 | `brust = true` | `burst = true` |
| aiutp.hrl PCB 记录 | `ida = fasle` | `ida = false` |
| aiutp_pcb.erl:505 | `{fasle,PCB#...}` | `{false,PCB#...}` |
| aiutp_pcb.erl 多处 | `brust` | `burst` |
| aiutp_net.erl 多处 | `brust`, `Brust` | `burst`, `Burst` |
| aiutp_net.erl | `BRUST_OUTGOING_BUFFER_SIZE` | `BURST_OUTGOING_BUFFER_SIZE` |
| aiutp_packet.erl | 本地定义 `EXT_SACK`, `EXT_BITS` | 使用头文件中的 `EXT_SACK`, `EXT_EXT_BITS` |

## 受影响的文件

- `include/aiutp.hrl` - 完全重写
- `src/aiutp_pcb.erl` - 修复拼写错误
- `src/aiutp_net.erl` - 修复拼写错误
- `src/aiutp_packet.erl` - 使用统一的扩展类型宏

## 验证

- 编译通过 ✅
- 所有 100 个单元测试通过 ✅

## 参考资料

- [BEP-29 uTP Specification](https://www.bittorrent.org/beps/bep_0029.html)
- [RFC 6817 LEDBAT](https://datatracker.ietf.org/doc/html/rfc6817)
- [docs/report/bep29-analysis-2025-12-03.md](../../../docs/report/bep29-analysis-2025-12-03.md)
