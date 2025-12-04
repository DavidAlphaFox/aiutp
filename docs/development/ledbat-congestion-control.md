# LEDBAT 拥塞控制

> 本文档解释 LEDBAT 拥塞控制算法的工作原理，以及它如何限制发送速率。

## 概述

**LEDBAT** (Low Extra Delay Background Transport) 是一种延迟敏感的拥塞控制算法，设计目标是：

> **"只使用网络的剩余容量，不与其他流量竞争"**

这意味着它**故意限制**发送速率，以保持低延迟。

## 核心公式

```
窗口调整 = (TARGET - 当前延迟) / TARGET × 增益

TARGET = 100ms (目标延迟)

当前延迟 < 100ms → 窗口增大（网络空闲）
当前延迟 > 100ms → 窗口减小（网络拥塞）
当前延迟 = 100ms → 窗口不变（达到平衡）
```

## 与 TCP 的对比

| 特性 | TCP (CUBIC/Reno) | LEDBAT |
|------|------------------|--------|
| 目标 | 最大化吞吐量 | 最小化额外延迟 |
| 策略 | 持续增加窗口直到丢包 | 当延迟超过 100ms 就减速 |
| 结果 | 占满带宽，延迟增加 | 主动让出带宽，保持低延迟 |
| 适用场景 | 前台传输、文件下载 | 后台传输、P2P |

## 行为示例

### 场景：100 Mbps 链路，RTT = 20ms

**TCP 行为**：
```
- 持续增加窗口
- 填满路由器缓冲区
- 延迟增加到 200-500ms
- 最终吞吐量: ~95 Mbps
```

**LEDBAT 行为**：
```
- 检测到延迟 > 100ms
- 主动减小窗口
- 延迟保持在 ~100ms
- 最终吞吐量: ~30-50 Mbps (取决于缓冲区大小)
```

## aiutp 实现

### 窗口调整代码

```erlang
%% aiutp_pcb_cc.erl - LEDBAT 窗口调整

%% 1. 计算延迟偏差
OffTarget = ?TARGET - OurDelay,  %% TARGET = 100ms (100000 微秒)

%% 2. 计算窗口因子和延迟因子
WindowFactor = min(AckedBytes, MaxWindow) / max(AckedBytes, MaxWindow),
DelayFactor = clamp(OffTarget, -?TARGET, ?TARGET) / ?TARGET,

%% 3. 计算缩放增益
ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT  %% 3000 字节/RTT
           * WindowFactor
           * DelayFactor,

%% 4. 关键点:
%%    - 当 OurDelay > TARGET 时，OffTarget < 0
%%    - 导致 DelayFactor < 0
%%    - 进而 ScaledGain < 0
%%    - 窗口减小！

%% 5. 窗口更新
NewMaxWindow = max(?MIN_WINDOW_SIZE, MaxWindow + ScaledGain)
```

### 常量定义

```erlang
%% include/aiutp.hrl

-define(TARGET, 100000).                        %% 目标延迟 100ms (微秒)
-define(MAX_CWND_INCREASE_BYTES_PER_RTT, 3000). %% 每 RTT 最大增长 3000 字节
-define(MIN_WINDOW_SIZE, 3888).                 %% 最小窗口 3 个包
```

## 发送速率计算

发送速率由窗口大小和 RTT 决定：

```
发送速率 = max_window / RTT
```

### 示例

```
max_window = 64 KB (LEDBAT 限制后的窗口)
RTT = 50ms

发送速率 = 64 KB / 0.05s
         = 1.28 MB/s
         ≈ 10 Mbps

即使网络带宽是 100 Mbps，LEDBAT 也只用 10 Mbps
因为再快就会导致延迟超过 100ms
```

## 对性能优化的影响

### 为什么批量发送优化收益有限

```
LEDBAT 限制下的实际情况:

┌────────────────────────────────────────┐
│                                        │
│  max_window = 64 KB                    │
│  PACKET_SIZE = 1296 bytes              │
│  最大在途包数 = 64K / 1296 ≈ 50 包     │
│                                        │
│  RTT = 50ms                            │
│  每秒最多发送 = 50 包 / 0.05s          │
│              = 1000 包/秒              │
│                                        │
│  系统调用开销 = 1000 × 5μs = 5ms       │
│  占 CPU 时间 = 0.5%                    │
│                                        │
│  结论: 系统调用不是瓶颈！              │
└────────────────────────────────────────┘
```

### 瓶颈分析

| 优化方向 | 理论收益 | 实际收益 | 原因 |
|----------|----------|----------|------|
| 批量发送 | 减少系统调用 | 有限 | 发送速率被 LEDBAT 限制 |
| 零拷贝 | 减少内存拷贝 | 有限 | 包速率本身不高 |
| 更快的定时器 | 更及时的检测 | 有限 | 500ms 节流已足够 |

## 设计哲学

```
LEDBAT 的哲学:

"我是后台传输协议，不应该影响用户体验。
 如果网络拥塞，我主动让步。
 宁可传输慢一点，也不能让视频卡顿。"

这就是为什么 BitTorrent 选择 uTP/LEDBAT:
- 用户可以边下载边看视频
- 不会导致网页加载变慢
- 对家庭网络友好
```

## 窗口动态变化

```
网络空闲时:
  延迟 = 20ms < TARGET (100ms)
  → 窗口增大
  → 发送更多数据
  → 延迟逐渐增加

网络拥塞时:
  延迟 = 150ms > TARGET (100ms)
  → 窗口减小
  → 发送更少数据
  → 延迟逐渐降低

平衡状态:
  延迟 ≈ TARGET (100ms)
  → 窗口稳定
  → 发送速率稳定
```

## 与其他流量共存

```
┌─────────────────────────────────────────────────┐
│                   网络带宽                       │
│  ████████████████████████████████████████████   │
│  │← TCP 流量 (占用大部分) →│←LEDBAT(剩余)→│    │
│                                                 │
│  LEDBAT 主动让出带宽给 TCP                      │
│  确保前台应用（网页、视频）不受影响              │
└─────────────────────────────────────────────────┘
```

## 相关文档

- [RFC 6817: LEDBAT Congestion Control](https://datatracker.ietf.org/doc/html/rfc6817)
- [BEP-29: uTorrent Transport Protocol](https://www.bittorrent.org/beps/bep_0029.html)
- [数据发送流程](./data-sending-flow.md)
- [发送优化分析](../report/tx-optimization-analysis-2025-12-04.md)

## 总结

| 要点 | 说明 |
|------|------|
| **TARGET = 100ms** | LEDBAT 的延迟目标，超过就减速 |
| **窗口限制** | 延迟高时窗口被压缩 |
| **发送速率 = 窗口/RTT** | 窗口小 → 速率低 |
| **让出带宽** | 这是 LEDBAT 的设计目标，不是缺陷 |
| **优化收益有限** | 系统调用等优化被 LEDBAT 限制抵消 |
