# AIUTP 项目规划

## 项目概述

**aiutp** 是一个用 Erlang/OTP 实现的 uTP (uTorrent Transport Protocol) 协议库。uTP 是基于 UDP 的可靠传输协议，遵循 [BEP-29](http://www.bittorrent.org/beps/bep_0029.html) 规范，实现了 [LEDBAT](http://datatracker.ietf.org/wg/ledbat/charter/) 拥塞控制算法。

### 核心特性
- 基于 UDP 的可靠、有序数据传输
- LEDBAT 拥塞控制，最小化网络延迟影响
- 类似 TCP 的 API 接口（connect, listen, accept, send, recv, close）
- OTP 行为模式实现，支持 supervisor 监控
- 选择性确认（SACK）支持

## 项目架构

### 目录结构

```
aiutp/
├── src/                      # 源代码目录
│   ├── aiutp.erl            # 公共 API 入口模块
│   ├── aiutp_app.erl        # OTP Application 行为
│   ├── aiutp_sup.erl        # 顶层 Supervisor
│   ├── aiutp_socket.erl     # Socket 管理 (gen_server)
│   ├── aiutp_socket_sup.erl # Socket Supervisor
│   ├── aiutp_worker.erl     # 连接工作进程 (gen_server)
│   ├── aiutp_worker_sup.erl # Worker Supervisor
│   ├── aiutp_acceptor.erl   # 连接接受器
│   ├── aiutp_channel.erl    # 通道状态机 (gen_statem) [开发中]
│   ├── aiutp_pcb.erl        # 协议控制块 (PCB)
│   ├── aiutp_packet.erl     # 数据包编解码
│   ├── aiutp_net.erl        # 网络发送逻辑
│   ├── aiutp_tx.erl         # 发送队列管理
│   ├── aiutp_rx.erl         # 接收队列管理
│   ├── aiutp_buffer.erl     # 环形缓冲区
│   ├── aiutp_queue.erl      # 队列实现
│   ├── aiutp_delay.erl      # 延迟统计
│   ├── aiutp_rtt.erl        # RTT 计算
│   ├── aiutp_util.erl       # 工具函数
│   └── aiutp_test.erl       # 测试模块
├── include/
│   └── aiutp.hrl            # 头文件（常量、记录定义）
├── examples/                 # 参考实现
│   ├── libutp/              # C++ 官方实现 (BitTorrent)
│   ├── ethereum-utp/        # Rust 实现 (Ethereum Portal Network)
│   └── rs-proxy/            # Rust 代理示例
├── docs/                     # 文档目录
│   ├── requirements/        # 需求文档
│   │   ├── features/        # 功能设计
│   │   └── decisions/       # 决策记录
│   ├── development/         # 开发规范
│   ├── report/              # 开发报告
│   └── tasks/               # 完成的任务归档
├── CLAUDE.md                # Claude Code 指令
├── PLANNING.md              # 项目规划（本文件）
├── TASK.md                  # 任务追踪
└── README.md                # 项目说明
```

### OTP 监督树结构

```
aiutp_sup (one_for_all)
├── aiutp_socket_sup (simple_one_for_one)
│   └── aiutp_socket (gen_server) - 管理 UDP socket 和连接
└── aiutp_worker_sup (simple_one_for_one)
    └── aiutp_worker (gen_server) - 处理单个 uTP 连接
```

### 核心模块职责

| 模块 | 职责 | 类型 |
|------|------|------|
| `aiutp` | 公共 API 入口 | 接口模块 |
| `aiutp_socket` | UDP socket 管理，连接分发 | gen_server |
| `aiutp_worker` | 单个 uTP 连接的生命周期管理 | gen_server |
| `aiutp_acceptor` | 处理入站连接请求 | 工作进程 |
| `aiutp_channel` | 连接状态机（重构中） | gen_statem |
| `aiutp_pcb` | 协议控制块，核心协议逻辑 | 纯函数模块 |
| `aiutp_packet` | 数据包编解码 | 纯函数模块 |
| `aiutp_net` | 网络 I/O，数据包发送 | 纯函数模块 |
| `aiutp_tx` | 发送队列，ACK 处理 | 纯函数模块 |
| `aiutp_rx` | 接收队列，重排序 | 纯函数模块 |
| `aiutp_delay` | 延迟历史统计 | 纯函数模块 |
| `aiutp_rtt` | RTT/RTO 计算 | 纯函数模块 |

## uTP 协议实现

### 数据包格式 (BEP-29)

```
 0       4       8               16              24              32
+-------+-------+---------------+---------------+---------------+
| type  | ver   | extension     | connection_id                 |
+-------+-------+---------------+---------------+---------------+
| timestamp_microseconds                                        |
+---------------+---------------+---------------+---------------+
| timestamp_difference_microseconds                             |
+---------------+---------------+---------------+---------------+
| wnd_size                                                      |
+---------------+---------------+---------------+---------------+
| seq_nr                        | ack_nr                        |
+---------------+---------------+---------------+---------------+
```

### 数据包类型

| 类型 | 值 | 描述 |
|------|-----|------|
| ST_DATA | 0 | 数据包 |
| ST_FIN | 1 | 结束连接 |
| ST_STATE | 2 | 状态/ACK |
| ST_RESET | 3 | 重置连接 |
| ST_SYN | 4 | 同步/握手 |

### 连接状态机

```
CS_UNINITIALIZED → CS_IDLE → CS_SYN_SENT → CS_CONNECTED
                            ↓
                     CS_SYN_RECV → CS_CONNECTED → CS_CONNECTED_FULL
                                                 ↓
                                           CS_DESTROY / CS_RESET
```

### 核心算法

1. **LEDBAT 拥塞控制**
   - 目标延迟：300ms（可配置）
   - 基于延迟梯度调整窗口
   - 慢启动支持

2. **RTT 估算**
   - Karn 算法（仅使用首次传输计算 RTT）
   - 平滑 RTT 计算
   - RTO 计算：`RTO = RTT + 4 * RTTVar`

3. **选择性确认 (SACK)**
   - 支持最多 32 个包的位图
   - 减少不必要的重传

4. **快速重传**
   - 4 个重复 ACK 触发快速重传
   - 结合 SACK 优化重传策略

## 代码规范

### 命名约定

- 模块名：`aiutp_` 前缀 + 功能名
- 函数名：小写字母 + 下划线
- 记录名：`aiutp_` 前缀
- 常量宏：全大写 + 下划线

### 记录定义

主要记录类型位于 `include/aiutp.hrl`：

- `#aiutp_packet{}` - 数据包结构
- `#aiutp_packet_wrap{}` - 数据包包装（含传输信息）
- `#aiutp_pcb{}` - 协议控制块（连接状态）

### OTP 设计原则

1. 使用 gen_server/gen_statem 行为
2. 进程间使用消息传递
3. 通过 supervisor 实现容错
4. 避免共享状态

## 参考实现

### 1. libutp (C++)
- **来源**: BitTorrent 官方实现
- **许可**: MIT
- **特点**: 权威参考，稳定成熟
- **文件**: `examples/libutp/`

### 2. ethereum-utp (Rust)
- **来源**: Ethereum Portal Network
- **许可**: Apache 2.0
- **特点**: 现代异步实现，tokio 生态
- **文件**: `examples/ethereum-utp/`

### 3. wmproxy (Rust)
- **来源**: Rust 代理项目
- **特点**: 展示 uTP 在代理场景的应用
- **文件**: `examples/rs-proxy/`

## 版本信息

- **当前版本**: 0.1.0
- **Erlang/OTP 版本**: OTP 26+
- **许可证**: BSD 3-Clause

## 后续规划

1. 完成 `aiutp_channel` 的 gen_statem 重构
2. 添加完整的单元测试和属性测试
3. 性能优化（批量发送、零拷贝）
4. 文档完善（API 文档、使用示例）
5. 发布到 Hex.pm
