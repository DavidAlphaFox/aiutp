# aiutp

Erlang/OTP 实现的 uTP (uTorrent Transport Protocol) 协议库。

[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26%2B-blue)](https://www.erlang.org/)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-green)](LICENSE)

## 概述

aiutp 是一个基于 UDP 的可靠传输协议实现，遵循 [BEP-29](http://www.bittorrent.org/beps/bep_0029.html) 规范。uTP 使用 [LEDBAT](http://datatracker.ietf.org/wg/ledbat/charter/) 拥塞控制算法，能在保证可靠传输的同时最小化对网络延迟的影响。

### 主要特性

- ✅ 基于 UDP 的可靠、有序数据传输
- ✅ LEDBAT 拥塞控制算法
- ✅ 类似 TCP 的 API (connect, listen, accept, send, recv, close)
- ✅ OTP 行为模式实现 (gen_server, supervisor)
- ✅ 选择性确认 (SACK) 支持
- ✅ 快速重传机制
- ✅ RTT/RTO 自适应计算
- ✅ Active/Passive 模式数据接收
- ✅ 路径 MTU 发现 (PMTUD)

## 安装

### 前置条件

- Erlang/OTP 26 或更高版本
- rebar3 构建工具

### 作为依赖添加

在 `rebar.config` 中添加：

```erlang
{deps, [
    {aiutp, {git, "https://github.com/your-username/aiutp.git", {branch, "master"}}}
]}.
```

### 从源码构建

```bash
git clone https://github.com/your-username/aiutp.git
cd aiutp
rebar3 compile
```

## 快速开始

### 服务端示例

```erlang
%% 启动 aiutp 应用
ok = application:ensure_all_started(aiutp),

%% 打开 socket 并绑定端口
{ok, Socket} = aiutp:open(9000),

%% 开始监听
ok = aiutp:listen(Socket),

%% 接受连接（阻塞）
{ok, Conn} = aiutp:accept(Socket),

%% 接收数据（0 表示接收任意长度）
{ok, Data} = aiutp:recv(Conn, 0),
io:format("Received: ~p~n", [Data]),

%% 发送响应
ok = aiutp:send(Conn, <<"Hello from server!">>),

%% 关闭连接
aiutp:close(Conn).
```

### 客户端示例

```erlang
%% 启动 aiutp 应用
ok = application:ensure_all_started(aiutp),

%% 打开 socket（端口 0 表示系统自动分配）
{ok, Socket} = aiutp:open(0),

%% 连接到服务端
{ok, Conn} = aiutp:connect(Socket, "127.0.0.1", 9000),

%% 发送数据
ok = aiutp:send(Conn, <<"Hello, uTP!">>),

%% 接收响应
{ok, Response} = aiutp:recv(Conn, 0),
io:format("Response: ~p~n", [Response]),

%% 关闭连接
aiutp:close(Conn).
```

### Active 模式示例

Active 模式下，数据以消息形式发送到控制进程：

```erlang
%% 接受连接后设置 active 模式
{ok, Conn} = aiutp:accept(Socket),
ok = aiutp:active(Conn, true),

%% 接收数据消息
receive
    {utp_data, Conn, Data} ->
        io:format("Received: ~p~n", [Data]);
    {utp_closed, Conn, Reason} ->
        io:format("Connection closed: ~p~n", [Reason])
end.
```

## API 参考

### Socket 操作

#### `aiutp:open/1,2`

打开一个 uTP socket。

```erlang
-spec open(Port :: non_neg_integer()) -> {ok, Socket} | {error, Reason}.
-spec open(Port :: non_neg_integer(), Options :: list()) -> {ok, Socket} | {error, Reason}.
```

- `Port` - 绑定端口，0 表示系统自动分配
- `Options` - 配置选项（当前保留）

**返回值:**
- `{ok, {utp, SocketPid}}` - 成功
- `{error, eaddrinuse}` - 端口已被占用
- `{error, Reason}` - 其他错误

#### `aiutp:listen/1,2`

开始监听入站连接。

```erlang
-spec listen(Socket) -> ok | {error, Reason}.
-spec listen(Socket, Options) -> ok | {error, Reason}.
```

#### `aiutp:accept/1`

接受入站连接（阻塞调用）。

```erlang
-spec accept(Socket) -> {ok, Connection} | {error, Reason}.
```

**返回值:**
- `{ok, {utp, SocketPid, WorkerPid}}` - 成功建立连接
- `{error, Reason}` - 接受失败

#### `aiutp:connect/3`

连接到远程地址。

```erlang
-spec connect(Socket, Address, Port) -> {ok, Connection} | {error, Reason}.
```

- `Address` - 目标地址（IP 地址字符串或元组）
- `Port` - 目标端口

**返回值:**
- `{ok, Connection}` - 连接成功
- `{error, timeout}` - 连接超时
- `{error, econnrefused}` - 连接被拒绝

### 数据传输

#### `aiutp:send/2`

发送数据到已连接的对端。

```erlang
-spec send(Connection, Data :: iodata()) -> ok | {error, Reason}.
```

**返回值:**
- `ok` - 数据已入队发送
- `{error, closed}` - 连接已关闭
- `{error, enotconn}` - 未连接

#### `aiutp:recv/2`

从连接接收数据（阻塞调用）。

```erlang
-spec recv(Connection, Length :: non_neg_integer()) -> {ok, binary()} | {error, Reason}.
```

- `Length` - 期望接收的字节数，0 表示接收任意长度

### 连接控制

#### `aiutp:active/2`

设置连接的 active 模式。

```erlang
-spec active(Connection, Mode :: boolean()) -> ok.
```

- `true` - 数据以消息形式发送：`{utp_data, Conn, Data}`
- `false` - 需要显式调用 `recv/2` 接收

#### `aiutp:controlling_process/2`

转移连接的控制进程。

```erlang
-spec controlling_process(Connection, NewOwner :: pid()) -> ok | {error, Reason}.
```

#### `aiutp:close/1`

关闭连接。

```erlang
-spec close(Connection) -> ok.
```

## Active 模式消息

当 active 模式开启时，控制进程会收到以下消息：

| 消息 | 描述 |
|------|------|
| `{utp_data, Connection, Data}` | 收到数据 |
| `{utp_closed, Connection, Reason}` | 连接关闭 |

## 架构概览

### OTP 监督树

```
aiutp_sup (rest_for_one)
├── aiutp_socket_sup (simple_one_for_one)
│   └── aiutp_socket (gen_server) - 管理 UDP socket 和连接
└── aiutp_channel_sup (simple_one_for_one)
    └── aiutp_channel (gen_statem) - 连接状态机
```

### 核心模块

| 模块 | 职责 |
|------|------|
| `aiutp` | 公共 API 入口 |
| `aiutp_socket` | UDP socket 管理，连接分发 |
| `aiutp_channel` | 连接状态机 (gen_statem) |
| `aiutp_pcb` | 协议控制块，核心状态管理 |
| `aiutp_pcb_cc` | LEDBAT 拥塞控制算法 |
| `aiutp_pcb_timeout` | 超时检测和重传处理 |
| `aiutp_mtu` | 路径 MTU 发现 |
| `aiutp_packet` | 数据包编解码 |
| `aiutp_net` | 网络发送逻辑 |

## 协议参数

aiutp 遵循 BEP-29 规范，关键参数如下：

| 参数 | 值 | 描述 |
|------|-----|------|
| `RTO_MIN` | 1000ms | 最小重传超时 |
| `RTO_MAX` | 6000ms | 最大重传超时 |
| `RTO_INITIAL` | 1000ms | 初始重传超时 |
| `KEEPALIVE_INTERVAL` | 29s | 保活间隔 |
| `TARGET_DELAY` | 100ms | LEDBAT 目标延迟 |
| `PACKET_SIZE` | 1296 bytes | 默认最大包大小 |
| `MTU_FLOOR` | 528 bytes | MTU 发现下限 |
| `MTU_CEILING` | 1452 bytes | MTU 发现上限 |
| `MTU_PROBE_INTERVAL` | 30 min | MTU 重新探测间隔 |

## 开发

### 构建

```bash
rebar3 compile
```

### 运行测试

```bash
# 运行所有测试
rebar3 eunit

# 运行特定测试
rebar3 eunit --module=aiutp_packet_tests
```

### 类型检查

```bash
rebar3 dialyzer
```

### 测试覆盖率

```bash
rebar3 eunit --cover
rebar3 cover --verbose
```

## 文档

- [项目规划](./PLANNING.md) - 架构设计和技术细节
- [任务追踪](./TASK.md) - 开发进度和待办事项
- [BEP-29 分析](./docs/report/bep29-analysis-2025-12-03.md) - 协议分析报告
- [代码分析](./docs/report/code-analysis-2025-12-03.md) - 代码质量分析

## 版本历史

### v0.1.0 (已完成)

- ✅ BEP-29 协议核心实现
- ✅ OTP 监督树结构
- ✅ 基础 API (connect, listen, accept, send, recv, close)
- ✅ LEDBAT 拥塞控制
- ✅ 选择性确认 (SACK)
- ✅ 单元测试套件 (109 个测试用例)
- ✅ 文档完善

### v0.2.0 (进行中)

- ✅ gen_statem 重构 (aiutp_channel)
- ✅ 路径 MTU 发现 (PMTUD)
- ✅ 单元测试套件 (220 个测试用例)
- 🔄 完整测试覆盖
- 🔄 性能优化
- 🔄 API 稳定化

### 计划中

- Hex.pm 发布
- 完整 edoc 文档
- 示例应用

## 许可证

BSD 3-Clause License

## 贡献

欢迎提交 Issue 和 Pull Request。

## 致谢

- [BEP-29](http://www.bittorrent.org/beps/bep_0029.html) - uTP 协议规范
- [libutp](https://github.com/bittorrent/libutp) - BitTorrent 官方 C++ 实现
- [LEDBAT](http://datatracker.ietf.org/wg/ledbat/charter/) - 低延迟拥塞控制算法
