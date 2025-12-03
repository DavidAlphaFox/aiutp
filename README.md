# aiutp

Erlang/OTP 实现的 uTP (uTorrent Transport Protocol) 协议库。

## 概述

aiutp 是一个基于 UDP 的可靠传输协议实现，遵循 [BEP-29](http://www.bittorrent.org/beps/bep_0029.html) 规范。uTP 使用 [LEDBAT](http://datatracker.ietf.org/wg/ledbat/charter/) 拥塞控制算法，能在保证可靠传输的同时最小化对网络延迟的影响。

## 特性

- 基于 UDP 的可靠、有序数据传输
- LEDBAT 拥塞控制
- 类似 TCP 的 API (connect, listen, accept, send, recv, close)
- OTP 行为模式实现
- 选择性确认 (SACK) 支持

## 安装

### Rebar3

```erlang
{deps, [
    {aiutp, {git, "https://github.com/your-username/aiutp.git", {branch, "master"}}}
]}.
```

## 快速开始

### 服务端

```erlang
% 打开 socket
{ok, Socket} = aiutp:open(9000),

% 开始监听
ok = aiutp:listen(Socket),

% 接受连接
{ok, Conn} = aiutp:accept(Socket),

% 接收数据
{ok, Data} = aiutp:recv(Conn, 0),

% 关闭连接
aiutp:close(Conn).
```

### 客户端

```erlang
% 打开 socket
{ok, Socket} = aiutp:open(0),

% 连接到服务端
{ok, Conn} = aiutp:connect(Socket, "127.0.0.1", 9000),

% 发送数据
ok = aiutp:send(Conn, <<"Hello, uTP!">>),

% 关闭连接
aiutp:close(Conn).
```

## 文档

- [项目规划](./PLANNING.md) - 架构设计和技术细节
- [任务追踪](./TASK.md) - 开发进度和待办事项
- [参考实现](./examples/README.md) - 其他语言的 uTP 实现

## API

### 主要函数

| 函数 | 描述 |
|------|------|
| `aiutp:open/1,2` | 打开 uTP socket |
| `aiutp:connect/3` | 连接到远程地址 |
| `aiutp:listen/1,2` | 开始监听连接 |
| `aiutp:accept/1` | 接受入站连接 |
| `aiutp:send/2` | 发送数据 |
| `aiutp:recv/2` | 接收数据 |
| `aiutp:close/1` | 关闭连接 |
| `aiutp:active/2` | 设置 active 模式 |
| `aiutp:controlling_process/2` | 转移控制进程 |

## 开发

### 构建

```bash
rebar3 compile
```

### 测试

```bash
rebar3 eunit
```

### Dialyzer

```bash
rebar3 dialyzer
```

## 许可证

BSD 3-Clause License
