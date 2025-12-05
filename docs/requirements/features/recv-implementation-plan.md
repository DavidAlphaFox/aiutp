# recv 功能实现计划

> 版本: 1.0
> 日期: 2025-12-05
> 状态: ✅ 已完成

## 1. 概述

### 1.1 背景

当前 `aiutp:recv/2` 返回 `{error, not_implemented}`，只能通过 active 模式接收数据。为了提供完整的类 gen_tcp API，需要实现阻塞式 `recv` 功能。

### 1.2 目标

实现符合 gen_tcp 语义的 `recv` 函数：
- 支持阻塞等待数据
- 支持超时控制
- 支持长度控制（0 = 返回所有可用数据）
- 正确处理连接关闭

### 1.3 当前状态

```erlang
%% src/aiutp_channel.erl:524-526
connected({call, From}, {recv, _Len}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_implemented}}]};
```

底层 `aiutp_pcb:read/1` 已实现，可读取接收队列中的数据。

## 2. 设计方案

### 2.1 API 设计

```erlang
%% 基本 recv（无超时）
-spec recv(utp_connection(), non_neg_integer()) ->
    {ok, binary()} | {error, closed | timeout | term()}.

%% 带超时的 recv（新增）
-spec recv(utp_connection(), non_neg_integer(), timeout()) ->
    {ok, binary()} | {error, closed | timeout | term()}.
```

**参数说明**:
- `Len = 0`: 返回所有可用数据
- `Len > 0`: 返回恰好 `Len` 字节（当前简化为返回所有可用数据）
- `Timeout = infinity | Milliseconds`: 等待超时

### 2.2 状态机扩展

在 `state_data()` 中添加：

```erlang
-type state_data() :: #{
    %% 现有字段...
    recv_waiter => {gen_statem:from(), non_neg_integer(), reference() | undefined}
    %% {From, Len, TimerRef}
}.
```

### 2.3 核心逻辑

```
recv 调用
    │
    ▼
检查 inque 是否有数据
    │
    ├── 有数据 ──────► 直接返回 {ok, Data}
    │
    └── 无数据
         │
         ├── got_fin_reached = true ──► 返回 {error, closed}
         │
         └── 否则 ──► 保存 From，设置超时定时器，等待数据到达
                         │
                         ▼
                   数据到达或超时
                         │
                   ├── 数据到达 ──► 回复 {ok, Data}，清理定时器
                   │
                   └── 超时 ──► 回复 {error, timeout}，清理等待者
```

### 2.4 关键场景

| 场景 | 行为 |
|------|------|
| 有数据，无等待者 | 数据缓存在 inque |
| 有数据，有等待者 | 唤醒等待者，返回数据 |
| 无数据，recv 调用 | 阻塞等待 |
| recv 等待中，收到数据 | 唤醒等待者 |
| recv 等待中，超时 | 返回 `{error, timeout}` |
| recv 等待中，连接关闭 | 返回 `{error, closed}` |
| recv 等待中，对端 FIN | 如无数据返回 `{error, closed}` |

## 3. 实现步骤

### 阶段 1: 基础结构 (RECV-1)

| 任务 ID | 任务 | 描述 | 文件 |
|---------|------|------|------|
| RECV-1.1 | API 扩展 | 添加 `recv/3` 带超时版本 | `src/aiutp.erl` |
| RECV-1.2 | Channel API | 添加 `aiutp_channel:recv/3` | `src/aiutp_channel.erl` |
| RECV-1.3 | 状态字段 | 添加 `recv_waiter` 字段到 state_data | `src/aiutp_channel.erl` |

### 阶段 2: 核心逻辑 (RECV-2)

| 任务 ID | 任务 | 描述 | 文件 |
|---------|------|------|------|
| RECV-2.1 | 非阻塞读取 | 有数据时直接返回 | `src/aiutp_channel.erl` |
| RECV-2.2 | 阻塞等待 | 无数据时保存 From 并等待 | `src/aiutp_channel.erl` |
| RECV-2.3 | 数据到达唤醒 | 收到数据时检查并唤醒等待者 | `src/aiutp_channel.erl` |
| RECV-2.4 | 超时处理 | 处理 recv 超时 | `src/aiutp_channel.erl` |

### 阶段 3: 边缘情况 (RECV-3)

| 任务 ID | 任务 | 描述 | 文件 |
|---------|------|------|------|
| RECV-3.1 | EOF 处理 | got_fin_reached 时返回 closed | `src/aiutp_channel.erl` |
| RECV-3.2 | 连接关闭处理 | closing 状态下的 recv 处理 | `src/aiutp_channel.erl` |
| RECV-3.3 | 等待者清理 | 进程退出/关闭时清理等待者 | `src/aiutp_channel.erl` |

### 阶段 4: 测试 (RECV-4)

| 任务 ID | 任务 | 描述 | 文件 |
|---------|------|------|------|
| RECV-4.1 | 单元测试 | recv 基础功能测试 | `test/aiutp_channel_recv_tests.erl` |
| RECV-4.2 | 超时测试 | 超时场景测试 | `test/aiutp_channel_recv_tests.erl` |
| RECV-4.3 | EOF 测试 | 连接关闭场景测试 | `test/aiutp_channel_recv_tests.erl` |

## 4. 详细设计

### 4.1 connected 状态处理 recv

```erlang
%% 接收数据
connected({call, From}, {recv, Len, Timeout}, #{pcb := PCB} = Data) ->
    case aiutp_pcb:read(PCB) of
        {undefined, PCB1} ->
            %% 无数据，检查是否已关闭
            case aiutp_pcb:got_fin_reached(PCB1) of
                true ->
                    %% 对端已关闭且无数据
                    {keep_state, Data#{pcb := PCB1},
                     [{reply, From, {error, closed}}]};
                false ->
                    %% 设置等待者
                    TimerRef = case Timeout of
                        infinity -> undefined;
                        Ms -> erlang:start_timer(Ms, self(), recv_timeout)
                    end,
                    NewData = Data#{
                        pcb := PCB1,
                        recv_waiter => {From, Len, TimerRef}
                    },
                    {keep_state, NewData}
            end;
        {Payload, PCB1} ->
            %% 有数据，直接返回
            {keep_state, Data#{pcb := PCB1}, [{reply, From, {ok, Payload}}]}
    end;
```

### 4.2 数据到达时唤醒等待者

修改 `maybe_deliver_data/1`，检查并唤醒 `recv_waiter`：

```erlang
maybe_deliver_data(#{recv_waiter := {From, _Len, TimerRef}} = Data) ->
    %% 有 recv 等待者
    case try_read_for_waiter(Data) of
        {ok, Payload, Data1} ->
            cancel_timer(TimerRef),
            Data2 = maps:remove(recv_waiter, Data1),
            gen_statem:reply(From, {ok, Payload}),
            Data2;
        {closed, Data1} ->
            cancel_timer(TimerRef),
            Data2 = maps:remove(recv_waiter, Data1),
            gen_statem:reply(From, {error, closed}),
            Data2;
        {wait, Data1} ->
            Data1
    end;
maybe_deliver_data(#{active := true} = Data) ->
    %% 现有 active 模式逻辑...
```

### 4.3 超时处理

```erlang
connected(info, {timeout, TRef, recv_timeout},
          #{recv_waiter := {From, _Len, TRef}} = Data) ->
    NewData = maps:remove(recv_waiter, Data),
    {keep_state, NewData, [{reply, From, {error, timeout}}]};
```

### 4.4 关闭时清理等待者

在 `do_closing_cleanup/2` 中添加：

```erlang
%% 回复 recv 等待者
case maps:get(recv_waiter, Data, undefined) of
    undefined -> ok;
    {RecvFrom, _Len, RecvTimer} ->
        cancel_timer(RecvTimer),
        gen_statem:reply(RecvFrom, {error, closed})
end,
```

## 5. 与 active 模式的交互

### 5.1 互斥规则

- `active = true` 时，recv 返回 `{error, active}`
- recv 等待中，设置 `active = true` 应取消等待并返回错误
- 保持简单：**不允许同时使用 active 模式和 recv**

### 5.2 实现

```erlang
connected({call, From}, {recv, _Len, _Timeout}, #{active := true}) ->
    {keep_state_and_data, [{reply, From, {error, active}}]};
```

## 6. 测试计划

### 6.1 单元测试用例

| 测试 | 描述 |
|------|------|
| recv_immediate_data | 有数据时立即返回 |
| recv_wait_data | 无数据时阻塞，数据到达后返回 |
| recv_timeout | 超时返回 {error, timeout} |
| recv_closed | 对端关闭返回 {error, closed} |
| recv_active_error | active 模式下返回 {error, active} |
| recv_multiple_calls | 多次 recv 调用（第二次返回 {error, busy}） |

### 6.2 集成测试用例

| 测试 | 描述 |
|------|------|
| echo_recv | 客户端 send，服务端 recv 回显 |
| large_data_recv | 大数据分片接收 |
| close_while_recv | recv 等待中连接关闭 |

## 7. 风险和缓解

| 风险 | 影响 | 缓解措施 |
|------|------|----------|
| recv 和 active 模式冲突 | 中 | 明确互斥，返回错误 |
| 多个并发 recv | 中 | 只允许一个等待者 |
| 等待者未清理 | 高 | 所有退出路径都清理 |
| 死锁 | 高 | 使用 gen_statem:reply 异步回复 |

## 8. 简化决策

为保持实现简单，第一版做以下简化：

1. **Len 参数暂不实现精确读取**: `Len > 0` 时仍返回所有可用数据
2. **单等待者**: 只允许一个进程等待 recv
3. **与 active 互斥**: active=true 时 recv 返回错误

未来可扩展：
- 精确长度读取
- 多等待者队列
- {active, once} 语义增强

## 9. 验收标准

- [x] `recv(Conn, 0)` 有数据时立即返回
- [x] `recv(Conn, 0)` 无数据时阻塞
- [x] `recv(Conn, 0, 1000)` 超时返回 `{error, timeout}`
- [x] 对端关闭后 recv 返回 `{error, closed}`
- [x] active=true 时 recv 返回 `{error, active}`
- [x] 所有测试通过 (238 个测试)
- [x] 现有测试不受影响
