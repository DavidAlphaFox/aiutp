# MTU 发现开发计划

> 版本: 1.0
> 日期: 2025-12-03
> 状态: 草案

## 1. 开发阶段概览

```
阶段 1: 基础设施        阶段 2: 核心逻辑        阶段 3: 集成测试
─────────────────────  ─────────────────────  ─────────────────────
[常量定义]             [aiutp_mtu 模块]       [单元测试]
[PCB 字段扩展]         [探测包发送]           [集成测试]
[配置接口]             [ACK/超时处理]         [性能验证]
                       [周期性重新探测]        [文档更新]
```

## 2. 详细任务分解

### 阶段 1: 基础设施准备

#### 任务 1.1: 常量定义
**文件**: `include/aiutp.hrl`

```erlang
%% 添加 MTU 相关常量
-define(MTU_FLOOR_DEFAULT, 528).
-define(MTU_CEILING_DEFAULT, 1452).
-define(MTU_SEARCH_THRESHOLD, 16).
-define(MTU_PROBE_INTERVAL, 30 * 60 * 1000000).
-define(MTU_PROBE_FAILURE_THRESHOLD, 3).
-define(MTU_PROBE_BACKOFF_MULTIPLIER, 2).
```

**验收条件**:
- [ ] 常量定义完成
- [ ] 编译无警告
- [ ] 注释说明每个常量用途

---

#### 任务 1.2: PCB 记录扩展
**文件**: `include/aiutp.hrl`

```erlang
%% 在 aiutp_pcb 记录中添加 MTU 字段
-record(aiutp_pcb, {
    %% ... 现有字段 ...

    %% MTU Discovery
    mtu_floor = ?MTU_FLOOR_DEFAULT,
    mtu_ceiling = ?MTU_CEILING_DEFAULT,
    mtu_last = ?PACKET_SIZE,
    mtu_probe_seq = 0,
    mtu_probe_size = 0,
    mtu_discover_time = 0,
    mtu_probe_failures = 0
}).
```

**验收条件**:
- [ ] 字段添加完成
- [ ] 默认值正确
- [ ] 现有测试通过

---

#### 任务 1.3: packet_wrap 记录扩展
**文件**: `include/aiutp.hrl`

```erlang
%% 在 aiutp_packet_wrap 记录中添加探测标记
-record(aiutp_packet_wrap, {
    %% ... 现有字段 ...
    is_mtu_probe = false :: boolean()
}).
```

**验收条件**:
- [ ] 字段添加完成
- [ ] 现有测试通过

---

#### 任务 1.4: 配置接口
**文件**: `src/aiutp_app.erl`, `src/aiutp.erl`

**新增函数**:
- `aiutp:get_mtu_config/0` - 获取 MTU 配置
- `aiutp:set_mtu_config/1` - 设置 MTU 配置

**验收条件**:
- [ ] 配置读取功能
- [ ] 配置验证逻辑
- [ ] 默认配置生效

---

### 阶段 2: 核心逻辑实现

#### 任务 2.1: aiutp_mtu 模块骨架
**文件**: `src/aiutp_mtu.erl` (新建)

```erlang
-module(aiutp_mtu).
-export([reset/1, search_update/1, should_probe/2,
         on_probe_acked/2, on_probe_timeout/1, on_probe_lost/1,
         packet_size/1, is_probing/1]).
```

**子任务**:
- [ ] 2.1.1 创建模块文件
- [ ] 2.1.2 实现 reset/1
- [ ] 2.1.3 实现 search_update/1
- [ ] 2.1.4 实现 should_probe/2
- [ ] 2.1.5 实现 on_probe_acked/2
- [ ] 2.1.6 实现 on_probe_timeout/1
- [ ] 2.1.7 实现 on_probe_lost/1
- [ ] 2.1.8 实现 packet_size/1
- [ ] 2.1.9 实现 is_probing/1

**验收条件**:
- [ ] 所有函数实现完成
- [ ] 单元测试覆盖
- [ ] Dialyzer 类型检查通过

---

#### 任务 2.2: 探测包发送集成
**文件**: `src/aiutp_net.erl`

**修改点**:
1. `send_data_packet/3` - 检查是否作为探测包
2. `normalize_bytes/1` - 使用动态 packet_size

```erlang
%% 修改 send_data_packet
send_data_packet(Iter, BytesToSend, PCB) ->
    MaxPayload = aiutp_mtu:packet_size(PCB),
    %% ...
    IsMtuProbe = aiutp_mtu:should_probe(ActualPayload, PCB),
    %% ...
```

**验收条件**:
- [ ] 探测包正确标记
- [ ] 探测状态正确更新
- [ ] 现有发送逻辑不受影响

---

#### 任务 2.3: ACK 处理集成
**文件**: `src/aiutp_pcb_cc.erl`

**修改点**:
1. `ack_packet/3` - 处理探测包 ACK

```erlang
%% 修改 ack_packet
ack_packet(WrapPacket, AckNR, PCB) ->
    PCB1 = case WrapPacket#aiutp_packet_wrap.is_mtu_probe of
        true -> aiutp_mtu:on_probe_acked(SeqNR, PCB);
        false -> PCB
    end,
    %% ...
```

**验收条件**:
- [ ] 探测成功正确提高 floor
- [ ] 非探测包 ACK 不受影响
- [ ] 搜索继续进行

---

#### 任务 2.4: 超时处理集成
**文件**: `src/aiutp_pcb_timeout.erl`

**修改点**:
1. `check_timeouts/1` - 检测探测包超时
2. 新增 `is_only_probe_in_flight/1`

```erlang
%% 修改超时处理
check_timeouts(PCB) ->
    case is_only_probe_in_flight(PCB) of
        true ->
            PCB1 = aiutp_mtu:on_probe_timeout(PCB),
            resend_probe_packet(PCB1);
        false ->
            handle_normal_timeout(PCB)
    end.
```

**验收条件**:
- [ ] 探测超时不触发拥塞控制
- [ ] ceiling 正确降低
- [ ] 立即重发探测包

---

#### 任务 2.5: 丢包检测集成
**文件**: `src/aiutp_tx.erl`

**修改点**:
1. `update_skip_counts_loop/6` - 检测探测包丢失

```erlang
%% 修改 SACK 处理
update_skip_counts_loop(...) ->
    PCB1 = case WrapPacket#aiutp_packet_wrap.is_mtu_probe andalso
                NewSkipCount >= ?DUPLICATE_ACKS_BEFORE_RESEND of
        true -> aiutp_mtu:on_probe_lost(PCB);
        false -> PCB
    end,
    %% ...
```

**验收条件**:
- [ ] 重复 ACK 正确检测探测包丢失
- [ ] ceiling 正确降低
- [ ] 搜索继续进行

---

#### 任务 2.6: 周期性重新探测
**文件**: `src/aiutp_pcb_timeout.erl`, `src/aiutp_net.erl`

**修改点**:
1. 在发送时检查重新探测时间
2. 定时器触发重新探测

```erlang
%% 在发送时检查
maybe_restart_mtu_discovery(PCB) ->
    Now = aiutp_util:microseconds(),
    case Now >= PCB#aiutp_pcb.mtu_discover_time andalso
         PCB#aiutp_pcb.mtu_floor < PCB#aiutp_pcb.mtu_ceiling of
        true -> aiutp_mtu:reset(PCB);
        false -> PCB
    end.
```

**验收条件**:
- [ ] 30 分钟后自动重新探测
- [ ] 探测期间保持当前 MTU
- [ ] 重新探测不影响数据传输

---

#### 任务 2.7: PCB 初始化集成
**文件**: `src/aiutp_pcb.erl`

**修改点**:
1. `new/3` - 初始化 MTU 状态
2. `connect/3`, `accept/2` - 启动 MTU 发现

```erlang
%% 修改 new/3
new(Socket, Remote, Options) ->
    PCB0 = #aiutp_pcb{...},
    PCB1 = case proplists:get_value(mtu_discovery, Options, true) of
        true -> aiutp_mtu:reset(PCB0);
        false -> PCB0
    end,
    PCB1.
```

**验收条件**:
- [ ] 连接建立时自动启动 MTU 发现
- [ ] 可通过选项禁用
- [ ] 默认启用

---

### 阶段 3: 测试和文档

#### 任务 3.1: 单元测试
**文件**: `test/aiutp_mtu_tests.erl` (新建)

**测试用例**:
- [ ] 3.1.1 reset_test - 重置状态测试
- [ ] 3.1.2 search_update_test - 二分查找测试
- [ ] 3.1.3 should_probe_test - 探测条件测试
- [ ] 3.1.4 probe_ack_test - ACK 处理测试
- [ ] 3.1.5 probe_timeout_test - 超时处理测试
- [ ] 3.1.6 probe_lost_test - 丢包处理测试
- [ ] 3.1.7 convergence_test - 收敛性测试
- [ ] 3.1.8 failure_fallback_test - 失败回退测试

---

#### 任务 3.2: 集成测试
**文件**: `test/aiutp_mtu_integration_tests.erl` (新建)

**测试场景**:
- [ ] 3.2.1 标准以太网 MTU 发现
- [ ] 3.2.2 小 MTU 环境测试
- [ ] 3.2.3 探测失败回退测试
- [ ] 3.2.4 周期性重新探测测试
- [ ] 3.2.5 禁用 MTU 发现测试

---

#### 任务 3.3: 性能验证
**内容**:
- [ ] 探测开销测量（< 1%）
- [ ] 收敛时间验证（< 30 秒）
- [ ] 吞吐量对比（MTU 发现 vs 固定）

---

#### 任务 3.4: 文档更新
**文件**:
- [ ] README.md - 添加 MTU 发现功能说明
- [ ] API 文档 - 新增 API 说明
- [ ] CHANGELOG.md - 记录变更

---

## 3. 任务依赖关系

```
1.1 常量定义 ───────┐
                    │
1.2 PCB 扩展 ───────┼───► 2.1 aiutp_mtu 模块 ─┬─► 2.2 发送集成
                    │                         │
1.3 wrap 扩展 ──────┘                         ├─► 2.3 ACK 集成
                                              │
1.4 配置接口 ─────────────────────────────────┼─► 2.4 超时集成
                                              │
                                              ├─► 2.5 丢包集成
                                              │
                                              └─► 2.6 周期重新探测
                                                        │
                                              2.7 初始化集成
                                                        │
                                                        ▼
                                              3.1 单元测试
                                                        │
                                              3.2 集成测试
                                                        │
                                              3.3 性能验证
                                                        │
                                              3.4 文档更新
```

## 4. 里程碑

### M1: 基础设施完成
- 完成任务: 1.1, 1.2, 1.3, 1.4
- 交付物: 扩展后的头文件，配置接口
- 验收: 编译通过，现有测试通过

### M2: 核心功能完成
- 完成任务: 2.1 - 2.7
- 交付物: aiutp_mtu 模块，集成修改
- 验收: 基本功能可用

### M3: 测试完成
- 完成任务: 3.1, 3.2, 3.3
- 交付物: 测试套件，性能报告
- 验收: 所有测试通过，性能达标

### M4: 发布就绪
- 完成任务: 3.4
- 交付物: 完整文档
- 验收: 文档审核通过

## 5. 风险和缓解

| 风险 | 概率 | 影响 | 缓解措施 |
|------|------|------|----------|
| 与现有代码冲突 | 中 | 中 | 逐步集成，频繁测试 |
| 性能影响 | 低 | 中 | 探测频率限制 |
| 边界条件遗漏 | 中 | 低 | 充分测试 |

## 6. 实现顺序建议

```
第 1 周期: 基础设施 (任务 1.1 - 1.4)
           ↓
第 2 周期: 核心模块 (任务 2.1)
           ↓
第 3 周期: 发送集成 (任务 2.2, 2.7)
           ↓
第 4 周期: ACK/超时集成 (任务 2.3, 2.4, 2.5)
           ↓
第 5 周期: 周期探测 (任务 2.6)
           ↓
第 6 周期: 测试 (任务 3.1, 3.2)
           ↓
第 7 周期: 验证和文档 (任务 3.3, 3.4)
```

## 7. 检查清单

### 代码质量
- [ ] 所有函数有类型规范 (-spec)
- [ ] 关键逻辑有注释说明
- [ ] 遵循项目编码规范
- [ ] Dialyzer 无警告

### 测试覆盖
- [ ] 单元测试覆盖率 > 90%
- [ ] 集成测试覆盖主要场景
- [ ] 边界条件测试

### 文档完整
- [ ] 模块文档 (edoc)
- [ ] 功能说明文档
- [ ] 配置说明

## 8. 相关文档

- [需求文档](./mtu-discovery.md)
- [详细设计](./mtu-discovery-design.md)
- [编码规范](../../development/coding-standards.md)
