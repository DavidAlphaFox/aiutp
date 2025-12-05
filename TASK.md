# AIUTP 任务追踪

> 最后更新: 2025-12-05

## 当前任务

### 进行中

| 任务 | 描述 | 开始日期 | 状态 |
|------|------|----------|------|
| (无) | - | - | - |

### 最近完成 (2025-12-05)

- ✅ **recv 功能实现** - 实现阻塞式 `aiutp:recv/2,3` 功能
  - 添加 `recv/3` 带超时版本到 `src/aiutp.erl`
  - 添加 `aiutp_channel:recv/3` 实现
  - 添加 `recv_waiter` 状态字段用于阻塞等待
  - 实现非阻塞读取（有数据立即返回）
  - 实现阻塞等待（无数据时保存 From 并等待）
  - 实现数据到达唤醒等待者
  - 实现超时处理（支持 infinity 和毫秒超时）
  - 实现 EOF 处理（got_fin_reached 时返回 closed）
  - 实现连接关闭时清理等待者
  - 与 active 模式互斥（active=true 时返回 `{error, active}`）
  - 单等待者策略（并发 recv 返回 `{error, busy}`）
  - 创建集成测试 `test/aiutp_channel_recv_tests.erl`（7 个测试用例）
  - 详细设计文档：[docs/requirements/features/recv-implementation-plan.md](./docs/requirements/features/recv-implementation-plan.md)

### 待处理

| 任务 | 描述 | 优先级 |
|------|------|--------|
| Hex 发布 | 准备发布到 Hex.pm | 低 |
| 删除测试文件 | 删除 src/aiutp_test.erl | 低 |

---

## 状态机边缘情况修复任务分解 ✅

> 详细分析报告: [docs/report/state-machine-edge-case-analysis-2025-12-05.md](./docs/report/state-machine-edge-case-analysis-2025-12-05.md)

### 🔴 高优先级（潜在资源泄漏或连接卡死） ✅

| 任务 ID | 任务 | 描述 | 文件 | 状态 |
|---------|------|------|------|------|
| SM-1.1 | FIN 等待超时值错误 | 将 `60` 改为 `60000`（60ms → 60s） | `src/aiutp_rx.erl:136` | ✅ 完成 |
| SM-1.2 | FIN 数据等待超时检测 | 添加 `got_fin=true` 但 `got_fin_reached=false` 时的超时检测 | `src/aiutp_pcb_timeout.erl` | ✅ 完成 |
| SM-1.3 | CS_SYN_RECV 重复 SYN 处理 | 处理 seq_nr 不匹配的 SYN 包 | `src/aiutp_pcb.erl:185` | ✅ 完成 |

### 🟡 中优先级（边缘情况处理不一致） ✅

| 任务 ID | 任务 | 描述 | 文件 | 状态 |
|---------|------|------|------|------|
| SM-2.1 | 半关闭状态通知 | 发送 `utp_passive_close` 消息通知应用层 | `src/aiutp_channel.erl` | ✅ 完成 |
| SM-2.2 | RESET 包验证 | 验证 ack_nr 在合理范围内（防止 RST 攻击） | `src/aiutp_pcb.erl:166` | ✅ 完成 |

### 🟢 低优先级（极端边缘情况） ✅

| 任务 ID | 任务 | 描述 | 文件 | 状态 |
|---------|------|------|------|------|
| SM-3.1 | CS_SYN_SENT 收到 ST_DATA | 允许 ST_DATA 触发 SYN_SENT → CONNECTED 状态转换 | `src/aiutp_pcb.erl:474` | ✅ 完成 |
| SM-3.2 | MTU 探测失败日志 | 添加连续探测失败时的警告日志 | `src/aiutp_mtu.erl:125` | ✅ 完成 |

### 测试任务 ✅

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| SM-T1 | FIN + 对端崩溃测试 | 测试收到 FIN 后对端不再发送数据的超时检测 | ✅ 完成 |
| SM-T2 | SYN_RECV 重复 SYN 测试 | 测试 seq_nr 不匹配的重复 SYN 处理 | ✅ 完成 |
| SM-T3 | RESET 包验证测试 | 测试 conn_id 和 ack_nr 验证 | ✅ 完成 |

---

## MTU 发现功能任务分解 ✅

> 详细设计文档: [docs/requirements/features/mtu-discovery.md](./docs/requirements/features/mtu-discovery.md)

### 阶段 1: 基础设施准备 ✅

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| MTU-1.1 | 常量定义 | 在 aiutp.hrl 添加 MTU 相关常量 | ✅ 完成 |
| MTU-1.2 | PCB 记录扩展 | 添加 mtu_floor, mtu_ceiling 等字段 | ✅ 完成 |
| MTU-1.3 | packet_wrap 扩展 | 添加 is_mtu_probe 标记 | ✅ 完成 |
| MTU-1.4 | 配置接口 | 添加 MTU 配置读取/设置函数 | ⏭️ 暂不需要 |

### 阶段 2: 核心逻辑实现 ✅

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| MTU-2.1 | aiutp_mtu 模块 | 创建新模块实现核心 MTU 逻辑 | ✅ 完成 |
| MTU-2.2 | 探测包发送集成 | 修改 aiutp_net.erl 支持探测包 | ✅ 完成 |
| MTU-2.3 | ACK 处理集成 | 修改 aiutp_pcb.erl 处理探测 ACK | ✅ 完成 |
| MTU-2.4 | 超时处理集成 | 修改 aiutp_pcb_timeout.erl 处理探测超时 | ✅ 完成 |
| MTU-2.5 | 丢包检测集成 | 修改 aiutp_tx.erl 检测探测包丢失 | ✅ 完成 |
| MTU-2.6 | 周期性重新探测 | 实现 30 分钟周期重新探测 | ✅ 完成 |
| MTU-2.7 | PCB 初始化集成 | 修改 aiutp_pcb.erl 初始化 MTU 状态 | ✅ 完成 |

### 阶段 3: 测试和文档 ✅

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| MTU-3.1 | 单元测试 | 创建 aiutp_mtu_tests.erl (29 个测试) | ✅ 完成 |
| MTU-3.2 | 集成测试 | 创建 MTU 集成测试场景 (14 个测试) | ✅ 完成 |
| MTU-3.3 | 性能验证 | 验证探测开销和收敛时间 (5 个测试) | ✅ 完成 |
| MTU-3.4 | 文档更新 | 更新 README 和 API 文档 | ✅ 完成 |

## 已完成任务

> 详细的已完成任务记录请参见 [TASK_DONE.md](./TASK_DONE.md)

### 最近完成 (2025-12-04)

- ✅ **修复 accepting 状态拒绝所有 API 调用** - aiutp_channel.erl:469
  - 根因：accepting 状态的 catch-all 子句拒绝所有调用，包括 active、controlling_process
  - 修复：在 accepting 状态添加 active、get_controller、get_active、set_controller 等 API 支持
  - 影响：accept 返回后无法调用 active、controlling_process 等必要 API
- ✅ **修复 CS_SYN_RECV 状态未处理 ST_STATE 包** - aiutp_pcb.erl:423
  - 根因：只有 ST_DATA 包才触发 CS_SYN_RECV -> CS_CONNECTED 转换
  - 修复：同时处理 ST_STATE 和 ST_DATA 完成三次握手
  - 影响：服务端 accept 后收到纯 ACK 时无法进入 connected 状态
- ✅ **修复 controlling_process 未处理错误返回** - aiutp_channel.erl:284
  - 根因：case 语句只匹配 `{ok, ...}`，未处理 `{error, accepting}` 等错误
  - 修复：添加 `{error, _Reason} = Error` 分支
  - 影响：accepting 状态下调用 controlling_process 触发 case_clause 错误
- ✅ **修复 handle_controller_down map 更新操作符错误** - aiutp_channel.erl:822
  - 根因：`blocker := undefined` 使用 `:=` 更新不存在的键，accepting 状态下无 blocker
  - 修复：改用 `blocker => undefined`（插入操作符）
  - 影响：服务端 accepting 状态下 controller 进程崩溃时触发 badkey 错误
- ✅ **修复 do_closing_cleanup gen_statem 返回格式 bug** - aiutp_channel.erl:905
  - 根因：`{stop, normal, Data, Actions}` 不是有效的 gen_statem 返回格式
  - 修复：当有 Actions 时使用 `{stop_and_reply, normal, Actions}`，否则使用 `{stop, normal}`
  - 影响：调用 `aiutp:active` 时导致 `bad_return_from_state_function` 错误
- ✅ **修复 ST_STATE 包处理 function_clause 错误** - aiutp_channel.erl 中 process_incoming 调用参数错误
  - 根因：4 处 handle_packet_* 函数解构 `{Packet, RecvTime}` 后只传递 `Packet`，但 `process_incoming` 期望完整元组
  - 修复：使用 `= PacketWithTS` 模式匹配保留完整元组传递给 process_incoming
  - 影响函数：handle_packet_connecting, handle_packet_accepting, handle_packet_connected, handle_packet_closing
- ✅ **数据发送流程优化分析** - 深入分析发送流程，识别优化空间，对比 libutp
  - 分析 outque → outbuf → UDP 完整流程
  - 对比 libutp 发送策略（Nagle、批量发送、定时器频率）
  - 识别性能瓶颈（系统调用、二进制拷贝、缓冲区遍历）
  - 评估优化方案（批量发送、零拷贝、自适应定时器）
  - 生成详细分析报告：[docs/report/tx-optimization-analysis-2025-12-04.md](./docs/report/tx-optimization-analysis-2025-12-04.md)
  - **结论**: 当前实现已优化良好，无需大规模重构
- ✅ **libutp 差异分析报告** - 系统性分析 aiutp 与 libutp 参考实现的差异
  - 分析常量和参数差异（RTO_MIN、PACKET_SIZE、MTU 边界等）
  - 验证 LEDBAT 拥塞控制算法对齐
  - 验证快速重传和 SACK 处理对齐
  - 验证 RTT 计算和超时机制对齐
  - 生成详细差异报告：[docs/report/libutp-difference-analysis-2025-12-04.md](./docs/report/libutp-difference-analysis-2025-12-04.md)
- ✅ MTU 超时处理集成 (aiutp_pcb_timeout.erl)
- ✅ MTU 丢包检测集成 (aiutp_tx.erl)
- ✅ MTU 周期性重新探测 (30 分钟间隔)
- ✅ MTU 集成测试 (14 个测试用例)
- ✅ MTU 性能验证 (收敛步数、操作复杂度)
- ✅ 文档更新 (README.md)
- ✅ 清理无效导出 (TEST 保护内部函数)
- ✅ PropEr 属性测试 (5 个测试组)
- ✅ edoc API 文档 (aiutp 主模块)
- ✅ 性能基准测试 (5 个基准测试)

### 最近完成 (2025-12-03)

- ✅ MTU 发现模块实现 (aiutp_mtu.erl，29 个测试用例)
- ✅ MTU 探测包发送集成 (aiutp_net.erl)
- ✅ MTU ACK 处理集成 (aiutp_pcb.erl)
- ✅ MTU PCB 初始化集成 (aiutp_pcb.erl)
- ✅ 快速重传对齐 libutp (fast_resend_seq_nr 检查、send_skipped_packets)
- ✅ process_ack_and_sack 重构为子函数
- ✅ need_resend 和 skip_count 字段文档
- ✅ CS_CONNECTED_FULL 状态转换实现
- ✅ aiutp_buffer/aiutp_queue 模块重构
- ✅ aiutp_channel closing 状态修复
- ✅ aiutp_rx/aiutp_tx/aiutp_net 模块重构
- ✅ aiutp_rtt 模块重构
- ✅ 删除 aiutp_socket 向后兼容别名
- ✅ 缓冲区常量对齐 libutp
- ✅ aiutp_channel 模块重构
- ✅ clock_drift 惩罚机制
- ✅ 清理未使用的 PCB 字段
- ✅ aiutp_pcb_timeout 超时处理模块重构
- ✅ aiutp_delay 延迟估计模块重构
- ✅ LEDBAT 拥塞控制重构
- ✅ 常量优化（BEP-29 合规性）
- ✅ 监督树重构（故障隔离）
- ✅ aiutp_socket 代码重构和优化
- ✅ aiutp_pcb 与 aiutp_channel 配合修复
- ✅ 项目初始化和文档完善
- ✅ EUnit 测试套件 (210 个测试用例)
- ✅ PCB 模块拆分重构
- ✅ Dialyzer 类型规范支持
- ✅ OTP 模块状态 record 转换为 maps
- ✅ CS_DESTROY 状态转换修复

## 发现的工作

> 在开发过程中发现的新任务或需要注意的事项

### 测试相关
- [ ] 添加 Common Test 集成测试
- [x] 添加 PropEr 属性测试
- [ ] 为 gen_server/gen_statem 模块添加测试 (aiutp_socket, aiutp_channel)
- [ ] 为 aiutp_rtt:caculate_delay/4 添加测试 (需要 PCB 记录)
- [ ] 测试覆盖率达到核心功能 100%

### 架构改进
- [ ] 添加结构化日志和监控指标
- [ ] 配置管理改用应用环境变量

### 改进建议
- [ ] 考虑支持 IPv6

### 代码清理

**无效导出（仅内部使用，不应导出）:**
- [x] `aiutp_pcb:new/3` - 仅 connect/accept 内部调用 (已移除导出)
- [x] `aiutp_pcb:process/2` - 向后兼容别名，从未被调用 (已删除函数)
- [x] `aiutp_pcb_cc:maybe_decay_win/1` - 仅内部使用 (已添加 TEST 保护)
- [x] `aiutp_pcb_timeout:mark_need_resend/4` - 仅内部使用 (已添加 TEST 保护)

**仅测试使用的导出（已添加 TEST 保护）:**
- [ ] `aiutp_buffer:tail/1`
- [x] `aiutp_delay:new/0` (已添加 TEST 保护)
- [x] `aiutp_packet:fin/2` - 生产中用不同方式创建 FIN (已添加 TEST 保护)
- [x] `aiutp_queue:back/1`, `pop_back/1` (已添加 TEST 保护)
- [x] `aiutp_util:bit32_random/0` (已添加 TEST 保护)
- [x] `aiutp_util:wrapping_compare_less/3` (已添加 TEST 保护)

**低优先级:**
- [ ] 删除 `src/aiutp_test.erl` - 手动测试文件，非必需

## 里程碑

### v0.1.0 ✅ 已完成
- ✅ 基本协议实现
- ✅ OTP 结构
- ✅ 基础测试 (109 个测试用例)
- ✅ 文档完善 (README.md 增强)

### v0.2.0 (进行中)
- ✅ gen_statem 重构 (aiutp_channel)
- ✅ PCB Packet Processing 重构 (BEP-29 合规性)
- ✅ MTU 发现功能
- ✅ PropEr 属性测试
- ✅ edoc API 文档
- ✅ 性能基准测试
- ⏳ 完整测试覆盖 (当前 220 个测试)
- ⏳ API 稳定化

### v1.0.0 (目标)
- [ ] 生产就绪
- [ ] Hex.pm 发布
- [ ] 完整文档
- [ ] 示例应用

---

## 任务归档说明

完成的任务定期归档到 `docs/tasks/` 目录，按日期命名。索引请参见 [TASK_DONE.md](./TASK_DONE.md)。
