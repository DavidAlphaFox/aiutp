# AIUTP 任务追踪

> 最后更新: 2025-12-03

## 当前任务

### 进行中

| 任务 | 描述 | 开始日期 | 状态 |
|------|------|----------|------|
| (无) | - | - | - |

### 待处理

| 任务 | 描述 | 优先级 |
|------|------|--------|
| **MTU 发现** | 实现路径 MTU 发现功能 | **高** |
| 清理无效导出 | 移除仅内部使用的函数导出 | 中 |
| 属性测试 | 使用 PropEr 添加属性测试 | 中 |
| API 文档 | 添加 edoc 格式的 API 文档 | 中 |
| 性能测试 | 建立性能基准测试 | 低 |
| Hex 发布 | 准备发布到 Hex.pm | 低 |
| 删除测试文件 | 删除 src/aiutp_test.erl | 低 |

---

## MTU 发现功能任务分解

> 详细设计文档: [docs/requirements/features/mtu-discovery.md](./docs/requirements/features/mtu-discovery.md)

### 阶段 1: 基础设施准备

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| MTU-1.1 | 常量定义 | 在 aiutp.hrl 添加 MTU 相关常量 | [x] |
| MTU-1.2 | PCB 记录扩展 | 添加 mtu_floor, mtu_ceiling 等字段 | [x] |
| MTU-1.3 | packet_wrap 扩展 | 添加 is_mtu_probe 标记 | [x] |
| MTU-1.4 | 配置接口 | 添加 MTU 配置读取/设置函数 | [-] 暂不需要 |

### 阶段 2: 核心逻辑实现

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| MTU-2.1 | aiutp_mtu 模块 | 创建新模块实现核心 MTU 逻辑 | [x] |
| MTU-2.2 | 探测包发送集成 | 修改 aiutp_net.erl 支持探测包 | [x] |
| MTU-2.3 | ACK 处理集成 | 修改 aiutp_pcb.erl 处理探测 ACK | [x] |
| MTU-2.4 | 超时处理集成 | 修改 aiutp_pcb_timeout.erl 处理探测超时 | [ ] |
| MTU-2.5 | 丢包检测集成 | 修改 aiutp_tx.erl 检测探测包丢失 | [ ] |
| MTU-2.6 | 周期性重新探测 | 实现 30 分钟周期重新探测 | [ ] |
| MTU-2.7 | PCB 初始化集成 | 修改 aiutp_pcb.erl 初始化 MTU 状态 | [x] |

### 阶段 3: 测试和文档

| 任务 ID | 任务 | 描述 | 状态 |
|---------|------|------|------|
| MTU-3.1 | 单元测试 | 创建 aiutp_mtu_tests.erl (29 个测试) | [x] |
| MTU-3.2 | 集成测试 | 创建 MTU 集成测试场景 | [ ] |
| MTU-3.3 | 性能验证 | 验证探测开销和收敛时间 | [ ] |
| MTU-3.4 | 文档更新 | 更新 README 和 API 文档 | [ ] |

## 已完成任务

> 详细的已完成任务记录请参见 [TASK_DONE.md](./TASK_DONE.md)

### 最近完成 (2025-12-03)

- [x] MTU 发现模块实现 (aiutp_mtu.erl，29 个测试用例)
- [x] MTU 探测包发送集成 (aiutp_net.erl)
- [x] MTU ACK 处理集成 (aiutp_pcb.erl)
- [x] MTU PCB 初始化集成 (aiutp_pcb.erl)
- [x] 快速重传对齐 libutp (fast_resend_seq_nr 检查、send_skipped_packets)
- [x] process_ack_and_sack 重构为子函数
- [x] need_resend 和 skip_count 字段文档
- [x] CS_CONNECTED_FULL 状态转换实现
- [x] aiutp_buffer/aiutp_queue 模块重构
- [x] aiutp_channel closing 状态修复
- [x] aiutp_rx/aiutp_tx/aiutp_net 模块重构
- [x] aiutp_rtt 模块重构
- [x] 删除 aiutp_socket 向后兼容别名
- [x] 缓冲区常量对齐 libutp
- [x] aiutp_channel 模块重构
- [x] clock_drift 惩罚机制
- [x] 清理未使用的 PCB 字段
- [x] aiutp_pcb_timeout 超时处理模块重构
- [x] aiutp_delay 延迟估计模块重构
- [x] LEDBAT 拥塞控制重构
- [x] 常量优化（BEP-29 合规性）
- [x] 监督树重构（故障隔离）
- [x] aiutp_socket 代码重构和优化
- [x] aiutp_pcb 与 aiutp_channel 配合修复
- [x] 项目初始化和文档完善
- [x] EUnit 测试套件 (158 个测试用例)
- [x] PCB 模块拆分重构
- [x] Dialyzer 类型规范支持
- [x] OTP 模块状态 record 转换为 maps
- [x] CS_DESTROY 状态转换修复

## 发现的工作

> 在开发过程中发现的新任务或需要注意的事项

### 测试相关
- [ ] 添加 Common Test 集成测试
- [ ] 添加 PropEr 属性测试
- [ ] 为 gen_server/gen_statem 模块添加测试 (aiutp_socket, aiutp_channel)
- [ ] 为 aiutp_rtt:caculate_delay/4 添加测试 (需要 PCB 记录)
- [ ] 测试覆盖率达到核心功能 100%

### 架构改进
- [ ] 添加结构化日志和监控指标
- [ ] 配置管理改用应用环境变量

### 改进建议
- [ ] 考虑支持 IPv6
- [ ] MTU 发现功能（已规划，见上方任务分解）

### 代码清理

**无效导出（仅内部使用，不应导出）:**
- [ ] `aiutp_pcb:new/3` - 仅 connect/accept 内部调用
- [ ] `aiutp_pcb:process/2` - 向后兼容别名，从未被调用
- [ ] `aiutp_pcb_cc:maybe_decay_win/1` - 仅内部使用
- [ ] `aiutp_pcb_timeout:mark_need_resend/4` - 仅内部使用

**仅测试使用的导出（考虑保留或移除）:**
- [ ] `aiutp_buffer:tail/1`
- [ ] `aiutp_delay:new/0`
- [ ] `aiutp_packet:fin/2` - 生产中用不同方式创建 FIN
- [ ] `aiutp_queue:back/1`, `pop_back/1`
- [ ] `aiutp_util:bit32_random/0`
- [ ] `aiutp_util:wrapping_compare_less/3`

**低优先级:**
- [ ] 删除 `src/aiutp_test.erl` - 手动测试文件，非必需

## 里程碑

### v0.1.0 (已完成 ✅)
- [x] 基本协议实现
- [x] OTP 结构
- [x] 基础测试 (109 个测试用例)
- [x] 文档完善 (README.md 增强)

### v0.2.0 (进行中)
- [x] gen_statem 重构 (aiutp_channel)
- [x] PCB Packet Processing 重构 (BEP-29 合规性)
- [ ] 完整测试覆盖 (当前 187 个测试)
- [ ] 性能优化
- [ ] API 稳定化

### v1.0.0 (目标)
- [ ] 生产就绪
- [ ] Hex.pm 发布
- [ ] 完整文档
- [ ] 示例应用

---

## 任务归档说明

完成的任务定期归档到 `docs/tasks/` 目录，按日期命名。索引请参见 [TASK_DONE.md](./TASK_DONE.md)。
