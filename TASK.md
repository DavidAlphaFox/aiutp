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
| 清理无效导出 | 移除仅内部使用的函数导出 | 中 |
| 属性测试 | 使用 PropEr 添加属性测试 | 中 |
| API 文档 | 添加 edoc 格式的 API 文档 | 中 |
| 性能测试 | 建立性能基准测试 | 低 |
| Hex 发布 | 准备发布到 Hex.pm | 低 |
| 删除测试文件 | 删除 src/aiutp_test.erl | 低 |

## 已完成任务

> 详细的已完成任务记录请参见 [TASK_DONE.md](./TASK_DONE.md)

### 最近完成 (2025-12-03)

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
- [ ] 完整测试覆盖 (当前 158 个测试)
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
