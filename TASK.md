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
| 属性测试 | 使用 PropEr 添加属性测试 | 中 |
| API 文档 | 添加 edoc 格式的 API 文档 | 中 |
| 性能测试 | 建立性能基准测试 | 低 |
| Hex 发布 | 准备发布到 Hex.pm | 低 |

## 已完成任务

### 2025-12-03
- [x] 监督树重构（故障隔离）
  - 新结构：aiutp_sup (simple_one_for_one) → aiutp_socket_sup (one_for_all) → {socket, channel_sup}
  - 每个 socket 拥有独立的 channel_sup 实例
  - socket 崩溃时自动终止所有相关 channel
  - 修改 aiutp_sup.erl: rest_for_one → simple_one_for_one
  - 修改 aiutp_socket_sup.erl: simple_one_for_one → one_for_all，管理 socket + channel_sup
  - 修改 aiutp_channel_sup.erl: 移除全局注册，new/2 → new/3
  - 修改 aiutp_socket.erl: 从父监督者获取 channel_sup pid
  - 修改 aiutp_acceptor.erl: start_link/4 → start_link/5，添加 channel_sup 参数
  - 修改 aiutp.erl: open 调用 aiutp_sup:new/2
  - 更新测试用例适配新结构
  - 147 个测试全部通过
- [x] aiutp_socket 代码重构和注释
  - 添加完整模块文档和架构图
  - 函数重命名: `add_conn_inner` → `do_register_channel`, `free_conn_inner` → `do_unregister_channel`
  - 函数重命名: `reset_conn` → `send_reset`, `dispatch` → `dispatch_packet`
  - 新增 API: `register_channel/3`, `unregister_channel/3` (保留向后兼容别名)
  - 拆分复杂函数: `handle_udp_packet/4`, `dispatch_packet/3`, `handle_unknown_connection/10`
  - 新增辅助函数: `open_udp_socket/2`, `ensure_binary_mode/1`, `close_socket_if_open/1`
  - 添加 UDP 缓冲区大小宏定义
  - 代码按功能分组: API、gen_server 回调、内部函数
  - 全部使用中文注释
  - 146 个测试全部通过
- [x] aiutp_socket 代码优化
  - 改进 `connect` 函数错误处理，使用嵌套 case 替代模式匹配崩溃
  - 添加 `conn_count` 字段缓存连接数，避免 `dispatch` 中重复调用 `maps:size`
  - 更新 `add_conn_inner`、`free_conn_inner`、`handle_info DOWN` 同步维护计数
  - 146 个测试全部通过
- [x] aiutp_pcb 与 aiutp_channel 配合修复
  - 修复 `aiutp_pcb:write/2` 返回值统一为 `{ok, PCB} | {{error, atom()}, PCB}`
  - 修复 `aiutp_channel` 中 `connected` 状态对 write 返回值的处理
  - 修复 `connecting` 状态控制器崩溃时调用 `aiutp_pcb:close()` 通知对端
  - 修复 `accepting` 状态控制器崩溃时调用 `aiutp_pcb:close()` 通知对端
  - 146 个测试全部通过
- [x] 项目初始化
- [x] 创建 PLANNING.md 规划文档
- [x] 创建 TASK.md 任务追踪
- [x] 创建文档目录结构
- [x] 完成全面代码分析报告
- [x] 为核心纯函数模块添加 EUnit 测试 (100 个测试用例)
  - aiutp_util: 100% 覆盖率
  - aiutp_queue: 100% 覆盖率
  - aiutp_delay: 100% 覆盖率
  - aiutp_packet: 97% 覆盖率
  - aiutp_buffer: 96% 覆盖率
- [x] BEP-29 协议分析报告 (docs/report/bep29-analysis-2025-12-03.md)
  - 丢包重传机制分析
  - 对端崩溃处理分析
  - 断开连接处理分析
  - 项目实现与标准差异对比
- [x] PCB 和常量重构 (include/aiutp.hrl)
  - 常量按 BEP-29 标准分组（13 个 Section）
  - 添加协议版本、扩展类型、超时参数等新常量
  - PCB 记录按功能分组（连接标识、状态、序列号、FIN处理、窗口、RTT/RTO、拥塞控制、延迟统计、重传、时间戳、缓冲区、特殊模式）
  - 修复拼写错误：`brust` -> `burst`, `fasle` -> `false`
  - 添加详细的字段注释和类型规范
- [x] 高优先级技术债务修复
  - 修复 aiutp_sup.erl supervisor 子进程 ID 拼写错误
  - 修复 aiutp_worker.erl 不存在的函数调用 (remove_conn -> free_conn)
  - 修复 aiutp_worker.erl 拼写错误 (undefiend -> undefined)
  - 改进 aiutp_socket.erl 数据包解码错误日志 (使用 logger:debug)
  - 改进 aiutp_net.erl UDP 发送错误处理 (不再崩溃进程)
  - 添加 aiutp_sup_tests 和 aiutp_net_tests 单元测试 (109 个测试用例)
- [x] 文档完善 (v0.1.0 里程碑完成)
  - 增强 README.md：添加徽章、详细 API 文档
  - 添加服务端/客户端/Active 模式使用示例
  - 添加安装和快速开始指南
  - 添加协议参数说明表格
  - 添加架构概览和模块职责说明
  - 添加开发指南（构建、测试、类型检查）
- [x] Worker 重构为 Channel (gen_statem)
  - 创建 aiutp_channel.erl 使用 gen_statem 行为
  - 实现 5 个状态：idle, connecting, accepting, connected, closing
  - 创建 aiutp_channel_sup.erl 监督器
  - 集成到 aiutp_sup 监督树
  - 更新 aiutp_socket.erl 使用 channel
  - 更新 aiutp_acceptor.erl 使用 channel
  - 更新 aiutp.erl API 使用 channel
  - 添加 aiutp_channel_tests.erl 单元测试 (6 个测试用例)
  - 删除旧模块：aiutp_worker.erl, aiutp_worker_sup.erl
  - 总计 115 个测试用例通过
- [x] 代码质量修复
  - 替换 aiutp_pcb.erl 中的 io:format 为 logger:warning (3 处)
  - 改进 aiutp_sup.erl 监督策略 (one_for_all → rest_for_one)
  - 为 aiutp_util.erl 中的 wrapping_compare_less 添加文档和类型规范
  - 更新相关测试用例
  - 总计 115 个测试用例通过
- [x] aiutp_pcb.erl 模块拆分重构
  - 创建 aiutp_pcb_cc.erl 拥塞控制模块 (LEDBAT 算法)
    - cc_control/4: 主拥塞控制逻辑
    - maybe_decay_win/1: 窗口衰减
    - ack_packet/3: ACK 包 RTT 处理
    - caculate_acked_bytes/4: 计算已确认字节数
    - selective_ack_packet/3: SACK 处理
  - 创建 aiutp_pcb_timeout.erl 超时处理模块
    - check_timeouts/1: 超时检查入口
    - mark_need_resend/4: 标记重传包
  - 重构 aiutp_pcb.erl (669行 → 585行)
    - 添加详细的 edoc 文档
    - 改进函数命名 (process → process_by_type)
    - 清理注释和格式
  - 添加新模块测试 (20 个测试用例)
    - aiutp_pcb_cc_tests.erl (11 个测试)
    - aiutp_pcb_timeout_tests.erl (9 个测试)
  - 总计 135 个测试用例通过
- [x] PCB Packet Processing 重构 (BEP-29 合规性改进)
  - 阶段 1: 函数重命名
    - process/2 → process_incoming/2 (保留兼容别名)
    - process_by_type/3 → dispatch_by_type/3
    - process_packet/2 → validate_and_init/2
    - process_packet_1/2 → handle_duplicate_acks/2
    - process_packet_2/2 → process_ack_and_sack/2
    - process_packet_3/2 → update_connection_state/2
    - process_packet_4/2 → handle_data_and_fin/2
  - 阶段 2: 连接断开处理改进
    - 添加 aiutp_net:send_reset/1 函数
    - 超时时发送 RESET 包通知对端 (BEP-29)
  - 阶段 3: SACK 处理改进
    - 添加 skip_count 字段跟踪包被 SACK 跳过次数
    - 实现 aiutp_tx:update_skip_counts/2 检测 SACK 缺口
    - 被跳过 3+ 次的包标记为快速重传 (BEP-29)
  - 阶段 4: 测试验证
    - 添加 aiutp_tx_tests.erl (9 个测试)
    - 添加 aiutp_net 额外测试 (2 个测试)
  - 总计 146 个测试用例通过
- [x] Dialyzer 类型规范支持
  - 配置 rebar.config 添加 dialyzer 设置
  - 为数据结构添加 opaque 类型 (aiutp_queue, aiutp_buffer, aiutp_delay)
  - 为 API 添加导出类型 (utp_socket, utp_connection, socket_ref)
  - 为所有模块添加 -spec 类型规范
  - 修复类型不一致 (PCB socket 字段、packet conn_id 字段、整数运算)
  - Dialyzer 警告: 62 → 19 (剩余为风格警告)
  - 146 个测试全部通过
- [x] OTP 模块状态 record 转换为 maps
  - aiutp_socket.erl: #state{} → maps (6 个字段)
  - aiutp_acceptor.erl: #state{} → maps (7 个字段)
  - aiutp_channel.erl: #data{} → maps (11 个字段)
  - 添加详细类型规范 (-type state(), -type data())
  - 保留性能关键 records: #aiutp_pcb{}, #aiutp_packet{}, #aiutp_buffer{}
  - 146 个测试全部通过
- [x] CS_DESTROY 状态转换修复 (对齐 libutp 实现)
  - 分析报告: docs/report/cs-destroy-analysis-2025-12-03.md
  - 修复 SYN_RECV 超时: 添加 send_reset 调用
  - 修复 closed/1: 移除非 CS_DESTROY 状态下对 got_fin_reached 的处理
  - 修复 close() 在 SYN_SENT/SYN_RECV: 发送 RESET 后进入 CS_DESTROY
  - 优化 closed/1 函数: 简化逻辑，移除 crash 返回值
  - 修复 format_status 废弃警告 (format_status/2 → format_status/1)
  - 146 个测试全部通过

### 历史任务
- [x] 实现 uTP 协议核心逻辑 (aiutp_pcb)
- [x] 实现数据包编解码 (aiutp_packet)
- [x] 实现 OTP supervisor 树结构
- [x] 实现基本的连接管理 (aiutp_socket, aiutp_channel)
- [x] 实现 LEDBAT 拥塞控制
- [x] 实现选择性确认 (SACK)
- [x] 添加 Micro Transport Protocol 注释

## 发现的工作

> 在开发过程中发现的新任务或需要注意的事项

### 技术债务（高优先级 - 立即修复）
- [x] ~~**aiutp_pcb.erl:493** - 🔴 `write/2` 类型签名与实现不一致~~ (已修复: 统一返回 `{ok, PCB}` 格式)
- [x] ~~**aiutp_channel.erl:292-297** - ⚠️ connecting 状态控制器崩溃未调用 `aiutp_pcb:close()` 通知对端~~ (已修复)
- [x] ~~**aiutp_channel.erl:346-349** - ⚠️ accepting 状态控制器崩溃未调用 `aiutp_pcb:close()` 通知对端~~ (已修复)
- [x] ~~**aiutp_worker.erl:329** - 🔴 严重：调用不存在的函数 `aiutp_socket:remove_conn/2`，应改为 `free_conn/3`~~ (已修复)
- [x] ~~**aiutp.hrl:89** - 🔴 拼写错误: `ida = fasle` 应为 `ida = false`~~ (已在 PCB 重构中修复)
- [x] ~~**aiutp_pcb.erl:505** - 🔴 拼写错误: `{fasle,PCB#...` 应为 `{false,PCB#...`~~ (已修复)
- [x] ~~**aiutp_worker.erl:193** - 🔴 拼写错误: `undefiend` 应为 `undefined`~~ (已修复)
- [x] ~~**aiutp_worker.erl:314** - 🔴 拼写错误: `undefiend` 应为 `undefined`~~ (已修复)
- [x] ~~**aiutp_sup.erl:22** - 🔴 拼写错误: `aiutp_woker_sup` 应为 `aiutp_worker_sup`~~ (已修复)
- [x] ~~**aiutp_socket.erl:168-169** - ⚠️ 数据包解码失败时静默丢弃，需添加日志~~ (已添加 logger:debug)
- [x] ~~**aiutp_net.erl:349** - ⚠️ UDP 发送失败时直接崩溃进程，应优雅处理~~ (已改为返回错误并记录日志)

### 测试相关
- [x] 添加 EUnit 单元测试套件 (115 个测试用例)
- [ ] 添加 Common Test 集成测试
- [ ] 添加 PropEr 属性测试
- [ ] 为 gen_server/gen_statem 模块添加测试 (aiutp_socket, aiutp_channel)
- [ ] 为 aiutp_rtt:caculate_delay/4 添加测试 (需要 PCB 记录)
- [ ] 测试覆盖率达到核心功能 100%

### 架构改进
- [x] `aiutp_pcb.erl` - 模块拆分完成 (669行 → 585行 + 2个子模块)
  - aiutp_pcb_cc.erl: 拥塞控制 (~200行)
  - aiutp_pcb_timeout.erl: 超时处理 (~180行)
- [x] `aiutp_channel.erl` - gen_statem 状态机实现完成
- [x] 使用 logger 模块替代 io:format (aiutp_pcb.erl, aiutp_socket.erl)
- [x] 改进监督策略 (one_for_all → rest_for_one)
- [ ] 添加结构化日志和监控指标
- [ ] 配置管理改用应用环境变量

### 改进建议
- [x] 添加 dialyzer 类型规范检查 (已完成 2025-12-03)
- [x] 使用 maps 替代部分 record，提高可读性 (已完成 2025-12-03)
- [ ] 考虑支持 IPv6

## 里程碑

### v0.1.0 (已完成 ✅)
- [x] 基本协议实现
- [x] OTP 结构
- [x] 基础测试 (109 个测试用例)
- [x] 文档完善 (README.md 增强)

### v0.2.0 (进行中)
- [x] gen_statem 重构 (aiutp_channel)
- [x] PCB Packet Processing 重构 (BEP-29 合规性)
- [ ] 完整测试覆盖 (当前 146 个测试)
- [ ] 性能优化
- [ ] API 稳定化

### v1.0.0 (目标)
- [ ] 生产就绪
- [ ] Hex.pm 发布
- [ ] 完整文档
- [ ] 示例应用

---

## 任务归档说明

完成的任务将定期归档到 `docs/tasks/` 目录，按日期命名。例如:
- `docs/tasks/2025-12-03.md`
