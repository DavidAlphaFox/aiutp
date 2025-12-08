%%------------------------------------------------------------------------------
%% @doc aiutp - uTP 协议常量和记录定义
%%
%% 本文件根据 BEP-29 规范定义常量和记录。
%% 参考: https://www.bittorrent.org/beps/bep_0029.html
%%
%% @end
%%------------------------------------------------------------------------------

-ifndef(AIUTP_HRL).
-define(AIUTP_HRL, true).

%%==============================================================================
%% 第 1 节: 协议版本 (BEP-29)
%%==============================================================================

-define(UTP_VERSION, 1).

%%==============================================================================
%% 第 2 节: 数据包类型 (BEP-29)
%%
%% | 类型     | 值  | 描述                           |
%% |----------|-----|--------------------------------|
%% | ST_DATA  | 0   | 常规数据包                     |
%% | ST_FIN   | 1   | 结束连接                       |
%% | ST_STATE | 2   | ACK 包（无载荷）               |
%% | ST_RESET | 3   | 强制终止连接                   |
%% | ST_SYN   | 4   | 发起连接                       |
%%==============================================================================

-define(ST_DATA,  0).
-define(ST_FIN,   1).
-define(ST_STATE, 2).
-define(ST_RESET, 3).
-define(ST_SYN,   4).

%%==============================================================================
%% 第 3 节: 扩展类型 (BEP-29)
%%
%% 扩展以链式结构存储，每个扩展以类型字节和长度字节开头。
%% 类型 0 表示没有更多扩展。
%%==============================================================================

-define(EXT_NONE, 0).        %% 无更多扩展
-define(EXT_SACK, 1).        %% 选择性确认位图
-define(EXT_EXT_BITS, 2).    %% 扩展位（保留供将来使用）

%%==============================================================================
%% 第 4 节: 连接状态
%%
%% 状态机:
%%   发起方: IDLE -> SYN_SENT -> CONNECTED -> DESTROY
%%   响应方: IDLE -> SYN_RECV -> CONNECTED -> DESTROY
%%==============================================================================

-define(CS_UNINITIALIZED, 'CS_UNINITIALIZED').
-define(CS_IDLE,          'CS_IDLE').
-define(CS_SYN_SENT,      'CS_SYN_SENT').
-define(CS_SYN_RECV,      'CS_SYN_RECV').
-define(CS_CONNECTED,     'CS_CONNECTED').
-define(CS_CONNECTED_FULL,'CS_CONNECTED_FULL').  %% 发送缓冲区已满
-define(CS_RESET,         'CS_RESET').
-define(CS_DESTROY,       'CS_DESTROY').

%%==============================================================================
%% 第 5 节: 超时参数 (BEP-29 / libutp)
%%
%% RTO 计算: timeout = max(rtt + rtt_var * 4, RTO_MIN)
%% 连续超时时: timeout *= 2（libutp 指数退避）
%%==============================================================================

%% libutp: 最小 RTO 为 1000ms
%% 注意: BEP-29 建议 500ms，但 libutp 使用 1000ms
%% 优化: 降低到 300ms 以提升高丢包环境下的响应速度
-define(RTO_MIN, 300).

%% BEP-29: 最大 RTO（实现相关）
-define(RTO_MAX, 6000).

%% libutp: 任何 RTT 样本之前的初始超时
-define(RTO_INITIAL, 1000).

%% 初始 RTT 方差估计（毫秒）
%% 根据 RFC 6298，初始 RTO 应为 1 秒
%% RTT_VAR 初始值 = (RTO_INITIAL - RTT) / 4 ≈ 250ms
-define(RTT_VAR_INITIAL, 250).

%% libutp: Keep-alive 间隔 29 秒
%% 用于保持 NAT 映射和检测死连接
-define(KEEPALIVE_INTERVAL, 29000).

%% Channel tick 定时器间隔（毫秒）
%% 相当于 libutp 建议的应用层调用频率 (50ms)
-define(CHANNEL_TICK_INTERVAL, 50).

%% 超时检查节流阈值（毫秒）
%% 与 libutp TIMEOUT_CHECK_INTERVAL 一致
%% check_timeouts 内部使用此值节流，实际超时检查最多每 500ms 执行一次
-define(TIMEOUT_CHECK_INTERVAL, 500).

%% 接收空闲超时（毫秒）
%% 当连接没有发送数据（纯接收方）且长时间未收到对端数据时触发
%% 用于检测对端崩溃导致的死连接，防止资源泄漏
-define(RECV_IDLE_TIMEOUT, 60000).

%% FIN 数据等待超时（毫秒）
%% 当收到 FIN（got_fin=true）但还有之前的数据包未到达（got_fin_reached=false）时触发
%% 10 秒足够对端重传所有缺失的包，超时说明对端可能已崩溃
-define(FIN_DATA_TIMEOUT, 10000).

%%==============================================================================
%% 第 5.1 节: 零窗口探测参数
%%
%% 当对端通告接收窗口为 0 时，发送方需要定期探测以检测窗口何时重新打开。
%% 使用指数退避策略：从 1 秒开始，每次翻倍，最大 10 秒。
%% 参考: RFC 793 (TCP), libutp
%%==============================================================================

%% 零窗口探测初始间隔（毫秒）
%% 当对端通告 wnd=0 时，等待此时间后开始第一次探测
-define(ZERO_WINDOW_PROBE_INTERVAL, 1000).

%% 零窗口探测最大间隔（毫秒）
%% 指数退避的上限，防止探测间隔过长
-define(ZERO_WINDOW_PROBE_MAX_INTERVAL, 10000).

%% 零窗口探测最大重试次数
%% 超过此次数后，恢复窗口尝试发送，如果仍然失败则由 RTO 机制处理
-define(ZERO_WINDOW_PROBE_MAX_RETRIES, 5).

%% RST 信息缓存超时（毫秒）
-define(RST_INFO_TIMEOUT, 10000).
-define(RST_INFO_LIMIT, 1000).

%%==============================================================================
%% 第 6 节: 窗口和缓冲区参数
%%==============================================================================

%% BEP-29: 固定头部大小为 20 字节
-define(UTP_HEADER_SIZE, 20).

%% 每个包的最大载荷大小（MTU - 头部）
%% 典型值: 1500 (MTU) - 20 (IP) - 8 (UDP) - 20 (uTP) = 1452
%% 保守值，用于处理路径 MTU 问题
-define(PACKET_SIZE, 1296).

%% BEP-29: 最小窗口大小为 150 字节
%% 这防止窗口缩小到零
-define(MIN_WINDOW_SIZE_BEP29, 150).

%% 实际使用的最小窗口大小
%% 设为 3 个包大小，确保流水线效率
-define(MIN_WINDOW_SIZE, (3 * ?PACKET_SIZE)).  %% = 3888 字节

%% 最大发送缓冲区大小（包数）
-define(OUTGOING_BUFFER_MAX_SIZE, 1024).

%% 重排序缓冲区大小（包数）
%% 用于接收乱序包的临时存储
-define(REORDER_BUFFER_SIZE, 32).

%%==============================================================================
%% 第 6.1 节: MTU 发现参数
%%
%% 路径 MTU 发现 (PMTUD) 使用二分查找探测网络路径支持的最大包大小。
%% 参考: RFC 4821 (PLPMTUD), libutp
%%==============================================================================

%% MTU 下限（载荷大小，字节）
%% TCP 最小 MTU 576 - IP(20) - UDP(8) - uTP(20) = 528
-define(MTU_FLOOR_DEFAULT, 528).

%% MTU 上限（载荷大小，字节）
%% 以太网 MTU 1500 - IP(20) - UDP(8) - uTP(20) = 1452
-define(MTU_CEILING_DEFAULT, 1452).

%% 二分查找终止阈值（字节）
%% 当 ceiling - floor <= 此值时，搜索完成
-define(MTU_SEARCH_THRESHOLD, 16).

%% 重新探测间隔（微秒）
%% libutp: 30 分钟 = 30 * 60 * 1000000
-define(MTU_PROBE_INTERVAL, 1800000000).

%% 探测失败回退阈值
%% 连续失败此次数后，回退到 floor 值
-define(MTU_PROBE_FAILURE_THRESHOLD, 3).

%% 探测失败后延长间隔倍数
-define(MTU_PROBE_BACKOFF_MULTIPLIER, 2).

%%==============================================================================
%% 第 7 节: 拥塞控制参数 (LEDBAT)
%%
%% LEDBAT（低额外延迟后台传输）目标:
%% - 最小化对网络的延迟影响
%% - 让步于其他流量
%% 参考: RFC 6817, libutp (utp_internal.cpp)
%%
%% LEDBAT 公式:
%%   cwnd += GAIN * off_target * bytes_acked * MSS / cwnd
%%   其中 off_target = (TARGET - queuing_delay) / TARGET
%%==============================================================================

%% RFC 6817: 目标延迟必须 <= 100ms
%% BEP-29: 默认 CCONTROL_TARGET = 100ms
-define(TARGET_DELAY, 200000).  %% 微秒

%% RFC 6817: GAIN 必须 <= 1
%% libutp: 每个 RTT 最大增加 MAX_CWND_INCREASE_BYTES_PER_RTT 字节
%% 对于标准 MSS (~1400 字节), 这意味着 GAIN ≈ 2
-define(MAX_CWND_INCREASE_BYTES_PER_RTT, 3000).

%% libutp: 无包发送时的窗口衰减间隔（毫秒）
-define(MAX_WINDOW_DECAY, 100).

%% libutp: 窗口未满载使用的检测阈值（毫秒）
%% 如果窗口在此时间内未被充分利用，不增长窗口
-define(WINDOW_SATURATION_TIMEOUT, 1000).

%% libutp: 保留的延迟样本数量
-define(CUR_DELAY_SIZE, 3).

%% libutp: 延迟基准历史条目数
%% 每分钟更新一次，保持 13 分钟的历史
%% 原因: 每 325 秒可能有约 10ms 的时钟偏移
-define(DELAY_BASE_HISTORY, 13).

%% RFC 6817: INIT_CWND 和 MIN_CWND 应为 2
%% 慢启动阈值初始值
-define(SSTHRESH_INITIAL, (?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE)).

%%==============================================================================
%% 第 8 节: 重传参数
%%==============================================================================

%% BEP-29: 快速重传的重复 ACK 阈值
%% 收到 3 个重复 ACK 后触发快速重传（与 TCP 一致）
-define(DUPLICATE_ACKS_BEFORE_RESEND, 3).

%% ACK 号允许窗口
%% ack_nr 差值大于此值的非 SYN 包被视为可疑
-define(ACK_NR_ALLOWED_WINDOW, 4).

%%==============================================================================
%% 第 9 节: 位掩码和限制
%%==============================================================================

-define(SEQ_NR_MASK, 16#FFFF).
-define(ACK_NR_MASK, 16#FFFF).
-define(TIMESTAMP_MASK, 16#FFFFFFFF).
-define(RTT_MAX, 16#FFFFFFFFFFFFFFFF).

%%==============================================================================
%% 第 10 节: 工具宏
%%
%% 用于序列号比较的环绕差值计算。
%% 如果 L < R 返回负数，如果 L > R 返回正数（考虑环绕）
%%==============================================================================

-define(WRAPPING_DIFF_32(L, R),
        (((R - L) band 16#FFFFFFFF) - ((L - R) band 16#FFFFFFFF))).

-define(WRAPPING_DIFF_16(L, R),
        (((R - L) band 16#FFFF) - ((L - R) band 16#FFFF))).

%%==============================================================================
%% 第 11 节: 数据包记录
%%
%% 根据 BEP-29 头部格式表示 uTP 数据包。
%%==============================================================================

-record(aiutp_packet, {
    %% 包类型: ST_DATA | ST_FIN | ST_STATE | ST_RESET | ST_SYN
    type :: ?ST_DATA | ?ST_FIN | ?ST_STATE | ?ST_RESET | ?ST_SYN,

    %% 连接 ID（16 位）
    %% 接收方使用 conn_id，发送方使用 conn_id + 1
    %% 包增量构建时可能为 undefined
    conn_id :: non_neg_integer() | undefined,

    %% 通告的接收窗口大小（32 位，字节）
    wnd = 0 :: non_neg_integer(),

    %% 本包的序列号（16 位）
    seq_nr = 0 :: non_neg_integer(),

    %% 最后接收的序列号（16 位）
    ack_nr = 0 :: non_neg_integer(),

    %% 包发送时的时间戳（32 位，微秒）
    tv_usec = 0 :: non_neg_integer(),

    %% 时间戳差值: 我方时间戳 - 对方时间戳（32 位，微秒）
    %% 接收方用于计算单向延迟
    reply_micro = 0 :: non_neg_integer(),

    %% 扩展: [{sack, binary()} | {ext_bits, binary()}]
    extension = [] :: list(),

    %% 载荷数据
    payload = <<>> :: binary()
}).

%%==============================================================================
%% 第 12 节: 数据包包装记录
%%
%% 包装数据包及其传输元数据，用于发送缓冲区。
%%==============================================================================

-record(aiutp_packet_wrap, {
    %% 待发送的数据包
    packet :: #aiutp_packet{},

    %% 序列化的包内容（缓存用于重传）
    content = undefined :: undefined | binary(),

    %% 载荷大小（字节）
    payload = 0 :: non_neg_integer(),

    %% 包发送时间（微秒）
    time_sent = 0 :: non_neg_integer(),

    %% 本包已传输次数
    transmissions = 0 :: non_neg_integer(),

    %% 标记包需要重传
    need_resend = false :: boolean(),

    %% BEP-29: 本包被 SACK 跳过的次数
    %% 当 skip_count >= 3 时，包应标记为快速重传
    skip_count = 0 :: non_neg_integer(),

    %% MTU 探测标记
    %% 当为 true 时，此包用于 MTU 发现探测
    is_mtu_probe = false :: boolean()
}).

%%==============================================================================
%% 第 13 节: 协议控制块 (PCB) 记录
%%
%% PCB 包含单个 uTP 连接的所有状态。
%% 字段按功能区域组织以提高清晰度。
%%==============================================================================

-record(aiutp_pcb, {
    %%--------------------------------------------------------------------------
    %% 连接标识
    %%--------------------------------------------------------------------------

    %% 接收包的连接 ID
    conn_id_recv :: non_neg_integer() | undefined,

    %% 发送包的连接 ID（发起方为 conn_id_recv + 1）
    conn_id_send :: non_neg_integer() | undefined,

    %% UDP socket 和远端地址的引用
    %% 格式: {gen_udp:socket(), {inet:ip_address(), inet:port_number()}} | undefined
    socket :: {gen_udp:socket(), {inet:ip_address(), inet:port_number()}} | undefined,

    %%--------------------------------------------------------------------------
    %% 连接状态
    %%--------------------------------------------------------------------------

    %% 当前连接状态
    state = ?CS_UNINITIALIZED :: atom(),

    %% 用户已调用 close()
    close_requested = false :: boolean(),

    %%--------------------------------------------------------------------------
    %% 序列号管理
    %%--------------------------------------------------------------------------

    %% 下一个要发送的序列号（SYN 从 1 开始）
    seq_nr = 1 :: non_neg_integer(),

    %% 我们已确认的最后序列号（所有此序列号之前的包已收到）
    ack_nr = 0 :: non_neg_integer(),

    %% 下一个允许快速重发的序列号（防止重复快速重发）
    fast_resend_seq_nr = 1 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% FIN 处理（连接终止）
    %%--------------------------------------------------------------------------

    %% 收到了 FIN 包
    got_fin = false :: boolean(),

    %% FIN 之前的所有包都已收到
    got_fin_reached = false :: boolean(),

    %% 我们已发送 FIN
    fin_sent = false :: boolean(),

    %% 我们的 FIN 已被确认
    fin_sent_acked = false :: boolean(),

    %% 收到的 FIN 包的序列号
    eof_pkt = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% 窗口管理
    %%--------------------------------------------------------------------------

    %% 当前在途字节数（已发送但未确认）
    %% 不包括标记为重发的包
    cur_window = 0 :: non_neg_integer(),

    %% 发送队列中的包数量（包括需要重发的）
    %% 最老的未确认包在 seq_nr - cur_window_packets
    cur_window_packets = 0 :: non_neg_integer(),

    %% 最大发送窗口（字节），由拥塞控制调整
    max_window = 0 :: non_neg_integer(),

    %% 对端通告的最大接收窗口（字节）
    %% libutp: max_window_user = 255 * PACKET_SIZE
    %% 减 1 为 FIN 包预留空间
    max_window_user = 255 * ?PACKET_SIZE :: non_neg_integer(),

    %% 我们通告的最后接收窗口（字节）
    last_rcv_win = 0 :: non_neg_integer(),

    %% 窗口最后衰减的时间戳（会环绕）
    last_rwin_decay = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% RTT/RTO 管理
    %%--------------------------------------------------------------------------

    %% 平滑往返时间（毫秒）
    rtt = 0 :: non_neg_integer(),

    %% RTT 方差（毫秒）
    rtt_var = ?RTT_VAR_INITIAL :: non_neg_integer(),

    %% 重传超时（毫秒）
    %% 计算公式: max(rtt + rtt_var * 4, RTO_MIN)
    rto = ?RTO_INITIAL :: non_neg_integer(),

    %% RTT 历史，用于计算最小 RTT
    rtt_hist :: any(),

    %% 当前重传超时值（可能已退避）
    retransmit_timeout = 0 :: non_neg_integer(),

    %% RTO 到期的绝对时间
    rto_timeout = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% 拥塞控制 (LEDBAT)
    %%--------------------------------------------------------------------------

    %% LEDBAT 目标延迟（微秒）
    %% BEP-29 指定 100ms = 100000us
    target_delay = ?TARGET_DELAY :: non_neg_integer(),

    %% 慢启动阈值（字节）
    ssthresh = ?SSTHRESH_INITIAL :: non_neg_integer(),

    %% 处于慢启动阶段（指数增长）
    slow_start = true :: boolean(),

    %% 窗口最后满载使用的时间戳
    %% 用于在未满负荷发送时防止窗口增长
    last_maxed_out_window = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% 延迟统计（用于 LEDBAT）
    %%--------------------------------------------------------------------------

    %% 我方延迟历史（我们测量的样本）
    our_hist :: any(),

    %% 对方延迟历史（对方报告的样本）
    their_hist :: any(),

    %% 相对于初始样本的平均延迟（微秒）
    average_delay = 0 :: integer(),

    %% 最近延迟样本的总和（用于平均）
    current_delay_sum = 0 :: integer(),

    %% current_delay_sum 中的样本数
    current_delay_samples = 0 :: non_neg_integer(),

    %% 第一个原始延迟样本（相对延迟的基准）
    average_delay_base = 0 :: non_neg_integer(),

    %% 下次添加平均延迟样本的时间
    average_sample_time = 0 :: non_neg_integer(),

    %% 最后测量的延迟（微秒）
    last_measured_delay = 0 :: non_neg_integer(),

    %% 估计的时钟漂移（每 5 秒的微秒数）
    clock_drift = 0 :: integer(),

    %%--------------------------------------------------------------------------
    %% 重传管理
    %%--------------------------------------------------------------------------

    %% 连续重传次数
    retransmit_count = 0 :: non_neg_integer(),

    %% 收到的重复 ACK 数量
    duplicate_ack = 0 :: non_neg_integer(),

    %% 快速超时标志
    fast_timeout = false :: boolean(),

    %% 乱序包数量
    reorder_count = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% 时间戳
    %%--------------------------------------------------------------------------

    %% 当前时间（处理前设置）
    time = undefined :: non_neg_integer() | undefined,

    %% 最后收到包的时间
    recv_time = undefined :: non_neg_integer() | undefined,

    %% 最后收到任何包的时间（用于 keep-alive）
    last_got_packet = 0 :: non_neg_integer(),

    %% 最后发送任何包的时间
    last_sent_packet = 0 :: non_neg_integer(),

    %% 要回显给发送方的时间戳差值
    reply_micro = 0 :: non_neg_integer(),

    %% 零窗口探测定时器（窗口为 0 时定期发送探测包）
    zerowindow_time = 0 :: non_neg_integer(),

    %% 零窗口探测重试计数
    %% 用于实现指数退避，每次探测失败时递增
    zerowindow_probes = 0 :: non_neg_integer(),

    %% 上次超时检查的时间（毫秒）
    %% 用于节流，防止过于频繁的检查（与 libutp TIMEOUT_CHECK_INTERVAL 对应）
    last_timeout_check = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% 缓冲区和队列
    %%--------------------------------------------------------------------------

    %% 接收缓冲区（用于重组的环形缓冲区）
    inbuf :: any(),

    %% 发送缓冲区（用于未确认包的环形缓冲区）
    outbuf :: any(),

    %% 接收队列（准备好供应用程序读取的有序数据）
    inque :: any(),

    %% 发送队列（等待打包的数据）
    outque :: any(),

    %%--------------------------------------------------------------------------
    %% 特殊模式
    %%--------------------------------------------------------------------------

    %% 立即数据确认模式
    ida = false :: boolean(),

    %%--------------------------------------------------------------------------
    %% MTU 发现
    %%--------------------------------------------------------------------------

    %% MTU 二分查找下限（字节）
    mtu_floor = ?MTU_FLOOR_DEFAULT :: non_neg_integer(),

    %% MTU 二分查找上限（字节）
    mtu_ceiling = ?MTU_CEILING_DEFAULT :: non_neg_integer(),

    %% 当前使用的 MTU（载荷大小，字节）
    mtu_last = ?PACKET_SIZE :: non_neg_integer(),

    %% 在途探测包的序列号（0 表示无在途探测）
    mtu_probe_seq = 0 :: non_neg_integer(),

    %% 在途探测包的大小（字节）
    mtu_probe_size = 0 :: non_neg_integer(),

    %% 下次重新探测时间（微秒，绝对时间）
    mtu_discover_time = 0 :: non_neg_integer(),

    %% 连续探测失败次数
    mtu_probe_failures = 0 :: non_neg_integer()
}).

-endif. %% AIUTP_HRL
