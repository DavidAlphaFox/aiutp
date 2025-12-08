%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_pcb_timeout module
%%
%% Tests timeout handling and packet resend marking.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_timeout_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% Test: mark_need_resend/4
%%==============================================================================

mark_need_resend_empty_buffer_test() ->
    %% Empty buffer (Iter = -1) should return unchanged
    OutBuf = aiutp_buffer:new(16),
    {CurWindow, OutBuf0} = aiutp_pcb_timeout:mark_need_resend(5, 10000, -1, OutBuf),
    ?assertEqual(10000, CurWindow),
    ?assertEqual(OutBuf, OutBuf0).

mark_need_resend_zero_packets_test() ->
    %% Zero packets should return unchanged
    OutBuf = aiutp_buffer:new(16),
    WrapPacket = #aiutp_packet_wrap{
        transmissions = 1,
        need_resend = false,
        payload = 1400
    },
    OutBuf1 = aiutp_buffer:append(WrapPacket, OutBuf),
    Iter = aiutp_buffer:head(OutBuf1),
    {CurWindow, _} = aiutp_pcb_timeout:mark_need_resend(0, 10000, Iter, OutBuf1),
    ?assertEqual(10000, CurWindow).

mark_need_resend_marks_packets_test() ->
    %% Should mark packets for resend and adjust window
    OutBuf = aiutp_buffer:new(16),
    WrapPacket1 = #aiutp_packet_wrap{
        transmissions = 1,
        need_resend = false,
        payload = 1400
    },
    WrapPacket2 = #aiutp_packet_wrap{
        transmissions = 1,
        need_resend = false,
        payload = 1000
    },
    OutBuf1 = aiutp_buffer:append(WrapPacket1, OutBuf),
    OutBuf2 = aiutp_buffer:append(WrapPacket2, OutBuf1),
    Iter = aiutp_buffer:head(OutBuf2),

    {CurWindow, OutBuf3} = aiutp_pcb_timeout:mark_need_resend(2, 10000, Iter, OutBuf2),

    %% Window should be reduced by payload of marked packets
    ?assertEqual(10000 - 1400 - 1000, CurWindow),

    %% Packets should be marked
    Iter1 = aiutp_buffer:head(OutBuf3),
    Marked1 = aiutp_buffer:data(Iter1, OutBuf3),
    ?assertEqual(true, Marked1#aiutp_packet_wrap.need_resend).

mark_need_resend_skips_already_marked_test() ->
    %% Should skip packets already marked for resend
    OutBuf = aiutp_buffer:new(16),
    WrapPacket1 = #aiutp_packet_wrap{
        transmissions = 1,
        need_resend = true,  %% Already marked
        payload = 1400
    },
    WrapPacket2 = #aiutp_packet_wrap{
        transmissions = 1,
        need_resend = false,
        payload = 1000
    },
    OutBuf1 = aiutp_buffer:append(WrapPacket1, OutBuf),
    OutBuf2 = aiutp_buffer:append(WrapPacket2, OutBuf1),
    Iter = aiutp_buffer:head(OutBuf2),

    {CurWindow, _} = aiutp_pcb_timeout:mark_need_resend(2, 10000, Iter, OutBuf2),

    %% Only second packet's payload should be deducted
    ?assertEqual(10000 - 1000, CurWindow).

mark_need_resend_skips_unsent_test() ->
    %% Should skip packets never sent (transmissions = 0)
    OutBuf = aiutp_buffer:new(16),
    WrapPacket = #aiutp_packet_wrap{
        transmissions = 0,  %% Never sent
        need_resend = false,
        payload = 1400
    },
    OutBuf1 = aiutp_buffer:append(WrapPacket, OutBuf),
    Iter = aiutp_buffer:head(OutBuf1),

    {CurWindow, _} = aiutp_pcb_timeout:mark_need_resend(1, 10000, Iter, OutBuf1),

    %% Window should be unchanged
    ?assertEqual(10000, CurWindow).

%%==============================================================================
%% Test: check_timeouts/1 state filtering
%%==============================================================================

check_timeouts_destroy_state_test() ->
    %% DESTROY state should return unchanged
    PCB = #aiutp_pcb{state = ?CS_DESTROY},
    Result = aiutp_pcb_timeout:check_timeouts(PCB),
    ?assertEqual(?CS_DESTROY, Result#aiutp_pcb.state).

check_timeouts_reset_state_test() ->
    %% RESET state should return unchanged
    PCB = #aiutp_pcb{state = ?CS_RESET},
    Result = aiutp_pcb_timeout:check_timeouts(PCB),
    ?assertEqual(?CS_RESET, Result#aiutp_pcb.state).

check_timeouts_idle_state_test() ->
    %% IDLE state should return unchanged (after time update)
    Now = aiutp_util:millisecond(),
    PCB = #aiutp_pcb{
        state = ?CS_IDLE,
        time = Now,
        outbuf = aiutp_buffer:new(16),
        inbuf = aiutp_buffer:new(16)
    },
    Result = aiutp_pcb_timeout:check_timeouts(PCB),
    ?assertEqual(?CS_IDLE, Result#aiutp_pcb.state).

%%==============================================================================
%% Test: Module exports
%%==============================================================================

exports_test() ->
    Exports = aiutp_pcb_timeout:module_info(exports),
    ?assert(lists:member({check_timeouts, 1}, Exports)),
    ?assert(lists:member({mark_need_resend, 4}, Exports)).

%%==============================================================================
%% Test: MTU 探测超时处理
%%==============================================================================

mtu_probe_timeout_test_() ->
    {"MTU 探测超时测试",
     [{"RTO 超时时处理 MTU 探测超时",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = #aiutp_pcb{
               state = ?CS_CONNECTED,
               time = Now,
               rto_timeout = Now - 100,  %% 已超时
               retransmit_timeout = 1000,
               cur_window_packets = 0,
               max_window = ?PACKET_SIZE * 4,
               outbuf = aiutp_buffer:new(16),
               inbuf = aiutp_buffer:new(16),
               outque = aiutp_queue:new(),
               %% 设置在途 MTU 探测
               mtu_probe_seq = 100,
               mtu_probe_size = 1000,
               mtu_floor = ?MTU_FLOOR_DEFAULT,
               mtu_ceiling = ?MTU_CEILING_DEFAULT,
               mtu_probe_failures = 0,
               %% 避免 keepalive 超时
               last_got_packet = Now,
               fin_sent = false,
               last_sent_packet = Now
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB),
           %% MTU 探测超时应该被处理
           ?assertEqual(0, Result#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(0, Result#aiutp_pcb.mtu_probe_size),
           ?assertEqual(999, Result#aiutp_pcb.mtu_ceiling),  %% probe_size - 1
           ?assertEqual(1, Result#aiutp_pcb.mtu_probe_failures)
       end},
      {"无 MTU 探测时不处理",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = #aiutp_pcb{
               state = ?CS_CONNECTED,
               time = Now,
               rto_timeout = Now - 100,
               retransmit_timeout = 1000,
               cur_window_packets = 0,
               max_window = ?PACKET_SIZE * 4,
               outbuf = aiutp_buffer:new(16),
               inbuf = aiutp_buffer:new(16),
               outque = aiutp_queue:new(),
               %% 无 MTU 探测
               mtu_probe_seq = 0,
               mtu_floor = ?MTU_FLOOR_DEFAULT,
               mtu_ceiling = ?MTU_CEILING_DEFAULT,
               last_got_packet = Now,
               fin_sent = false,
               last_sent_packet = Now
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB),
           %% ceiling 应该不变
           ?assertEqual(?MTU_CEILING_DEFAULT, Result#aiutp_pcb.mtu_ceiling)
       end},
      {"连续失败达到阈值时回退到 floor",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = #aiutp_pcb{
               state = ?CS_CONNECTED,
               time = Now,
               rto_timeout = Now - 100,
               retransmit_timeout = 1000,
               cur_window_packets = 0,
               max_window = ?PACKET_SIZE * 4,
               outbuf = aiutp_buffer:new(16),
               inbuf = aiutp_buffer:new(16),
               outque = aiutp_queue:new(),
               mtu_probe_seq = 100,
               mtu_probe_size = 1000,
               mtu_floor = 600,
               mtu_ceiling = 1200,
               mtu_probe_failures = ?MTU_PROBE_FAILURE_THRESHOLD - 1,
               last_got_packet = Now,
               fin_sent = false,
               last_sent_packet = Now
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB),
           %% 应该回退到 floor
           ?assertEqual(600, Result#aiutp_pcb.mtu_last),
           ?assertEqual(600, Result#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%%==============================================================================
%% Test: MTU 周期性重新探测
%%==============================================================================

mtu_restart_discovery_test_() ->
    {"MTU 周期性重新探测测试",
     [{"搜索完成且时间到时重新开始探测",
       fun() ->
           Now = aiutp_util:millisecond(),
           NowMicro = aiutp_util:microsecond(),
           PCB = #aiutp_pcb{
               state = ?CS_CONNECTED,
               time = Now,
               rto_timeout = 0,  %% 无 RTO 超时
               retransmit_timeout = 1000,
               cur_window_packets = 0,
               max_window = ?PACKET_SIZE * 4,
               outbuf = aiutp_buffer:new(16),
               inbuf = aiutp_buffer:new(16),
               outque = aiutp_queue:new(),
               %% MTU 搜索已完成
               mtu_floor = 1000,
               mtu_ceiling = 1000,
               mtu_probe_seq = 0,
               mtu_probe_size = 0,
               mtu_discover_time = NowMicro - 1000,  %% 时间已到
               mtu_last = 1000,
               %% 避免其他超时
               last_got_packet = Now,
               fin_sent = false,
               last_sent_packet = Now
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB),
           %% 应该重新开始探测
           ?assertEqual(?MTU_FLOOR_DEFAULT, Result#aiutp_pcb.mtu_floor),
           ?assertEqual(?MTU_CEILING_DEFAULT, Result#aiutp_pcb.mtu_ceiling)
       end},
      {"时间未到时不重新探测",
       fun() ->
           Now = aiutp_util:millisecond(),
           NowMicro = aiutp_util:microsecond(),
           PCB = #aiutp_pcb{
               state = ?CS_CONNECTED,
               time = Now,
               rto_timeout = 0,
               retransmit_timeout = 1000,
               cur_window_packets = 0,
               max_window = ?PACKET_SIZE * 4,
               outbuf = aiutp_buffer:new(16),
               inbuf = aiutp_buffer:new(16),
               outque = aiutp_queue:new(),
               mtu_floor = 1000,
               mtu_ceiling = 1000,
               mtu_probe_seq = 0,
               mtu_discover_time = NowMicro + ?MTU_PROBE_INTERVAL,  %% 时间未到
               last_got_packet = Now,
               fin_sent = false,
               last_sent_packet = Now
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB),
           %% 不应该重新探测
           ?assertEqual(1000, Result#aiutp_pcb.mtu_floor)
       end},
      {"搜索未完成时不重新探测",
       fun() ->
           Now = aiutp_util:millisecond(),
           NowMicro = aiutp_util:microsecond(),
           PCB = #aiutp_pcb{
               state = ?CS_CONNECTED,
               time = Now,
               rto_timeout = 0,
               retransmit_timeout = 1000,
               cur_window_packets = 0,
               max_window = ?PACKET_SIZE * 4,
               outbuf = aiutp_buffer:new(16),
               inbuf = aiutp_buffer:new(16),
               outque = aiutp_queue:new(),
               mtu_floor = ?MTU_FLOOR_DEFAULT,
               mtu_ceiling = ?MTU_CEILING_DEFAULT,  %% 搜索未完成
               mtu_probe_seq = 0,
               mtu_discover_time = NowMicro - 1000,
               last_got_packet = Now,
               fin_sent = false,
               last_sent_packet = Now
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB),
           %% 不应该重置
           ?assertEqual(?MTU_FLOOR_DEFAULT, Result#aiutp_pcb.mtu_floor),
           ?assertEqual(?MTU_CEILING_DEFAULT, Result#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%%==============================================================================
%% Test: 接收空闲超时
%%==============================================================================

recv_idle_timeout_test_() ->
    {"接收空闲超时测试",
     [{"纯接收方超过 RECV_IDLE_TIMEOUT 时触发超时",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               cur_window_packets = 0,  %% 没有发送待确认的数据
               last_got_packet = Now - ?RECV_IDLE_TIMEOUT - 1000  %% 超过超时时间
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 应该触发超时，状态变为 RESET
           ?assertEqual(?CS_RESET, Result#aiutp_pcb.state)
       end},
      {"有待确认发送数据时不触发接收空闲超时",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               cur_window_packets = 1,  %% 有发送待确认的数据
               last_got_packet = Now - ?RECV_IDLE_TIMEOUT - 1000  %% 超过超时时间
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 不应该触发接收空闲超时（由重传机制处理）
           ?assertNotEqual(?CS_RESET, Result#aiutp_pcb.state),
           ?assertNotEqual(?CS_DESTROY, Result#aiutp_pcb.state)
       end},
      {"未超过 RECV_IDLE_TIMEOUT 时不触发超时",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               cur_window_packets = 0,
               last_got_packet = Now - (?RECV_IDLE_TIMEOUT div 2)  %% 未超过超时时间
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 不应该触发超时
           ?assertEqual(?CS_CONNECTED, Result#aiutp_pcb.state)
       end},
      {"收到 FIN 但数据不完整时，FIN_DATA_TIMEOUT 后正确关闭",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               cur_window_packets = 0,
               got_fin = true,           %% 收到了 FIN
               got_fin_reached = false,  %% 但数据不完整
               eof_pkt = 100,
               ack_nr = 98,              %% 缺少数据包
               last_got_packet = Now - ?FIN_DATA_TIMEOUT - 1000  %% 超过 FIN 数据等待超时
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 应该触发超时
           ?assertEqual(?CS_RESET, Result#aiutp_pcb.state)
       end},
      {"收到 FIN 但数据不完整时，未超过 FIN_DATA_TIMEOUT 则不触发超时",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               cur_window_packets = 0,
               got_fin = true,           %% 收到了 FIN
               got_fin_reached = false,  %% 但数据不完整
               eof_pkt = 100,
               ack_nr = 98,              %% 缺少数据包
               last_got_packet = Now - (?FIN_DATA_TIMEOUT div 2)  %% 未超过 FIN 数据等待超时
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 不应该触发超时
           ?assertEqual(?CS_CONNECTED, Result#aiutp_pcb.state)
       end},
      {"正常接收完 FIN（got_fin_reached=true）时使用 RECV_IDLE_TIMEOUT",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               cur_window_packets = 0,
               got_fin = true,            %% 收到了 FIN
               got_fin_reached = true,    %% 所有数据都已收到
               eof_pkt = 100,
               ack_nr = 100,
               last_got_packet = Now - (?FIN_DATA_TIMEOUT + 1000)  %% 超过 FIN_DATA_TIMEOUT
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% FIN 已完整接收，使用 RECV_IDLE_TIMEOUT，不应该触发超时
           ?assertEqual(?CS_CONNECTED, Result#aiutp_pcb.state)
       end}
     ]}.

%%==============================================================================
%% Helper functions
%%==============================================================================

create_test_pcb(Now) ->
    #aiutp_pcb{
        state = ?CS_CONNECTED,
        time = Now,
        rto_timeout = 0,  %% 无 RTO 超时
        retransmit_timeout = 1000,
        cur_window_packets = 0,
        max_window = ?PACKET_SIZE * 4,
        outbuf = aiutp_buffer:new(16),
        inbuf = aiutp_buffer:new(16),
        outque = aiutp_queue:new(),
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_probe_seq = 0,
        last_got_packet = Now,
        fin_sent = false,
        last_sent_packet = Now
    }.

%%==============================================================================
%% Test: 零窗口探测
%%==============================================================================

zero_window_probe_test_() ->
    {"零窗口探测测试",
     [{"零窗口探测定时器到期时恢复窗口并发送探测",
       {setup,
        fun() -> setup_real_socket() end,
        fun(Socket) -> cleanup_socket(Socket) end,
        fun(Socket) ->
           fun() ->
               Now = aiutp_util:millisecond(),
               PCB = create_zero_window_test_pcb_with_socket(Now, Socket),
               PCB1 = PCB#aiutp_pcb{
                   max_window_user = 0,           %% 对端通告零窗口
                   zerowindow_time = Now - 100,   %% 探测定时器已到期
                   zerowindow_probes = 0          %% 第一次探测
               },
               Result = aiutp_pcb_timeout:check_timeouts(PCB1),
               %% 窗口应该恢复为 PACKET_SIZE
               ?assertEqual(?PACKET_SIZE, Result#aiutp_pcb.max_window_user),
               %% 探测计数应该增加
               ?assertEqual(1, Result#aiutp_pcb.zerowindow_probes),
               %% 下次探测时间应该设置
               ?assert(Result#aiutp_pcb.zerowindow_time > Now)
           end
        end}},
      {"零窗口探测使用指数退避（每次翻倍，最大10秒）",
       {setup,
        fun() -> setup_real_socket() end,
        fun(Socket) -> cleanup_socket(Socket) end,
        fun(Socket) ->
           fun() ->
               Now = aiutp_util:millisecond(),
               PCB = create_zero_window_test_pcb_with_socket(Now, Socket),
               PCB1 = PCB#aiutp_pcb{
                   max_window_user = 0,
                   zerowindow_time = Now - 100,
                   zerowindow_probes = 2  %% 已经探测过 2 次
               },
               Result = aiutp_pcb_timeout:check_timeouts(PCB1),
               %% 探测计数应该增加到 3
               ?assertEqual(3, Result#aiutp_pcb.zerowindow_probes),
               %% 下次探测间隔应该是 1000 * 2^2 = 4000ms
               ExpectedInterval = ?ZERO_WINDOW_PROBE_INTERVAL bsl 2,  %% 1000 * 4 = 4000
               ActualInterval = Result#aiutp_pcb.zerowindow_time - Now,
               %% 允许一些误差
               ?assert(abs(ActualInterval - ExpectedInterval) < 100)
           end
        end}},
      {"零窗口探测间隔不超过最大值",
       {setup,
        fun() -> setup_real_socket() end,
        fun(Socket) -> cleanup_socket(Socket) end,
        fun(Socket) ->
           fun() ->
               Now = aiutp_util:millisecond(),
               PCB = create_zero_window_test_pcb_with_socket(Now, Socket),
               PCB1 = PCB#aiutp_pcb{
                   max_window_user = 0,
                   zerowindow_time = Now - 100,
                   zerowindow_probes = 4  %% 已经探测过 4 次，理论间隔 1000*16=16000 > 10000
               },
               Result = aiutp_pcb_timeout:check_timeouts(PCB1),
               %% 探测计数应该增加到 5
               ?assertEqual(5, Result#aiutp_pcb.zerowindow_probes),
               %% 下次探测间隔应该被限制在 10000ms
               ActualInterval = Result#aiutp_pcb.zerowindow_time - Now,
               ?assert(abs(ActualInterval - ?ZERO_WINDOW_PROBE_MAX_INTERVAL) < 100)
           end
        end}},
      {"达到最大重试次数后恢复窗口并重置状态",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_zero_window_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               max_window_user = 0,
               zerowindow_time = Now - 100,
               zerowindow_probes = ?ZERO_WINDOW_PROBE_MAX_RETRIES  %% 已达到最大重试次数
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 窗口应该恢复
           ?assertEqual(?PACKET_SIZE, Result#aiutp_pcb.max_window_user),
           %% 探测状态应该重置
           ?assertEqual(0, Result#aiutp_pcb.zerowindow_time),
           ?assertEqual(0, Result#aiutp_pcb.zerowindow_probes)
       end},
      {"定时器未到期时不触发探测",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_zero_window_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               max_window_user = 0,
               zerowindow_time = Now + 5000,  %% 定时器还没到期
               zerowindow_probes = 1
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 窗口应该保持为 0
           ?assertEqual(0, Result#aiutp_pcb.max_window_user),
           %% 探测计数不变
           ?assertEqual(1, Result#aiutp_pcb.zerowindow_probes)
       end},
      {"窗口非零时不触发探测",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_zero_window_test_pcb(Now),
           PCB1 = PCB#aiutp_pcb{
               max_window_user = ?PACKET_SIZE,  %% 窗口不为零
               zerowindow_time = Now - 100,     %% 即使定时器到期
               zerowindow_probes = 1
           },
           Result = aiutp_pcb_timeout:check_timeouts(PCB1),
           %% 探测计数不变（因为窗口不为零）
           ?assertEqual(1, Result#aiutp_pcb.zerowindow_probes)
       end}
     ]}.

create_zero_window_test_pcb(Now) ->
    %% 用于不需要发送的测试（如达到最大重试次数、定时器未到期等）
    #aiutp_pcb{
        state = ?CS_CONNECTED,
        time = Now,
        socket = undefined,
        conn_id_recv = 12345,
        conn_id_send = 12346,
        rto_timeout = 0,
        retransmit_timeout = 1000,
        cur_window_packets = 0,
        max_window = ?PACKET_SIZE * 4,
        max_window_user = 0,
        outbuf = aiutp_buffer:new(16),
        inbuf = aiutp_buffer:new(16),
        outque = aiutp_queue:new(),
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_probe_seq = 0,
        last_got_packet = Now,
        fin_sent = false,
        last_sent_packet = Now,
        zerowindow_time = 0,
        zerowindow_probes = 0
    }.

create_zero_window_test_pcb_with_socket(Now, Socket) ->
    %% 用于需要实际发送探测包的测试
    #aiutp_pcb{
        state = ?CS_CONNECTED,
        time = Now,
        socket = Socket,
        conn_id_recv = 12345,
        conn_id_send = 12346,
        rto_timeout = 0,
        retransmit_timeout = 1000,
        cur_window_packets = 0,
        max_window = ?PACKET_SIZE * 4,
        max_window_user = 0,
        outbuf = aiutp_buffer:new(16),
        inbuf = aiutp_buffer:new(16),
        outque = aiutp_queue:new(),
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_probe_seq = 0,
        last_got_packet = Now,
        fin_sent = false,
        last_sent_packet = Now,
        zerowindow_time = 0,
        zerowindow_probes = 0
    }.

setup_real_socket() ->
    %% 创建真实的 UDP socket 用于测试
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Socket),
    {Socket, {{127,0,0,1}, Port}}.

cleanup_socket({Socket, _Addr}) ->
    gen_udp:close(Socket).
