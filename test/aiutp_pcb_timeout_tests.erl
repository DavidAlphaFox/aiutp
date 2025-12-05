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
