%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_pcb module - state machine and packet processing
%%
%% Tests PCB state transitions, packet dispatch and edge case handling.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% Test: CS_SYN_RECV duplicate SYN handling
%%==============================================================================

syn_recv_duplicate_syn_test_() ->
    {"CS_SYN_RECV 重复 SYN 处理测试",
     [
      {"SYN seq_nr 不匹配应发送 RESET 并进入 CS_DESTROY",
       fun() ->
           PCB = create_syn_recv_pcb(100),
           SYN = create_syn_packet(200),  %% seq_nr 不匹配 ack_nr
           TS = aiutp_util:millisecond(),
           Result = aiutp_pcb:process_incoming({SYN, TS}, PCB),
           %% 应该进入 CS_DESTROY 状态
           ?assertEqual(?CS_DESTROY, Result#aiutp_pcb.state)
       end},
      {"SYN seq_nr 不匹配时 last_got_packet 应更新",
       fun() ->
           Now = aiutp_util:millisecond(),
           PCB = create_syn_recv_pcb(100),
           PCB1 = PCB#aiutp_pcb{last_got_packet = Now - 1000},  %% 1秒前
           SYN = create_syn_packet(200),  %% seq_nr 不匹配 ack_nr
           TS = aiutp_util:millisecond(),
           Result = aiutp_pcb:process_incoming({SYN, TS}, PCB1),
           %% last_got_packet 应该被更新为接近 Now
           ?assert(Result#aiutp_pcb.last_got_packet > PCB1#aiutp_pcb.last_got_packet)
       end}
     ]}.

%%==============================================================================
%% Test: RESET packet validation
%%==============================================================================

reset_packet_validation_test_() ->
    {"RESET 包验证测试",
     [
      {"有效 RESET（conn_id 和 ack_nr 都有效）应转换为 CS_RESET",
       fun() ->
           PCB = create_connected_pcb(),
           RESET = create_reset_packet(PCB#aiutp_pcb.conn_id_recv,
                                       PCB#aiutp_pcb.seq_nr - 1),  %% 有效 ack_nr
           TS = aiutp_util:millisecond(),
           Result = aiutp_pcb:process_incoming({RESET, TS}, PCB),
           ?assertEqual(?CS_RESET, Result#aiutp_pcb.state)
       end},
      {"无效 RESET（conn_id 错误）应被忽略",
       fun() ->
           PCB = create_connected_pcb(),
           RESET = create_reset_packet(99999,  %% 错误的 conn_id
                                       PCB#aiutp_pcb.seq_nr - 1),
           TS = aiutp_util:millisecond(),
           Result = aiutp_pcb:process_incoming({RESET, TS}, PCB),
           %% 状态应保持 CS_CONNECTED
           ?assertEqual(?CS_CONNECTED, Result#aiutp_pcb.state)
       end},
      {"无效 RESET（ack_nr 超出范围）应被忽略",
       fun() ->
           PCB = create_connected_pcb(),
           %% ack_nr 远超过 seq_nr，超出有效范围
           InvalidAckNR = aiutp_util:bit16(PCB#aiutp_pcb.seq_nr + 100),
           RESET = create_reset_packet(PCB#aiutp_pcb.conn_id_recv, InvalidAckNR),
           TS = aiutp_util:millisecond(),
           Result = aiutp_pcb:process_incoming({RESET, TS}, PCB),
           %% 状态应保持 CS_CONNECTED
           ?assertEqual(?CS_CONNECTED, Result#aiutp_pcb.state)
       end}
     ]}.

%%==============================================================================
%% Helper functions
%%==============================================================================

create_connected_pcb() ->
    Now = aiutp_util:millisecond(),
    #aiutp_pcb{
        state = ?CS_CONNECTED,
        time = Now,
        ack_nr = 100,
        seq_nr = 1000,
        conn_id_send = 12345,
        conn_id_recv = 12346,
        last_got_packet = Now - 100,
        cur_window_packets = 5,  %% 有一些待确认的包
        max_window = ?PACKET_SIZE * 4,
        outbuf = aiutp_buffer:new(16),
        inbuf = aiutp_buffer:new(16),
        outque = aiutp_queue:new(),
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_last = ?PACKET_SIZE
    }.

create_reset_packet(ConnId, AckNR) ->
    #aiutp_packet{
        type = ?ST_RESET,
        conn_id = ConnId,
        tv_usec = aiutp_util:millisecond() * 1000,
        reply_micro = 0,
        wnd = 0,
        seq_nr = 0,
        ack_nr = AckNR,
        extension = []
    }.

create_syn_recv_pcb(AckNR) ->
    Now = aiutp_util:millisecond(),
    #aiutp_pcb{
        state = ?CS_SYN_RECV,
        time = Now,
        ack_nr = AckNR,
        seq_nr = 1000,
        conn_id_send = 12345,
        conn_id_recv = 12346,
        last_got_packet = Now - 100,
        cur_window_packets = 0,
        max_window = ?PACKET_SIZE * 4,
        outbuf = aiutp_buffer:new(16),
        inbuf = aiutp_buffer:new(16),
        outque = aiutp_queue:new(),
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_last = ?PACKET_SIZE
    }.

create_syn_packet(SeqNR) ->
    #aiutp_packet{
        type = ?ST_SYN,
        conn_id = 12346,
        tv_usec = aiutp_util:millisecond() * 1000,
        reply_micro = 0,
        wnd = 65535,
        seq_nr = SeqNR,
        ack_nr = 0,
        extension = []
    }.
