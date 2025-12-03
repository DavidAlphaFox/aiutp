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
