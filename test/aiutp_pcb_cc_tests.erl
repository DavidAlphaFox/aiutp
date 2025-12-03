%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_pcb_cc module (Congestion Control)
%%
%% Tests LEDBAT congestion control algorithm implementation.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_cc_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% Test: caculate_acked_bytes/4
%%==============================================================================

caculate_acked_bytes_empty_test() ->
    %% Empty lists should return initial accumulator
    Result = aiutp_pcb_cc:caculate_acked_bytes({0, ?RTT_MAX}, 1000, [], []),
    ?assertEqual({0, ?RTT_MAX}, Result).

caculate_acked_bytes_single_packet_test() ->
    %% Single packet with payload
    Now = aiutp_util:millisecond(),
    WrapPacket = #aiutp_packet_wrap{
        payload = 1400,
        time_sent = Now - 100
    },
    {Bytes, RTT} = aiutp_pcb_cc:caculate_acked_bytes({0, ?RTT_MAX}, Now, [WrapPacket], []),
    ?assertEqual(1400, Bytes),
    ?assert(RTT =< 100).

caculate_acked_bytes_multiple_packets_test() ->
    %% Multiple packets accumulate bytes
    Now = aiutp_util:millisecond(),
    Packets = [
        #aiutp_packet_wrap{payload = 1400, time_sent = Now - 50},
        #aiutp_packet_wrap{payload = 1400, time_sent = Now - 100},
        #aiutp_packet_wrap{payload = 500, time_sent = Now - 75}
    ],
    {Bytes, RTT} = aiutp_pcb_cc:caculate_acked_bytes({0, ?RTT_MAX}, Now, Packets, []),
    ?assertEqual(3300, Bytes),
    ?assert(RTT =< 50).  %% Min RTT from all packets

caculate_acked_bytes_with_sack_test() ->
    %% SACK'd packets also contribute
    Now = aiutp_util:millisecond(),
    AckedPackets = [#aiutp_packet_wrap{payload = 1400, time_sent = Now - 50}],
    SAckedPackets = [#aiutp_packet_wrap{payload = 1400, time_sent = Now - 30}],
    {Bytes, RTT} = aiutp_pcb_cc:caculate_acked_bytes({0, ?RTT_MAX}, Now, AckedPackets, SAckedPackets),
    ?assertEqual(2800, Bytes),
    ?assert(RTT =< 30).  %% Min from both lists

%%==============================================================================
%% Test: ack_packet/3
%%==============================================================================

ack_packet_first_transmission_test() ->
    %% First transmission should update RTT
    MicroNow = aiutp_util:microsecond(),
    Now = aiutp_util:millisecond(),
    TimeSent = MicroNow - 50000,  %% 50ms ago
    WrapPacket = #aiutp_packet_wrap{
        transmissions = 1,
        time_sent = TimeSent,
        need_resend = false,
        payload = 1400
    },
    RTTHist = aiutp_delay:new(Now),
    Acc = {Now, 10000, 0, 1000, 0, RTTHist},  %% Initial RTT = 0
    {_, CurWindow, RTT, _RTO, _RTTVar, _} = aiutp_pcb_cc:ack_packet(MicroNow, WrapPacket, Acc),

    %% RTT should be calculated
    ?assert(RTT > 0),
    %% Window should decrease by payload
    ?assertEqual(10000 - 1400, CurWindow).

ack_packet_retransmission_test() ->
    %% Retransmitted packet should not update RTT (Karn's algorithm)
    MicroNow = aiutp_util:microsecond(),
    Now = aiutp_util:millisecond(),
    WrapPacket = #aiutp_packet_wrap{
        transmissions = 2,  %% Retransmitted
        time_sent = MicroNow - 50000,
        need_resend = false,
        payload = 1400
    },
    RTTHist = aiutp_delay:new(Now),
    Acc = {Now, 10000, 100, 1000, 25, RTTHist},
    {_, _, RTT, RTO, RTTVar, _} = aiutp_pcb_cc:ack_packet(MicroNow, WrapPacket, Acc),

    %% RTT values should remain unchanged
    ?assertEqual(100, RTT),
    ?assertEqual(1000, RTO),
    ?assertEqual(25, RTTVar).

ack_packet_resent_no_window_decrease_test() ->
    %% Resent packet should not decrease window
    MicroNow = aiutp_util:microsecond(),
    Now = aiutp_util:millisecond(),
    WrapPacket = #aiutp_packet_wrap{
        transmissions = 1,
        time_sent = MicroNow - 50000,
        need_resend = true,  %% Was marked for resend
        payload = 1400
    },
    RTTHist = aiutp_delay:new(Now),
    Acc = {Now, 10000, 100, 1000, 25, RTTHist},
    {_, CurWindow, _, _, _, _} = aiutp_pcb_cc:ack_packet(MicroNow, WrapPacket, Acc),

    %% Window should NOT decrease for resent packets
    ?assertEqual(10000, CurWindow).

%%==============================================================================
%% Test: maybe_decay_win/1
%%==============================================================================

maybe_decay_win_too_soon_test() ->
    %% Should not decay if called too soon (less than MAX_WINDOW_DECAY = 100ms)
    Now = aiutp_util:millisecond(),
    PCB = #aiutp_pcb{
        time = Now,
        max_window = 10000,
        last_rwin_decay = Now - 50  %% Only 50ms ago, less than MAX_WINDOW_DECAY
    },
    Result = aiutp_pcb_cc:maybe_decay_win(PCB),
    ?assertEqual(10000, Result#aiutp_pcb.max_window).

maybe_decay_win_after_interval_test() ->
    %% Should decay after MAX_WINDOW_DECAY interval
    Now = aiutp_util:millisecond(),
    PCB = #aiutp_pcb{
        time = Now,
        max_window = 10000,
        slow_start = true,
        ssthresh = 50000,
        last_rwin_decay = Now - ?MAX_WINDOW_DECAY - 100
    },
    Result = aiutp_pcb_cc:maybe_decay_win(PCB),

    %% Window should be reduced by 20%
    ?assertEqual(8000, Result#aiutp_pcb.max_window),
    ?assertEqual(8000, Result#aiutp_pcb.ssthresh),
    ?assertEqual(false, Result#aiutp_pcb.slow_start),
    ?assertEqual(Now, Result#aiutp_pcb.last_rwin_decay).

maybe_decay_win_minimum_test() ->
    %% Should not go below MIN_WINDOW_SIZE
    Now = aiutp_util:millisecond(),
    PCB = #aiutp_pcb{
        time = Now,
        max_window = ?MIN_WINDOW_SIZE + 100,
        last_rwin_decay = Now - ?MAX_WINDOW_DECAY - 100
    },
    Result = aiutp_pcb_cc:maybe_decay_win(PCB),
    ?assert(Result#aiutp_pcb.max_window >= ?MIN_WINDOW_SIZE).

%%==============================================================================
%% Test: Module exports
%%==============================================================================

exports_test() ->
    Exports = aiutp_pcb_cc:module_info(exports),
    ?assert(lists:member({cc_control, 4}, Exports)),
    ?assert(lists:member({maybe_decay_win, 1}, Exports)),
    ?assert(lists:member({ack_packet, 3}, Exports)),
    ?assert(lists:member({caculate_acked_bytes, 4}, Exports)),
    ?assert(lists:member({selective_ack_packet, 3}, Exports)).
