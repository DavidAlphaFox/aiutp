%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_net module
%%
%% Tests network sending logic and error handling.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_net_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% Test: Window size calculation
%%==============================================================================

window_size_empty_buffer_test() ->
    %% Empty buffer should have full window available
    Buffer = aiutp_buffer:new(100),
    WindowSize = aiutp_net:window_size(0, Buffer),
    Expected = aiutp_buffer:unused(Buffer) * ?PACKET_SIZE,
    ?assertEqual(Expected, WindowSize).

window_size_partial_buffer_test() ->
    %% Partially filled buffer
    Buffer0 = aiutp_buffer:new(100),
    Buffer1 = aiutp_buffer:append(test_data, Buffer0),
    Buffer2 = aiutp_buffer:append(test_data, Buffer1),

    WindowSize = aiutp_net:window_size(0, Buffer2),
    Expected = aiutp_buffer:unused(Buffer2) * ?PACKET_SIZE,
    ?assertEqual(Expected, WindowSize).

%%==============================================================================
%% Test: is_full checks
%%==============================================================================

is_full_empty_pcb_test() ->
    %% A PCB with no packets should not be full
    PCB = create_test_pcb(),
    {IsFull, _PCB1} = aiutp_net:is_full(-1, PCB),
    ?assertEqual(false, IsFull).

is_full_respects_max_window_test() ->
    %% When cur_window exceeds max_window, should be full
    PCB = create_test_pcb(),
    PCB1 = PCB#aiutp_pcb{
        max_window = 1000,
        max_window_user = 2000,
        cur_window = 1500,
        burst = false
    },
    {IsFull, _PCB2} = aiutp_net:is_full(500, PCB1),
    ?assertEqual(true, IsFull).

%%==============================================================================
%% Test: Schedule ACK
%%==============================================================================

schedule_ack_when_ida_false_test() ->
    %% When ida is false, schedule_ack should return PCB unchanged
    PCB = create_test_pcb(),
    PCB1 = PCB#aiutp_pcb{ida = false},
    Result = aiutp_net:schedule_ack(PCB1),
    ?assertEqual(false, Result#aiutp_pcb.ida).

%%==============================================================================
%% Helper functions
%%==============================================================================

create_test_pcb() ->
    Now = aiutp_util:millisecond(),
    #aiutp_pcb{
        time = Now,
        state = ?CS_CONNECTED,
        conn_id_recv = 12345,
        conn_id_send = 12346,
        max_window = ?PACKET_SIZE * 10,
        max_window_user = ?PACKET_SIZE * 255,
        cur_window = 0,
        cur_window_packets = 0,
        inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        outbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        inque = aiutp_queue:new(),
        outque = aiutp_queue:new(),
        our_hist = aiutp_delay:new(Now),
        their_hist = aiutp_delay:new(Now),
        rtt_hist = aiutp_delay:new(Now),
        burst = true,
        ida = false
    }.
