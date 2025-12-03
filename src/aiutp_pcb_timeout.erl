%%------------------------------------------------------------------------------
%% @doc Timeout Handling for uTP Protocol Control Block
%%
%% This module handles various timeout conditions in the uTP protocol:
%% - Keepalive timeout detection
%% - RTT-based connection timeout
%% - Retransmission timeout and counting
%% - Packet resend marking
%%
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_timeout).
-include("aiutp.hrl").

-export([check_timeouts/1,
         mark_need_resend/4]).

%%------------------------------------------------------------------------------
%% @doc Main timeout check entry point
%%
%% Flushes pending packets and checks for various timeout conditions.
%% Should be called periodically to handle retransmission and keepalive.
%%
%% @param PCB Protocol control block
%% @returns Updated PCB after timeout processing
%% @end
%%------------------------------------------------------------------------------
-spec check_timeouts(#aiutp_pcb{}) -> #aiutp_pcb{}.
check_timeouts(#aiutp_pcb{state = State} = PCB)
  when State /= ?CS_DESTROY,
       State /= ?CS_RESET ->
    Now = aiutp_util:millisecond(),
    PCB0 = aiutp_net:flush_packets(PCB),
    check_timeouts_0(PCB0#aiutp_pcb{time = Now});
check_timeouts(PCB) -> PCB.

%%------------------------------------------------------------------------------
%% @private
%% @doc Filter out states that don't need timeout checking
%%------------------------------------------------------------------------------
check_timeouts_0(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_UNINITIALIZED;
       State == ?CS_IDLE;
       State == ?CS_RESET -> PCB;
check_timeouts_0(#aiutp_pcb{time = Now,
                            state = State,
                            zerowindow_time = ZeroWindowTime,
                            max_window_user = MaxWindowUser,
                            rto_timeout = RTOTimeout,
                            fin_sent = FinSent,
                            cur_window_packets = CurWindowPackets,
                            cur_window = CurWindow,
                            outbuf = OutBuf,
                            burst = Burst} = PCB) ->
    %% Handle zero window probe
    PCB0 =
        if (MaxWindowUser == 0) and
           (Now - ZeroWindowTime >= 0) ->
            PCB#aiutp_pcb{max_window_user = ?MIN_WINDOW_SIZE};
           true -> PCB
        end,

    %% Check RTO timeout or burst mode timeout
    {Continue, PCB1} =
        if (Burst == false) and
           (RTOTimeout > 0) and
           (Now - RTOTimeout >= 0) ->
            check_timeouts_2(check_timeouts_1(PCB0));
           (Burst == true) and
           (CurWindowPackets > 0) and
           (State == ?CS_CONNECTED) ->
            case check_timeouts_1(PCB0) of
                {true, _} ->
                    Iter = aiutp_buffer:head(OutBuf),
                    {CurWindow0, OutBuf0} = mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf),
                    {true, aiutp_net:flush_packets(PCB#aiutp_pcb{cur_window = CurWindow0, outbuf = OutBuf0})};
                {false, _} = W -> W
            end;
           true -> {true, PCB0}
        end,

    if Continue == true ->
        %% Handle keepalive
        PCBKeepAlive =
            if (FinSent == false) and
               ((State == ?CS_CONNECTED) or (State == ?CS_CONNECTED_FULL)) and
               (Now - PCB1#aiutp_pcb.last_sent_packet >= ?KEEPALIVE_INTERVAL) ->
                aiutp_net:send_keep_alive(PCB1);
               (FinSent == false) and
               (Burst == true) and
               (Now - PCB1#aiutp_pcb.last_sent_packet >= 5000) ->
                aiutp_net:send_keep_alive(PCB1);
               true -> PCB1
            end,

        %% Flush queue if no outstanding packets
        PCBFlush =
            if PCBKeepAlive#aiutp_pcb.cur_window_packets == 0 ->
                aiutp_net:flush_queue(PCBKeepAlive);
               true -> PCBKeepAlive
            end,

        %% Check if we can transition from CONNECTED_FULL to CONNECTED
        {ISFull, PCB2} = aiutp_net:is_full(-1, PCBFlush),
        if (State == ?CS_CONNECTED_FULL) and
           (ISFull == false) ->
            PCB2#aiutp_pcb{state = ?CS_CONNECTED};
           true -> PCB2
        end;
       true -> PCB1
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc Check for fatal timeout conditions
%%
%% Returns {false, PCB} if connection should be destroyed/reset,
%% {true, PCB} if connection is still valid.
%%------------------------------------------------------------------------------
check_timeouts_1(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
    {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};

check_timeouts_1(#aiutp_pcb{time = Now,
                            last_got_packet = LastGotPacket,
                            close_requested = CloseRequested} = PCB)
  when (LastGotPacket > 0), (Now - LastGotPacket > ?KEEPALIVE_INTERVAL * 2) ->
    logger:warning("Connection closed due to keepalive timeout: last_packet=~p", [LastGotPacket]),
    if CloseRequested == true -> {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB#aiutp_pcb{state = ?CS_RESET}}
    end;

check_timeouts_1(#aiutp_pcb{rtt = RTT, close_requested = CloseRequested} = PCB)
  when (RTT > 6000) ->
    logger:warning("Connection closed due to excessive RTT: rtt=~p", [RTT]),
    if CloseRequested == true -> {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB#aiutp_pcb{state = ?CS_RESET}}
    end;

check_timeouts_1(#aiutp_pcb{state = State,
                            close_requested = CloseRequested,
                            retransmit_count = RetransmitCount,
                            burst = false} = PCB)
  when (RetransmitCount >= 4);
       ((State == ?CS_SYN_SENT) and RetransmitCount > 2) ->
    logger:warning("Connection closed due to max retransmit count: count=~p", [RetransmitCount]),
    if CloseRequested == true -> {false, PCB#aiutp_pcb{state = ?CS_DESTROY}};
       true -> {false, PCB#aiutp_pcb{state = ?CS_RESET}}
    end;

check_timeouts_1(PCB) -> {true, PCB}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handle retransmission timeout
%%
%% Increases RTO, reduces window, and marks packets for resend.
%%------------------------------------------------------------------------------
check_timeouts_2({false, _} = W) -> W;
check_timeouts_2({true, #aiutp_pcb{time = Now,
                                   retransmit_timeout = RetransmitTimeout,
                                   cur_window_packets = CurWindowPackets,
                                   cur_window = CurWindow,
                                   max_window = MaxWindow,
                                   outbuf = OutBuf,
                                   seq_nr = SeqNR,
                                   retransmit_count = RetransmitCount} = PCB}) ->
    %% Exponential backoff: RTO *= 1.5
    NewTimeout = RetransmitTimeout * 1.5,

    PCB0 =
        if (CurWindowPackets == 0) and (MaxWindow > ?PACKET_SIZE) ->
            %% No outstanding packets, just decay window
            PCB#aiutp_pcb{
                retransmit_timeout = NewTimeout,
                rto_timeout = Now + NewTimeout,
                duplicate_ack = 0,
                max_window = erlang:max((MaxWindow * 2 div 3), ?MIN_WINDOW_SIZE)
            };
           true ->
            %% Outstanding packets, reduce window and enable slow start
            PCB#aiutp_pcb{
                retransmit_timeout = NewTimeout,
                rto_timeout = Now + NewTimeout,
                duplicate_ack = 0,
                max_window = erlang:max((MaxWindow div 2), ?MIN_WINDOW_SIZE),
                slow_start = true
            }
        end,

    if CurWindowPackets > 0 ->
        %% Mark packets for resend and send first packet
        Iter = aiutp_buffer:head(OutBuf),
        {CurWindow0, OutBuf0} = mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf),
        PCB1 = PCB0#aiutp_pcb{
            cur_window = CurWindow0,
            outbuf = OutBuf0,
            retransmit_count = RetransmitCount + 1,
            fast_timeout = true,
            timeout_seq_nr = SeqNR
        },
        {true, aiutp_net:send_packet(aiutp_buffer:head(OutBuf0), PCB1)};
       true -> {true, PCB0}
    end.

%%------------------------------------------------------------------------------
%% @doc Mark packets in the outgoing buffer as needing resend
%%
%% Iterates through the buffer and marks unacked packets for retransmission.
%% Adjusts current window to account for resent data.
%%
%% @param CurWindowPackets Number of packets in flight
%% @param CurWindow Current window size in bytes
%% @param Iter Current buffer iterator (-1 means end)
%% @param OutBuf Output buffer
%% @returns {NewCurWindow, NewOutBuf}
%% @end
%%------------------------------------------------------------------------------
-spec mark_need_resend(integer(), integer(), integer(), term()) -> {integer(), term()}.
mark_need_resend(_, CurWindow, -1, OutBuf) -> {CurWindow, OutBuf};
mark_need_resend(0, CurWindow, _, OutBuf) -> {CurWindow, OutBuf};
mark_need_resend(CurWindowPackets, CurWindow, Iter, OutBuf) ->
    Next = aiutp_buffer:next(Iter, OutBuf),
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    #aiutp_packet_wrap{
        transmissions = Transmissions,
        need_resend = NeedResend,
        payload = Payload
    } = WrapPacket,

    if (NeedResend == true) or (Transmissions == 0) ->
        %% Already marked or never sent
        mark_need_resend(CurWindowPackets - 1, CurWindow, Next, OutBuf);
       true ->
        %% Mark for resend and adjust window
        WrapPacket0 = WrapPacket#aiutp_packet_wrap{need_resend = true},
        OutBuf0 = aiutp_buffer:replace(Iter, WrapPacket0, OutBuf),
        mark_need_resend(CurWindowPackets - 1, CurWindow - Payload, Next, OutBuf0)
    end.
