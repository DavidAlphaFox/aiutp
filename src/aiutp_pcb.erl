%%------------------------------------------------------------------------------
%% @doc Protocol Control Block for uTP connections
%%
%% This module manages the core state and operations of a uTP connection,
%% including connection establishment, data transfer, and teardown.
%%
%% The PCB maintains all connection state including:
%% - Connection identifiers (recv/send IDs)
%% - Sequence and acknowledgment numbers
%% - Send/receive buffers
%% - RTT/RTO estimates
%% - Congestion window parameters
%%
%% Related modules:
%% - aiutp_pcb_cc: Congestion control (LEDBAT)
%% - aiutp_pcb_timeout: Timeout handling and retransmission
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb).
-include("aiutp.hrl").

-export([new/3,
         state/1,
         process/2,
         check_timeouts/1,
         write/2,
         close/1,
         read/1,
         connect/2,
         accept/2,
         closed/1,
         flush/1]).

%%------------------------------------------------------------------------------
%% @doc Create a new Protocol Control Block
%%
%% Initializes a new PCB with default values for a uTP connection.
%%
%% @param ConnIdRecv Connection ID for receiving
%% @param ConnIdSend Connection ID for sending
%% @param Socket The UDP socket reference
%% @returns New #aiutp_pcb{} record
%% @end
%%------------------------------------------------------------------------------
-spec new(integer(), integer(), term()) -> #aiutp_pcb{}.
new(ConnIdRecv, ConnIdSend, Socket) ->
    CurMilli = aiutp_util:millisecond(),
    #aiutp_pcb{
        time = CurMilli,
        state = ?CS_IDLE,
        socket = Socket,
        conn_id_send = ConnIdSend,
        conn_id_recv = ConnIdRecv,
        last_got_packet = CurMilli,
        last_sent_packet = CurMilli,
        last_measured_delay = CurMilli + 16#70000000,
        average_sample_time = CurMilli + 5000,
        last_rwin_decay = CurMilli - ?MAX_WINDOW_DECAY,
        our_hist = aiutp_delay:new(CurMilli),
        their_hist = aiutp_delay:new(CurMilli),
        rtt_hist = aiutp_delay:new(CurMilli),
        max_window = ?PACKET_SIZE,
        inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        outbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        inque = aiutp_queue:new(),
        outque = aiutp_queue:new()
    }.

%%------------------------------------------------------------------------------
%% @doc Get the current connection state
%% @end
%%------------------------------------------------------------------------------
-spec state(#aiutp_pcb{}) -> integer().
state(#aiutp_pcb{state = State}) -> State.

%%------------------------------------------------------------------------------
%% @doc Check if connection is closed and return reason
%%
%% @returns {closed, Reason} | not_closed
%% where Reason is: normal | reset | timeout | crash
%% @end
%%------------------------------------------------------------------------------
-spec closed(#aiutp_pcb{}) -> {closed, atom()} | not_closed.
closed(#aiutp_pcb{state = State})
  when State == ?CS_RESET -> {closed, reset};
closed(#aiutp_pcb{state = State,
                  fin_sent = FinSent,
                  fin_sent_acked = FinSentAcked,
                  got_fin = GotFin,
                  got_fin_reached = GotFinReached,
                  cur_window_packets = CurWindowPackets})
  when State == ?CS_DESTROY ->
    if (FinSent and FinSentAcked) or
       (GotFin and GotFinReached) -> {closed, normal};
       FinSent and CurWindowPackets == 1 -> {closed, normal};
       (FinSent == false) and
       (GotFin == false) -> {closed, timeout};
       true -> {closed, crash}
    end;
closed(#aiutp_pcb{got_fin = GotFin, got_fin_reached = GotFinReached}) ->
    if (GotFin and GotFinReached) -> {closed, normal};
       true -> not_closed
    end.

%%------------------------------------------------------------------------------
%% @doc Process an incoming packet
%%
%% Entry point for packet processing. Schedules ACK after processing.
%%
%% @param {Packet, Timestamp} Incoming packet with receive timestamp
%% @param PCB Protocol control block
%% @returns Updated PCB
%% @end
%%------------------------------------------------------------------------------
-spec process({#aiutp_packet{}, integer()}, #aiutp_pcb{}) -> #aiutp_pcb{}.
process({Packet, TS}, PCB) ->
    aiutp_net:schedule_ack(
        process_by_type(Packet#aiutp_packet.type, Packet,
                        PCB#aiutp_pcb{recv_time = TS})).

%% @private Process packet by type - filter destroyed/reset states
process_by_type(_, _, #aiutp_pcb{state = State} = PCB)
  when (State == ?CS_DESTROY);
       (State == ?CS_RESET) ->
    PCB;

%% @private Handle RESET packet
process_by_type(?ST_RESET,
                #aiutp_packet{conn_id = ConnId},
                #aiutp_pcb{conn_id_send = ConnIdSend,
                           conn_id_recv = ConnIdRecv,
                           close_requested = CloseRequested} = PCB) ->
    if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
        if CloseRequested == true -> PCB#aiutp_pcb{state = ?CS_DESTROY};
           true -> PCB#aiutp_pcb{state = ?CS_RESET}
        end;
       true -> PCB
    end;

%% @private Handle SYN packet (new connection)
process_by_type(?ST_SYN,
                #aiutp_packet{seq_nr = AckNR},
                #aiutp_pcb{state = ?CS_IDLE} = PCB) ->
    SeqNR = aiutp_util:bit16_random(),
    PCB0 = PCB#aiutp_pcb{
        state = ?CS_SYN_RECV,
        ack_nr = AckNR,
        seq_nr = SeqNR,
        fast_resend_seq_nr = SeqNR,
        last_got_packet = aiutp_util:millisecond()
    },
    aiutp_net:send_ack(PCB0);

%% @private Handle duplicate SYN (retransmit ACK)
process_by_type(?ST_SYN,
                #aiutp_packet{seq_nr = AckNR},
                #aiutp_pcb{state = ?CS_SYN_RECV, ack_nr = AckNR} = PCB) ->
    PCB0 = PCB#aiutp_pcb{last_got_packet = aiutp_util:millisecond()},
    aiutp_net:send_ack(PCB0);

%% @private Handle all other packet types (non-RESET, non-SYN)
process_by_type(_,
                #aiutp_packet{type = PktType, ack_nr = PktAckNR} = Packet,
                #aiutp_pcb{state = State, seq_nr = SeqNR,
                           cur_window_packets = CurWindowPackets} = PCB) ->
    %% Calculate permissible ACK range for validation
    CurrWindow = erlang:max(CurWindowPackets + ?ACK_NR_ALLOWED_WINDOW, ?ACK_NR_ALLOWED_WINDOW),
    MaxSeqNR = aiutp_util:bit16(SeqNR - 1),
    MinSeqNR = aiutp_util:bit16(SeqNR - 1 - CurrWindow),

    %% Validate ACK number to detect spoofed/malicious packets
    if ((PktType /= ?ST_SYN) or (State /= ?CS_SYN_RECV)) and
       (?WRAPPING_DIFF_16(MaxSeqNR, PktAckNR) < 0) or
       (?WRAPPING_DIFF_16(PktAckNR, MinSeqNR) < 0) ->
        %% Invalid ACK - ignore packet (possible spoofed address or attack)
        PCB;
       true ->
        process_packet(Packet, PCB)
    end.

%%------------------------------------------------------------------------------
%% Packet Processing Pipeline
%%
%% The packet processing is split into stages for clarity:
%% 1. process_packet - Initialize, check reorder distance
%% 2. process_packet_1 - Handle duplicate ACKs
%% 3. process_packet_2 - Flow control, ACK processing, SACK
%% 4. process_packet_3 - State transitions
%% 5. process_packet_4 - FIN handling, data reception
%%------------------------------------------------------------------------------

%% @private Initialize processing and check reorder buffer range
process_packet(#aiutp_packet{type = PktType, seq_nr = PktSeqNR} = Packet,
               #aiutp_pcb{state = State} = PCB) ->
    Now = aiutp_util:millisecond(),
    PCB0 =
        if State == ?CS_SYN_SENT ->
            %% SYN-ACK received: initialize ack_nr
            PCB#aiutp_pcb{
                ack_nr = aiutp_util:bit16(PktSeqNR - 1),
                last_got_packet = Now,
                time = Now
            };
           true ->
            PCB#aiutp_pcb{last_got_packet = Now, time = Now}
        end,

    %% Check if packet is within reorder buffer range
    NextPktAckNR = aiutp_util:bit16(PCB0#aiutp_pcb.ack_nr + 1),
    SeqDistance = aiutp_util:bit16(PktSeqNR - NextPktAckNR),

    if SeqDistance >= ?REORDER_BUFFER_MAX_SIZE ->
        %% Packet too far out of sequence
        if (SeqDistance >= (?SEQ_NR_MASK + 1 - ?REORDER_BUFFER_MAX_SIZE)) and
           (PktType /= ?ST_STATE) ->
            %% Old packet, schedule immediate ACK
            PCB0#aiutp_pcb{ida = true};
           true -> PCB0
        end;
       true ->
        process_packet_1(Packet, PCB0)
    end.

%% @private Handle duplicate ACK detection for fast retransmit
process_packet_1(#aiutp_packet{type = PktType, ack_nr = PktAckNR} = Packet,
                 #aiutp_pcb{cur_window_packets = CurWindowPackets,
                            duplicate_ack = DuplicateAck,
                            seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0 ->
    Seq = aiutp_util:bit16(SeqNR - CurWindowPackets - 1),
    if (PktAckNR == Seq) and (PktType == ?ST_STATE) ->
        %% Duplicate ACK received
        if DuplicateAck + 1 == ?DUPLICATE_ACKS_BEFORE_RESEND ->
            %% Trigger fast retransmit
            PCB0 = aiutp_net:send_packet(
                       aiutp_buffer:head(PCB#aiutp_pcb.outbuf),
                       PCB#aiutp_pcb{duplicate_ack = 0}),
            process_packet_2(Packet, PCB0);
           true ->
            process_packet_2(Packet, PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1})
        end;
       true ->
        process_packet_2(Packet, PCB#aiutp_pcb{duplicate_ack = 0})
    end;
process_packet_1(Packet, PCB) ->
    process_packet_2(Packet, PCB).

%% @private Flow control, ACK processing, congestion control, SACK
process_packet_2(#aiutp_packet{type = PktType, ack_nr = PktAckNR,
                               wnd = PktMaxWindowUser} = Packet,
                 #aiutp_pcb{state = State,
                            time = Now,
                            fast_resend_seq_nr = FastResendSeqNR,
                            fast_timeout = FastTimeout,
                            zerowindow_time = ZeroWindowTime,
                            fin_sent = FinSent,
                            close_requested = CloseRequested,
                            fin_sent_acked = FinSentAcked,
                            recv_time = RecvTime} = PCB) ->
    %% Pick acknowledged packets
    {AckedPackets, SAckedPackets, PCB0} = aiutp_tx:pick_acked(Packet, PCB),

    %% Calculate acked bytes and minimum RTT
    {AckedBytes, MinRTT} = aiutp_pcb_cc:caculate_acked_bytes(
                               {0, ?RTT_MAX}, RecvTime, AckedPackets, SAckedPackets),

    %% Calculate actual delay
    {ActualDelay, PCB1} = aiutp_rtt:caculate_delay(Now, RecvTime, Packet, PCB0),

    %% Update delay history
    OurHist = PCB1#aiutp_pcb.our_hist,
    OurHistValue = aiutp_delay:value(OurHist),
    OurHist0 =
        if OurHistValue > MinRTT ->
            aiutp_delay:shift(aiutp_util:bit32(OurHistValue - MinRTT), OurHist);
           true -> OurHist
        end,

    %% Apply congestion control if we got ACKs
    PCB2 =
        if (ActualDelay /= 0) and (AckedBytes > 0) ->
            aiutp_pcb_cc:cc_control(Now, AckedBytes, MinRTT,
                                    PCB1#aiutp_pcb{our_hist = OurHist0});
           true ->
            PCB1#aiutp_pcb{our_hist = OurHist0}
        end,

    %% Handle zero window
    ZeroWindowTime0 =
        if PktMaxWindowUser == 0 -> Now + 15000;
           true -> ZeroWindowTime
        end,

    %% State transitions
    State0 =
        if (PktType == ?ST_DATA) and (State == ?CS_SYN_RECV) -> ?CS_CONNECTED;
           true -> State
        end,

    {State1, FinSentAcked0} =
        if (PktType == ?ST_STATE) and (State0 == ?CS_SYN_SENT) ->
            {?CS_CONNECTED, false};
           (FinSent == true) and (PCB2#aiutp_pcb.cur_window_packets == 0) ->
            if CloseRequested -> {?CS_DESTROY, true};
               true -> {State0, true}
            end;
           true -> {State0, FinSentAcked}
        end,

    %% Update fast resend sequence number
    PktAckNR0 = aiutp_util:bit16(PktAckNR + 1),
    FastResendSeqNR0 =
        if ?WRAPPING_DIFF_16(FastResendSeqNR, PktAckNR0) < 0 -> PktAckNR0;
           true -> FastResendSeqNR
        end,

    %% Process ACKed packets for RTT
    {_, CurWindow0, RTT0, RTO0, RTTVar0, RTTHist0} =
        lists:foldr(
            fun(I, AccPCB) -> aiutp_pcb_cc:ack_packet(RecvTime, I, AccPCB) end,
            {Now, PCB2#aiutp_pcb.cur_window, PCB2#aiutp_pcb.rtt, PCB2#aiutp_pcb.rto,
             PCB2#aiutp_pcb.rtt_var, PCB2#aiutp_pcb.rtt_hist},
            AckedPackets),

    Now0 = aiutp_util:millisecond(),
    PCB3 = PCB2#aiutp_pcb{
        max_window_user = PktMaxWindowUser,
        state = State1,
        fin_sent_acked = FinSentAcked0,
        fast_resend_seq_nr = FastResendSeqNR0,
        zerowindow_time = ZeroWindowTime0,
        cur_window = CurWindow0,
        rtt = RTT0,
        rtt_var = RTTVar0,
        rtt_hist = RTTHist0,
        rto = RTO0,
        retransmit_count = 0,
        retransmit_timeout = RTO0,
        rto_timeout = RTO0 + Now0
    },

    %% Send next packet if only one in flight
    PCB4 =
        if PCB3#aiutp_pcb.cur_window_packets == 1 ->
            aiutp_net:send_packet(aiutp_buffer:head(PCB3#aiutp_pcb.outbuf), PCB3);
           true -> PCB3
        end,

    %% Handle fast timeout
    PCB5 =
        if FastTimeout ->
            if ?WRAPPING_DIFF_16(PCB4#aiutp_pcb.seq_nr,
                                 PCB4#aiutp_pcb.cur_window_packets) /= FastResendSeqNR0 ->
                PCB4#aiutp_pcb{fast_timeout = false};
               true ->
                aiutp_net:send_packet(
                    aiutp_buffer:head(PCB4#aiutp_pcb.outbuf),
                    PCB4#aiutp_pcb{fast_resend_seq_nr = aiutp_util:bit16(FastResendSeqNR0 + 1)})
            end;
           true -> PCB4
        end,

    %% Process selective ACKs
    PCB6 = aiutp_pcb_cc:selective_ack_packet(SAckedPackets, RecvTime, PCB5),
    process_packet_3(Packet, PCB6).

%% @private Handle state transitions and ST_STATE packets
process_packet_3(#aiutp_packet{type = PktType} = Packet,
                 #aiutp_pcb{state = State} = PCB) ->
    %% Check if connection is full
    {ISFull, PCB0} = aiutp_net:is_full(-1, PCB),
    PCB1 =
        if (ISFull == false) and (State == ?CS_CONNECTED_FULL) ->
            PCB0#aiutp_pcb{state = ?CS_CONNECTED};
           true -> PCB0
        end,

    %% ST_STATE packets don't carry data
    if PktType == ?ST_STATE -> PCB1;
       (State /= ?CS_CONNECTED) and (State /= ?CS_CONNECTED_FULL) -> PCB1;
       true -> process_packet_4(Packet, PCB1)
    end.

%% @private Handle FIN and data reception
process_packet_4(#aiutp_packet{type = PktType, seq_nr = PktSeqNR} = Packet,
                 #aiutp_pcb{got_fin = GotFin} = PCB) ->
    PCB0 =
        if (PktType == ?ST_FIN) and (GotFin == false) ->
            PCB#aiutp_pcb{got_fin = true, eof_pkt = PktSeqNR};
           true -> PCB
        end,
    aiutp_rx:in(Packet, PCB0).

%%------------------------------------------------------------------------------
%% @doc Check and handle timeouts
%%
%% Delegates to aiutp_pcb_timeout module.
%% @end
%%------------------------------------------------------------------------------
-spec check_timeouts(#aiutp_pcb{}) -> #aiutp_pcb{}.
check_timeouts(PCB) ->
    aiutp_pcb_timeout:check_timeouts(PCB).

%%------------------------------------------------------------------------------
%% @doc Write data to the connection
%%
%% @param Data Binary data to send
%% @param PCB Protocol control block
%% @returns {ok, PCB} | {{error, Reason}, PCB}
%% @end
%%------------------------------------------------------------------------------
-spec write(binary(), #aiutp_pcb{}) -> {ok | {error, atom()}, #aiutp_pcb{}}.
write(_, #aiutp_pcb{state = State} = PCB)
  when (State /= ?CS_CONNECTED),
       (State /= ?CS_CONNECTED_FULL) ->
    {{error, not_connected}, PCB};
write(_, #aiutp_pcb{fin_sent = FinSent} = PCB)
  when FinSent == true ->
    {{error, closed}, PCB};
write(Data, PCB) ->
    aiutp_tx:in(Data, PCB#aiutp_pcb{time = aiutp_util:millisecond()}).

%%------------------------------------------------------------------------------
%% @doc Close the connection
%%
%% Initiates graceful shutdown by sending FIN packet.
%%
%% @param PCB Protocol control block
%% @returns Updated PCB
%% @end
%%------------------------------------------------------------------------------
-spec close(#aiutp_pcb{}) -> #aiutp_pcb{}.
close(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_UNINITIALIZED;
       State == ?CS_IDLE;
       State == ?CS_DESTROY ->
    PCB#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{state = State, rto = RTO} = PCB)
  when State == ?CS_SYN_SENT ->
    PCB#aiutp_pcb{
        rto_timeout = erlang:min(RTO * 2, 60) + aiutp_util:millisecond(),
        state = ?CS_DESTROY
    };
close(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
    PCB#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{fin_sent_acked = FinSentAcked, fin_sent = FinSent} = PCB) ->
    PCB0 = PCB#aiutp_pcb{read_shutdown = true, close_requested = true},
    if FinSent == false ->
        aiutp_net:send_fin(PCB0#aiutp_pcb{fin_sent = true});
       FinSentAcked == true ->
        PCB0#aiutp_pcb{state = ?CS_DESTROY};
       true -> PCB0
    end.

%%------------------------------------------------------------------------------
%% @doc Read data from the connection
%%
%% Returns accumulated data from the receive queue.
%%
%% @param PCB Protocol control block
%% @returns {Data | undefined, UpdatedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec read(#aiutp_pcb{}) -> {binary() | undefined, #aiutp_pcb{}}.
read(#aiutp_pcb{inque = InQue, max_window = MaxWindow,
                inbuf = InBuf, last_rcv_win = LastRcvWin} = PCB) ->
    L = aiutp_queue:to_list(InQue),
    WindowSize = aiutp_net:window_size(MaxWindow, InBuf),
    Now = aiutp_util:millisecond(),

    %% Send ACK if window opens up
    PCB0 =
        if WindowSize > LastRcvWin ->
            if LastRcvWin == 0 ->
                aiutp_net:send_ack(PCB#aiutp_pcb{time = Now});
               true ->
                PCB#aiutp_pcb{time = Now, ida = true}
            end;
           true -> PCB#aiutp_pcb{time = Now}
        end,

    QueSize = aiutp_queue:size(InQue),
    if QueSize > 0 ->
        {lists:foldl(
             fun(Bin, Acc) -> <<Acc/binary, Bin/binary>> end,
             <<>>, L),
         PCB0#aiutp_pcb{inque = aiutp_queue:new()}};
       true ->
        {undefined, PCB0}
    end.

%%------------------------------------------------------------------------------
%% @doc Initiate outbound connection
%%
%% Creates a new PCB and sends SYN packet to establish connection.
%%
%% @param Socket The UDP socket reference
%% @param ConnIdRecv Connection ID for receiving
%% @returns Initialized PCB with SYN sent
%% @end
%%------------------------------------------------------------------------------
-spec connect(term(), integer()) -> #aiutp_pcb{}.
connect(Socket, ConnIdRecv) ->
    ConnIdSend = aiutp_util:bit16(ConnIdRecv + 1),
    PCB = new(ConnIdRecv, ConnIdSend, Socket),
    #aiutp_pcb{max_window = MaxWindow, inbuf = InBuf,
               conn_id_recv = ConnId, outbuf = OutBuf} = PCB,

    Now = aiutp_util:millisecond(),
    SeqNR = aiutp_util:bit16_random(),
    WindowSize = aiutp_net:window_size(MaxWindow, InBuf),

    %% Build SYN packet
    Packet = aiutp_packet:syn(SeqNR),
    Packet0 = Packet#aiutp_packet{conn_id = ConnId, wnd = WindowSize, seq_nr = SeqNR},
    WrapPacket = #aiutp_packet_wrap{packet = Packet0},

    %% Add to outgoing buffer
    OutBuf0 = aiutp_buffer:append(WrapPacket, OutBuf),
    Iter = aiutp_buffer:head(OutBuf0),

    PCB0 = PCB#aiutp_pcb{
        state = ?CS_SYN_SENT,
        time = Now,
        retransmit_timeout = 3000,
        rto_timeout = 3000 + Now,
        last_rcv_win = WindowSize,
        outbuf = OutBuf0,
        cur_window_packets = 1,
        seq_nr = SeqNR + 1
    },
    aiutp_net:send_packet(Iter, PCB0).

%%------------------------------------------------------------------------------
%% @doc Accept inbound connection
%%
%% Creates a new PCB for an incoming SYN packet and processes it.
%%
%% @param Socket The UDP socket reference
%% @param {Packet, Timestamp} Incoming SYN packet with timestamp
%% @returns {ConnIdRecv, InitializedPCB}
%% @end
%%------------------------------------------------------------------------------
-spec accept(term(), {#aiutp_packet{}, integer()}) -> {integer(), #aiutp_pcb{}}.
accept(Socket, {#aiutp_packet{conn_id = ConnIdSend}, _} = Packet) ->
    ConnIdRecv = aiutp_util:bit16(ConnIdSend + 1),
    PCB = new(ConnIdRecv, ConnIdSend, Socket),
    PCB1 = process(Packet, PCB),
    {ConnIdRecv, PCB1}.

%%------------------------------------------------------------------------------
%% @doc Flush pending outgoing data
%%
%% @param PCB Protocol control block
%% @returns Updated PCB after flushing queue
%% @end
%%------------------------------------------------------------------------------
-spec flush(#aiutp_pcb{}) -> #aiutp_pcb{}.
flush(PCB) ->
    aiutp_net:flush_queue(PCB).

%% Protocol packet format reference:
%%
%% 0       4       8               16              24              32
%% +-------+-------+---------------+---------------+---------------+
%% | type  | ver   | extension     | connection_id                 |
%% +-------+-------+---------------+---------------+---------------+
%% | timestamp_microseconds                                        |
%% +---------------+---------------+---------------+---------------+
%% | timestamp_difference_microseconds                             |
%% +---------------+---------------+---------------+---------------+
%% | wnd_size                                                      |
%% +---------------+---------------+---------------+---------------+
%% | seq_nr                        | ack_nr                        |
%% +---------------+---------------+---------------+---------------+
%%
%% - timestamp_microseconds: Send timestamp
%% - timestamp_difference_microseconds: One-way delay (recv - send time)
%% - wnd_size: Receiver's available buffer space in bytes
%%
%% SELECTIVE ACK: Max 4 bytes = 32 packets * 512 bytes = 16KB
%% Covers range [ack_nr + 2, ack_nr + 2 + 31]
%% Bit order within each byte is reversed (LSB = lowest seq_nr)
