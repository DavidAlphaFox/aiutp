%%------------------------------------------------------------------------------
%% @doc aiutp - uTP Protocol Constants and Records
%%
%% This file defines constants and records according to BEP-29 specification.
%% Reference: https://www.bittorrent.org/beps/bep_0029.html
%%
%% @end
%%------------------------------------------------------------------------------

-ifndef(AIUTP_HRL).
-define(AIUTP_HRL, true).

%%==============================================================================
%% Section 1: Protocol Version (BEP-29)
%%==============================================================================

-define(UTP_VERSION, 1).

%%==============================================================================
%% Section 2: Packet Types (BEP-29)
%%
%% | Type     | Value | Description                    |
%% |----------|-------|--------------------------------|
%% | ST_DATA  | 0     | Regular data packet            |
%% | ST_FIN   | 1     | Finalize connection            |
%% | ST_STATE | 2     | ACK packet (no payload)        |
%% | ST_RESET | 3     | Forcefully terminate           |
%% | ST_SYN   | 4     | Initiate connection            |
%%==============================================================================

-define(ST_DATA,  0).
-define(ST_FIN,   1).
-define(ST_STATE, 2).
-define(ST_RESET, 3).
-define(ST_SYN,   4).

%%==============================================================================
%% Section 3: Extension Types (BEP-29)
%%
%% Extensions are chained, each starting with a type byte and length byte.
%% Type 0 means no more extensions.
%%==============================================================================

-define(EXT_NONE, 0).        %% No more extensions
-define(EXT_SACK, 1).        %% Selective ACK bitmask
-define(EXT_EXT_BITS, 2).    %% Extension bits (reserved for future use)

%%==============================================================================
%% Section 4: Connection States
%%
%% State machine:
%%   Initiator: IDLE -> SYN_SENT -> CONNECTED -> DESTROY
%%   Responder: IDLE -> SYN_RECV -> CONNECTED -> DESTROY
%%==============================================================================

-define(CS_UNINITIALIZED, 'CS_UNINITIALIZED').
-define(CS_IDLE,          'CS_IDLE').
-define(CS_SYN_SENT,      'CS_SYN_SENT').
-define(CS_SYN_RECV,      'CS_SYN_RECV').
-define(CS_CONNECTED,     'CS_CONNECTED').
-define(CS_CONNECTED_FULL,'CS_CONNECTED_FULL').  %% Send buffer full
-define(CS_RESET,         'CS_RESET').
-define(CS_DESTROY,       'CS_DESTROY').

%%==============================================================================
%% Section 5: Timeout Parameters (BEP-29)
%%
%% RTO calculation: timeout = max(rtt + rtt_var * 4, RTO_MIN)
%% On consecutive timeouts: timeout *= 2 (exponential backoff)
%%==============================================================================

%% BEP-29: Minimum packet timeout is 500ms
-define(RTO_MIN, 500).

%% BEP-29: Maximum RTO (implementation specific)
-define(RTO_MAX, 6000).

%% BEP-29: Initial timeout before any RTT samples
-define(RTO_INITIAL, 1000).

%% Initial RTT variance estimate (ms)
-define(RTT_VAR_INITIAL, 800).

%% Keep-alive interval: 29 seconds
%% Reason: Measured from many home NAT devices to prevent NAT timeout
-define(KEEPALIVE_INTERVAL, 29000).

%% Timeout check interval (ms)
-define(TIMEOUT_CHECK_INTERVAL, 150).

%% RST info cache timeout (ms)
-define(RST_INFO_TIMEOUT, 10000).
-define(RST_INFO_LIMIT, 1000).

%%==============================================================================
%% Section 6: Window and Buffer Parameters
%%==============================================================================

%% BEP-29: Fixed header size is 20 bytes
-define(UTP_HEADER_SIZE, 20).

%% Maximum payload size per packet (MTU - headers)
%% Typical: 1500 (MTU) - 20 (IP) - 8 (UDP) - 20 (uTP) = 1452
%% Conservative value for path MTU issues
-define(PACKET_SIZE, 1296).

%% BEP-29: Minimum window size is 150 bytes
%% This prevents the window from shrinking to zero
%% Note: Current implementation uses larger value for performance
-define(MIN_WINDOW_SIZE_BEP29, 150).
-define(MIN_WINDOW_SIZE, 2906).  %% = 2 * PACKET_SIZE + 314

%% Maximum send buffer size (packets)
-define(OUTGOING_BUFFER_MAX_SIZE, 1024).

%% Maximum reorder buffer size (packets)
-define(REORDER_BUFFER_MAX_SIZE, 1024).
-define(REORDER_BUFFER_SIZE, 32).

%% Burst mode buffer size
-define(BURST_OUTGOING_BUFFER_SIZE, 255).

%%==============================================================================
%% Section 7: Congestion Control Parameters (LEDBAT)
%%
%% LEDBAT (Low Extra Delay Background Transport) aims to:
%% - Minimize delay impact on the network
%% - Yield to other traffic
%% Reference: RFC 6817
%%==============================================================================

%% BEP-29: Target delay is 100ms (CCONTROL_TARGET)
-define(TARGET_DELAY, 100000).  %% microseconds

%% Maximum window increase per RTT (bytes)
%% TCP increases by 1 MSS (~1500 bytes) per RTT
-define(MAX_CWND_INCREASE_BYTES_PER_RTT, 3000).

%% Window decay time when no packets sent (ms)
-define(MAX_WINDOW_DECAY, 100).

%% Number of delay samples to keep
-define(CUR_DELAY_SIZE, 3).

%% Delay base history entries (reset every 13 minutes)
%% Reason: Clock skew of ~10ms per 325 seconds is possible
-define(DELAY_BASE_HISTORY, 13).

%% Slow-start threshold initial value
-define(SSTHRESH_INITIAL, (?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE)).

%%==============================================================================
%% Section 8: Retransmission Parameters
%%==============================================================================

%% BEP-29: Duplicate ACK threshold for fast retransmit is 3
%% Note: Implementation uses 4 for more conservative behavior
-define(DUPLICATE_ACKS_BEFORE_RESEND_BEP29, 3).
-define(DUPLICATE_ACKS_BEFORE_RESEND, 4).

%% ACK number allowed window
%% A non-SYN packet with ack_nr difference > this is considered suspicious
-define(ACK_NR_ALLOWED_WINDOW, 4).

%%==============================================================================
%% Section 9: Bit Masks and Limits
%%==============================================================================

-define(SEQ_NR_MASK, 16#FFFF).
-define(ACK_NR_MASK, 16#FFFF).
-define(TIMESTAMP_MASK, 16#FFFFFFFF).
-define(RTT_MAX, 16#FFFFFFFFFFFFFFFF).

%%==============================================================================
%% Section 10: Utility Macros
%%
%% Wrapping difference for sequence number comparison.
%% Returns negative if L < R, positive if L > R (considering wrap-around)
%%==============================================================================

-define(WRAPPING_DIFF_32(L, R),
        (((R - L) band 16#FFFFFFFF) - ((L - R) band 16#FFFFFFFF))).

-define(WRAPPING_DIFF_16(L, R),
        (((R - L) band 16#FFFF) - ((L - R) band 16#FFFF))).

%%==============================================================================
%% Section 11: Packet Record
%%
%% Represents a uTP packet as per BEP-29 header format.
%%==============================================================================

-record(aiutp_packet, {
    %% Packet type: ST_DATA | ST_FIN | ST_STATE | ST_RESET | ST_SYN
    type :: ?ST_DATA | ?ST_FIN | ?ST_STATE | ?ST_RESET | ?ST_SYN,

    %% Connection ID (16-bit)
    %% Receiver uses conn_id, sender uses conn_id + 1
    conn_id :: non_neg_integer(),

    %% Advertised receive window size (32-bit, bytes)
    wnd = 0 :: non_neg_integer(),

    %% Sequence number of this packet (16-bit)
    seq_nr = 0 :: non_neg_integer(),

    %% Last received sequence number (16-bit)
    ack_nr = 0 :: non_neg_integer(),

    %% Timestamp when packet was sent (32-bit, microseconds)
    tv_usec = 0 :: non_neg_integer(),

    %% Timestamp difference: our_timestamp - their_timestamp (32-bit, microseconds)
    %% Used by receiver to calculate one-way delay
    reply_micro = 0 :: non_neg_integer(),

    %% Extensions: [{sack, binary()} | {ext_bits, binary()}]
    extension = [] :: list(),

    %% Payload data
    payload = <<>> :: binary()
}).

%%==============================================================================
%% Section 12: Packet Wrapper Record
%%
%% Wraps a packet with transmission metadata for the send buffer.
%%==============================================================================

-record(aiutp_packet_wrap, {
    %% The packet to send
    packet :: #aiutp_packet{},

    %% Serialized packet content (cached for retransmission)
    content = undefined :: undefined | binary(),

    %% Payload size in bytes
    payload = 0 :: non_neg_integer(),

    %% Time when packet was sent (microseconds)
    time_sent = 0 :: non_neg_integer(),

    %% Number of times this packet has been transmitted
    transmissions = 0 :: non_neg_integer(),

    %% Flag indicating packet needs retransmission
    need_resend = false :: boolean()
}).

%%==============================================================================
%% Section 13: Protocol Control Block (PCB) Record
%%
%% The PCB contains all state for a single uTP connection.
%% Fields are organized by functional area for clarity.
%%==============================================================================

-record(aiutp_pcb, {
    %%--------------------------------------------------------------------------
    %% Connection Identity
    %%--------------------------------------------------------------------------

    %% Connection ID for packets we receive
    conn_id_recv :: non_neg_integer() | undefined,

    %% Connection ID for packets we send (= conn_id_recv + 1 for initiator)
    conn_id_send :: non_neg_integer() | undefined,

    %% Reference to the UDP socket process
    socket :: pid() | undefined,

    %%--------------------------------------------------------------------------
    %% Connection State
    %%--------------------------------------------------------------------------

    %% Current connection state
    state = ?CS_UNINITIALIZED :: atom(),

    %% User has called close()
    close_requested = false :: boolean(),

    %% Reading is disabled (half-close)
    read_shutdown = false :: boolean(),

    %%--------------------------------------------------------------------------
    %% Sequence Number Management
    %%--------------------------------------------------------------------------

    %% Next sequence number to send (starts at 1 for SYN)
    seq_nr = 1 :: non_neg_integer(),

    %% Last sequence number we have ACKed (all packets up to this received)
    ack_nr = 0 :: non_neg_integer(),

    %% Sequence number for timeout detection
    timeout_seq_nr = 0 :: non_neg_integer(),

    %% Next sequence number allowed for fast resend (prevents duplicate fast resends)
    fast_resend_seq_nr = 1 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% FIN Handling (Connection Termination)
    %%--------------------------------------------------------------------------

    %% Received a FIN packet
    got_fin = false :: boolean(),

    %% All packets up to FIN have been received
    got_fin_reached = false :: boolean(),

    %% We have sent our FIN
    fin_sent = false :: boolean(),

    %% Our FIN has been ACKed
    fin_sent_acked = false :: boolean(),

    %% Sequence number of the received FIN packet
    eof_pkt = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% Window Management
    %%--------------------------------------------------------------------------

    %% Bytes currently in-flight (sent but not ACKed)
    %% Does not include packets marked for resend
    cur_window = 0 :: non_neg_integer(),

    %% Number of packets in send queue (including those needing resend)
    %% Oldest unacked packet is at seq_nr - cur_window_packets
    cur_window_packets = 0 :: non_neg_integer(),

    %% Maximum send window (bytes), adjusted by congestion control
    max_window = 0 :: non_neg_integer(),

    %% Maximum receive window advertised by peer (bytes)
    max_window_user = 255 * ?PACKET_SIZE :: non_neg_integer(),

    %% Last receive window we advertised (bytes)
    last_rcv_win = 0 :: non_neg_integer(),

    %% Timestamp when window was last decayed (wraps)
    last_rwin_decay = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% RTT/RTO Management
    %%--------------------------------------------------------------------------

    %% Smoothed Round Trip Time (milliseconds)
    rtt = 0 :: non_neg_integer(),

    %% RTT variance (milliseconds)
    rtt_var = ?RTT_VAR_INITIAL :: non_neg_integer(),

    %% Retransmission Timeout (milliseconds)
    %% Calculated as: max(rtt + rtt_var * 4, RTO_MIN)
    rto = ?RTO_INITIAL :: non_neg_integer(),

    %% RTT history for min RTT calculation
    rtt_hist :: any(),

    %% Current retransmit timeout value (may be backed off)
    retransmit_timeout = 0 :: non_neg_integer(),

    %% Absolute time when RTO expires
    rto_timeout = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% Congestion Control (LEDBAT)
    %%--------------------------------------------------------------------------

    %% Target delay for LEDBAT (microseconds)
    %% BEP-29 specifies 100ms = 100000us
    target_delay = ?TARGET_DELAY :: non_neg_integer(),

    %% Slow-start threshold (bytes)
    ssthresh = ?SSTHRESH_INITIAL :: non_neg_integer(),

    %% In slow-start phase (exponential growth)
    slow_start = true :: boolean(),

    %% Timestamp when window was last fully utilized
    %% Used to prevent window growth when not sending at capacity
    last_maxed_out_window = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% Delay Statistics (for LEDBAT)
    %%--------------------------------------------------------------------------

    %% Our delay history (samples we measure)
    our_hist :: any(),

    %% Their delay history (samples they report)
    their_hist :: any(),

    %% Average delay relative to initial sample (microseconds)
    average_delay = 0 :: integer(),

    %% Sum of recent delay samples (for averaging)
    current_delay_sum = 0 :: integer(),

    %% Number of samples in current_delay_sum
    current_delay_samples = 0 :: non_neg_integer(),

    %% First raw delay sample (baseline for relative delays)
    average_delay_base = 0 :: non_neg_integer(),

    %% Next time to add an average delay sample
    average_sample_time = 0 :: non_neg_integer(),

    %% Last measured delay (microseconds)
    last_measured_delay = 0 :: non_neg_integer(),

    %% Estimated clock drift (microseconds per 5 seconds)
    clock_drift = 0 :: integer(),

    %%--------------------------------------------------------------------------
    %% Retransmission Management
    %%--------------------------------------------------------------------------

    %% Number of consecutive retransmissions
    retransmit_count = 0 :: non_neg_integer(),

    %% Number of duplicate ACKs received
    duplicate_ack = 0 :: non_neg_integer(),

    %% Fast timeout flag
    fast_timeout = false :: boolean(),

    %% Number of reordered packets
    reorder_count = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% Timestamps
    %%--------------------------------------------------------------------------

    %% Current time (set before processing)
    time = undefined :: non_neg_integer() | undefined,

    %% Time when last packet was received
    recv_time = undefined :: non_neg_integer() | undefined,

    %% Last time we received any packet (for keep-alive)
    last_got_packet = 0 :: non_neg_integer(),

    %% Last time we sent any packet
    last_sent_packet = 0 :: non_neg_integer(),

    %% Timestamp difference to echo back to sender
    reply_micro = 0 :: non_neg_integer(),

    %% Timer for zero window probing (send packet every 30s when window is 0)
    zerowindow_time = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------------
    %% Buffers and Queues
    %%--------------------------------------------------------------------------

    %% Receive buffer (ring buffer for reassembly)
    inbuf :: any(),

    %% Send buffer (ring buffer for unacked packets)
    outbuf :: any(),

    %% Receive queue (ordered data ready for application)
    inque :: any(),

    %% Send queue (data waiting to be packetized)
    outque :: any(),

    %%--------------------------------------------------------------------------
    %% Special Modes
    %%--------------------------------------------------------------------------

    %% Immediate Data Acknowledgment mode
    ida = false :: boolean(),

    %% Burst mode (send multiple packets without waiting for ACK)
    burst = true :: boolean()
}).

-endif. %% AIUTP_HRL
