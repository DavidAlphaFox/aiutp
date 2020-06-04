-type utp_packet_type() :: st_data | st_fin | st_state | st_reset | st_syn.

-type utp_extension() :: {sack, binary()} | {ext_bits, binary()}.
-type timestamp() :: integer().

-define(SYN_SEND,'SYN_SEND').
-define(SYN_RECEIVE,'SYN_RECEIVE').
-define(ESTABLISHED,'ESTABLISHED').
-define(CLOSED,'CLOSED').
-define(CLOSING,'CLOSING').
-define(CLOSE_WAIT,'CLOSE_WAIT').
-define(ERROR,'ERROR').

-define(RTT_MAX,9223372036854775807).
-define(TS_DIFF_MAX,2147483647).
-define(SEQ_NO_MASK, 16#FFFF).
-define(ACK_NO_MASK, 16#FFFF).
-define(REORDER_BUFFER_MAX_SIZE,1024).
-define(OUTGOING_BUFFER_MAX_SIZE, 1024).
-define(PACKET_SIZE, 350).
-define(OPT_RECV_BUF, ?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE).
-define(OPT_SEND_BUF, ?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE).

-define(REORDER_BUFFER_SIZE, 32).
% us
-define(CONGESTION_CONTROL_TARGET, 100000).
%% 每个来回最大可以增加3000 bytes的窗口
% bytes
-define(MAX_CWND_INCREASE_BYTES_PER_RTT, 3000).

%% 最小的传输窗口是3000 bytes
% bytes
-define(MIN_WINDOW_SIZE, 3000).
-define(DUPLICATE_ACKS_BEFORE_RESEND,3).

-record(utp_packet, {type           :: utp_packet_type(),
                     conn_id        :: integer(),
                     win_sz = 0     :: integer(),
                     seq_no         :: integer(),
                     ack_no         :: integer(),
                     extension = [] :: [utp_extension()],
                     payload = <<>> :: binary()
                    }).

-type utp_packet() :: #utp_packet{}.

-record(utp_packet_wrap, {packet,
                          transmissions = 0,
                          payload = 0,
                          send_time = 0
                         }).
-record(utp_net,
        {%%sndbuf setting, in bytes
         opt_sndbuf = ?OPT_SEND_BUF,
         %%rcvbuf setting, in bytes
         opt_rcvbuf = ?OPT_RECV_BUF,
         state = undefined,
         %% the number of packets in the send queue. Packets that haven't
         %% yet been sent count as well as packets marked as needing resend
         %% the oldest un-acked packet in the send queue is seq_nr - cur_window_packets
         cur_window_packets = 0,
         %% how much of the window is used, number of bytes in-flight
         %% packets that have not yet been sent do not count, packets
         %% that are marked as needing to be re-sent (due to a timeout)
         %% don't count either
         cur_window = 0,
         %% maximum window size, in bytes
         max_window = ?OPT_SEND_BUF,
         %% max receive window for other end, in bytes
         max_peer_window = ?OPT_RECV_BUF,
         %% All sequence numbers up to including this have been properly received
         %% by us
         ack_nr = undefined,
         %% This is the sequence number for the next packet to be sent.
         seq_nr = undefined,
         %% This is the sequence number of the next packet we're allowed to
         %% do a fast resend with. This makes sure we only do a fast-resend
         %% once per packet. We can resend the packet with this sequence number
         %% or any later packet (with a higher sequence number).
         fast_resend_seq_nr = 1,
         inbuf = <<>>,
         reorder = [],
         outbuf = queue:new(),
         reply_micro,
         rtt = none,
         rtt_ledbat = none,
         our_ledbat = none,
         peer_ledbat = none,
         %% Round trip time measurements and LEDBAT
         min_rtt = 30000000 :: integer(),
         %% --------------------
         %% ms, When was the window last totally full (in send direction)
         last_maxed_out_window :: integer(),
         maxed_out_window = false,
         fin_sent = false,
         fin_acked = false,
         got_fin = false,
         eof_seq_no = -1,
         fast_timeout = false,
         conn_id,
         peer_conn_id
        }).
