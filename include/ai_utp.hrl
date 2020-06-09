-type utp_packet_type() :: st_data | st_fin | st_state | st_reset | st_syn.

-type utp_extension() :: {sack, binary()} | {ext_bits, binary()}.
-type timestamp() :: integer().

-define(SYN_SEND,'SYN_SEND').
-define(SYN_RECEIVE,'SYN_RECEIVE').
-define(ESTABLISHED,'ESTABLISHED').
-define(CLOSED,'CLOSED').
-define(CLOSING,'CLOSING').
-define(CLOSE_WAIT,'CLOSE_WAIT').

-define(RTT_MAX,9223372036854775807).
-define(TS_DIFF_MAX,2147483647).
-define(SEQ_NO_MASK, 16#FFFF).
-define(ACK_NO_MASK, 16#FFFF).
-define(HALF_CIRCLE, 16#6FFF).
-define(REORDER_BUFFER_MAX_SIZE,4096).
-define(OUTGOING_BUFFER_MAX_SIZE, 1024).
-define(PACKET_SIZE, 1180).
-define(OPT_RECV_BUF, 1208320).
-define(OPT_SEND_BUF, 1208320).

%us
-define(MAX_RECV_IDLE_TIME,30000000).
-define(MAX_SEND_IDLE_TIME,5000000).
% us
-define(CONGESTION_CONTROL_TARGET, 100000).
%% ms窗口劣化
-define(MAX_WINDOW_DECAY,100).
%% 每个来回最大可以增加3000 bytes的窗口
% bytes
-define(MAX_CWND_INCREASE_BYTES_PER_RTT, 102400).

%% 最小的传输窗口是3000 bytes
% bytes
-define(MIN_WINDOW_SIZE, 102400).
-define(MAX_WINDOW_SIZE,14680064).
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
                          wanted = 0,
                          send_time = 0,
                          need_resend = false
                         }).
-record(utp_net,{%%sndbuf setting, in bytes
                 opt_sndbuf = ?OPT_SEND_BUF * 4,
                 %%rcvbuf setting, in bytes
                 opt_recvbuf = ?OPT_RECV_BUF * 4,
                 socket = undefined,
                 remote = undefined,
                 state = undefined,
                 error = normal,
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
                 reorder = array:new(16#FFFF + 1,fixed),
                 reorder_size = 0,
                 outbuf = queue:new(),
                 recvbuf = queue:new(),
                 recvbuf_size = 0,
                 sndbuf = queue:new(),
                 sndbuf_size = 0,
                 reply_micro = 0,
                 rtt = none,
                 our_ledbat = none,
                 peer_ledbat = none,
                 %% --------------------
                 %% ms, When was the window last totally full (in send direction)
                 last_maxed_out_window :: integer(),
                 last_decay_win :: integer(),
                 fin_sent = false,
                 fin_acked = false,
                 got_fin = false,
                 eof_seq_no = -1,
                 conn_id,
                 peer_conn_id,
                 last_send = 0 :: integer(),
                 last_recv = 0:: integer(),
                 last_ack = undefined}).
