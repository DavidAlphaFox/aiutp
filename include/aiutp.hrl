-define(MAX_CWND_INCREASE_BYTES_PER_RTT,3000).
% number of bytes to increase max window size by, per RTT. This is
% scaled down linearly proportional to off_target. i.e. if all packets
% in one window have 0 delay, window size will increase by this number.
% Typically it's less. TCP increases one MSS per RTT, which is 1500
-define(CUR_DELAY_SIZE,3).
-define(DELAY_BASE_HISTORY,13).
% experiments suggest that a clock skew of 10 ms per 325 seconds
% is not impossible. Reset delay_base every 13 minutes. The clock
% skew is dealt with by observing the delay base in the other
% direction, and adjusting our own upwards if the opposite direction
% delay base keeps going down
-define(MAX_WINDOW_DECAY,100). % ms
-define(REORDER_BUFFER_SIZE,32).
-define(REORDER_BUFFER_MAX_SIZE,1024).
-define(OUTGOING_BUFFER_MAX_SIZE,1024).
-define(PACKET_SIZE,1453).
-define(MIN_WINDOW_SIZE,2906).
%-define(MIN_WINDOW_SIZE,7265).
% this is the minimum max_window value. It can never drop below this
-define(DUPLICATE_ACKS_BEFORE_RESEND,4).
% if we receive 4 or more duplicate acks, we resend the packet
% that hasn't been acked yet
-define(ACK_NR_ALLOWED_WINDOW,4).
% Allow a reception window of at least 3 ack_nrs behind seq_nr
% A non-SYN packet with an ack_nr difference greater than this is
% considered suspicious and ignored
-define(RST_INFO_TIMEOUT,10000).
-define(RST_INFO_LIMIT,1000).
-define(KEEPALIVE_INTERVAL,29000). %ms
% 29 seconds determined from measuring many home NAT devices
-define(UTP_HEADER_SIZE,20).
-define(TIMEOUT_CHECK_INTERVAL,500).
-define(SEQ_NR_MASK,16#FFFF).
-define(ACK_NR_MASK,16#FFFF).
-define(TIMESTAMP_MASK, 16#FFFFFFFF).
-define(RTT_MAX,16#FFFFFFFFFFFFFFFF).



%% WRAPPING_DIFF_16(1,65535) = 65532.
%% 循环比较
%% 如果L比R小，结果为负数
%% 如果L比R大，结果为正数
-define(WRAPPING_DIFF_32(L,R),(((R - L) band 16#FFFFFFFF) - ((L - R) band 16#FFFFFFFF))).
-define(WRAPPING_DIFF_16(L,R),(((R - L) band 16#FFFF) - ((L - R) band 16#FFFF))).

-define(CS_UNINITIALIZED,'CS_UNINITIALIZED').
-define(CS_IDLE,'CS_IDLE').
-define(CS_SYN_SENT,'CS_SYN_SENT').
-define(CS_SYN_RECV,'CS_SYN_RECV').
-define(CS_CONNECTED,'CS_CONNECTED').
-define(CS_CONNECTED_FULL,'CS_CONNECTED_FULL').
-define(CS_RESET,'CS_RESET').
-define(CS_DESTROY,'CS_DESTROY').


-define(ST_DATA,  0).
-define(ST_FIN,   1).
-define(ST_STATE, 2).
-define(ST_RESET, 3).
-define(ST_SYN,   4).



-record(aiutp_packet, {type           :: ?ST_DATA | ?ST_FIN | ?ST_STATE | ?ST_RESET | ?ST_SYN,
                       conn_id        :: integer(), % 会话ID
                       wnd = 0        :: integer(), % 我们的窗口
                       seq_nr = 0     :: integer(), % 我们的序号
                       ack_nr = 0         :: integer(), % 确认的序号
                       tv_usec = 0        :: integer(), % 发送时间
                       reply_micro = 0    :: integer(), % 回复时间
                       extension = [] :: [{sack, binary()} | {ext_bits, binary()}],
                       payload =  <<>> :: binary()
                    }).
-record(aiutp_packet_wrap,{packet,
                           content = undefined,
                           payload = 0,
                           time_sent = 0, %microsecond
                           transmissions = 0,
                           need_resend = false}).

-record(aiutp_pcb,{state = ?CS_UNINITIALIZED,
                   retransmit_count = 0,
                   socket,
                   time = undefined,
                   recv_time = undefined,
                   ida = fasle,
                   reorder_count = 0,
                   duplicate_ack = 0,
                   cur_window_packets = 0,
                   %% the number of packets in the send queue. Packets that haven't
                   %% yet been sent count as well as packets marked as needing resend
                   %% the oldest un-acked packet in the send queue is seq_nr - cur_window_packets
                   cur_window = 0,
                   % how much of the window is used, number of bytes in-flight
                   % packets that have not yet been sent do not count, packets
                   % that are marked as needing to be re-sent (due to a timeout)
                   % don't count either
                   max_window = 0, % maximum window size in bytes,
                   target_delay = 300000, % mircoseconds
                   got_fin = false, %% is a FIN packet in reassembly buffer
                   got_fin_reached = false, %% Have we reached the FIN
                   fin_sent = false, % Have we sent our FIN
                   fin_sent_acked = false, % Have our FIN been ACKed
                   read_shutdown = false, %  Reading is disabled
                   close_requested = false, % User called utp_close()
                   fast_timeout = false, % Timeout procedure
                   max_window_user = 255 * ?PACKET_SIZE, % max receive window for other end, in bytes
                   last_rwin_decay = 0, % TickCount when we last decayed window (wraps)
                   eof_pkt = 0,
                   % the sequence number of the FIN packet. This field is only set
                   % when we have received a FIN, and the flag field has the FIN flag set.
                   % it is used to know when it is safe to destroy the socket, we must have
                   % received all packets up to this sequence number first.
                   ack_nr = 0,
                   % All sequence numbers up to including this have been properly received by us
                   seq_nr = 1,
                   % This is the sequence number for the next packet to be sent.
                   timeout_seq_nr = 0,
                   fast_resend_seq_nr = 1,
                   % This is the sequence number of the next packet we're allowed to
                   % do a fast resend with. This makes sure we only do a fast-resend
                   % once per packet. We can resend the packet with this sequence number
                   % or any later packet (with a higher sequence number).
                   reply_micro = 0,
                   last_got_packet = 0,
                   last_sent_packet = 0,
                   last_measured_delay = 0,
                   last_maxed_out_window = 0,
                   % timestamp of the last time the cwnd was full
                   % this is used to prevent the congestion window
                   % from growing when we're not sending at capacity
                   rtt = 0, %Round trip time
                   rtt_var = 800, %Round trip time variance
                   rto = 3000, %Round trip timeout
                   rtt_hist,
                   retransmit_timeout = 0,
                   rto_timeout = 0, %The RTO timer will timeout here
                   zerowindow_time = 0 ,%When the window size is set to zero, start this timer. It will send a new packet every 30secs
                   conn_id_recv,% Connection ID for packets I receive
                   conn_id_send,% Connection ID for packets I send
                   last_rcv_win = 0 ,%Last rcv window we advertised, in bytes
                   our_hist,
                   their_hist,
                   average_delay = 0,
                   % this is the average delay samples, as compared to the initial sample. It's averaged over 5 seconds
                   current_delay_sum = 0,
                   % this is the sum of all the delay samples
                   % we've made recently. The important distinction
                   % of these samples is that they are all made compared
                   % to the initial sample, this is to deal with
                   % wrapping in a simple way
                   current_delay_samples = 0,
                   % number of sample ins current_delay_sum
                   average_delay_base = 0,
                   % initialized to 0, set to the first raw delay sample
                   % each sample that's added to current_delay_sum
                   % is subtracted from the value first, to make it
                   % a delay relative to this sample
                   average_sample_time = 0,
                   % the next time we should add an average delay sample into average_delay_hist
                   clock_drift = 0,
                   % the estimated clock drift between our computer
                   % and the endpoint computer. The unit is microseconds
                   % per 5 seconds
                   ssthresh = ?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE,
                   slow_start = true,
                   brust = false,
                   inbuf,
                   outbuf,
                   inque,
                   outque}).
