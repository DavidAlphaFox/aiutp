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
-define(PACKET_SIZE,1435).
-define(MIN_WINDOW_SIZE,10).
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

-define(DIV_ROUND_UP(NUM,DENOM),((NUM + DENOM - 1) div DENOM)).
-define(MIN(F, S), case F < S of true -> F; false -> S end).
-define(MAX(F, S), case F < S of true -> S; false -> F end).
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

-record(aiutp_packet, {type           ::  st_data | st_fin | st_state | st_reset | st_syn,
                       conn_id        :: integer(), % 会话ID
                       wnd = 0        :: integer(), % 我们的窗口
                       seq_no         :: integer(), % 我们的序号
                       ack_no         :: integer(), % 确认的序号
                       extension = [] :: [{sack, binary()} | {ext_bits, binary()}],
                       payload = <<>> :: binary()
                    }).
-define(aiutp_packet_wrap,{packet,
                           time_sent = 0, %microsecond
                           transmissions = 0,
                           need_resend = 0}).
