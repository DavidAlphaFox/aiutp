-module(aiutp_pcb).
-include("aiutp.hrl").

-export([new/2,
         state/1,
         swap_socket/1,
         process/2,
         check_timeouts/1,
         write/2,
         close/1,
         read/1,
         connect/1,
         accept/1,
         closed/1,
         flush/1]).

new(ConnIdRecv,ConnIdSend)->
  CurMilli = aiutp_util:millisecond(),
  #aiutp_pcb{time = CurMilli,
             state = ?CS_IDLE,
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
             max_window = ?PACKET_SIZE  ,
             inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
             outbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
             inque = aiutp_queue:new(),
             outque = aiutp_queue:new()}.

state(#aiutp_pcb{state = State}) -> State.
swap_socket(#aiutp_pcb{socket = Acc} = PCB) ->
  {lists:reverse(Acc),PCB#aiutp_pcb{socket = []}}.
closed(#aiutp_pcb{state = State})
  when State == ?CS_RESET -> {closed,reset};
closed(#aiutp_pcb{state = State,
                        fin_sent = FinSent,
                        fin_sent_acked = FinSentAcked,
                        got_fin = GotFin,
                        got_fin_reached = GotFinReached,
                        cur_window_packets = CurWindowPackets
                       })
  when State == ?CS_DESTROY->
  if (FinSent and FinSentAcked) or
     (GotFin and GotFinReached) -> {closed,normal};
     FinSent and CurWindowPackets == 1 -> {closed,normal};
     (FinSent == false) and
     (GotFin == false)-> {closed,timeout};
     true -> {closed,crash}
  end;
closed(#aiutp_pcb{got_fin = GotFin,
                  got_fin_reached = GotFinReached}) ->
  if (GotFin and GotFinReached) -> {closed,normal};
     true -> not_closed
  end.

process(Packet,PCB)-> aiutp_net:schedule_ack(process(Packet#aiutp_packet.type,Packet,PCB)).

process(_,_,#aiutp_pcb{state = State} = PCB)
  when (State == ?CS_DESTROY);
       (State == ?CS_RESET) -> PCB;
process(?ST_RESET,
        #aiutp_packet{conn_id = ConnId},
        #aiutp_pcb{conn_id_send = ConnIdSend,
                   conn_id_recv = ConnIdRecv,
                   close_requested = CloseRequested} = PCB)->
  if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
      if CloseRequested == true -> PCB#aiutp_pcb{state = ?CS_DESTROY};
         true -> PCB#aiutp_pcb{state = ?CS_RESET}
      end;
     true-> PCB
  end;
%% 处理SYN
process(?ST_SYN,
        #aiutp_packet{seq_nr = AckNR},
        #aiutp_pcb{state = ?CS_IDLE} = PCB) ->
  SeqNR = aiutp_util:bit16_random(),
  PCB0 = PCB#aiutp_pcb{state = ?CS_SYN_RECV,
                       ack_nr = AckNR,
                       seq_nr = SeqNR,
                       fast_resend_seq_nr = SeqNR,
                       last_got_packet = aiutp_util:millisecond()},
  aiutp_net:send_ack(PCB0);
process(?ST_SYN,
        #aiutp_packet{seq_nr = AckNR},
        #aiutp_pcb{state = ?CS_SYN_RECV,ack_nr = AckNR} = PCB) ->
  PCB0 = PCB#aiutp_pcb{last_got_packet = aiutp_util:millisecond()},
  aiutp_net:send_ack(PCB0);

%% 处理所有非RESET和非SYN
process(_,
        #aiutp_packet{ack_nr = PktAckNR}=Packet,
        #aiutp_pcb{seq_nr = SeqNR,cur_window_packets = CurWindowPackets} = PCB)->

      % window packets size is used to calculate a minimum
      % permissible range for received acks. connections with acks falling
      % out of this range are dropped
  CurrWindow = ?MAX(CurWindowPackets + ?ACK_NR_ALLOWED_WINDOW,?ACK_NR_ALLOWED_WINDOW),
  if (?WRAPPING_DIFF_16((SeqNR - 1), PktAckNR) < 0) or
     (?WRAPPING_DIFF_16(PktAckNR, (SeqNR -1 -CurrWindow)) < 0) -> PCB;
      % ignore packets whose ack_nr is invalid. This would imply a spoofed address
      % or a malicious attempt to attach the uTP implementation.
      % acking a packet that hasn't been sent yet!
      % SYN packets have an exception, since there are no previous packets

     true -> process_packet(Packet,PCB)
  end.

process_packet(#aiutp_packet{type = PktType,seq_nr = PktSeqNR} = Packet,
               #aiutp_pcb{state = State} = PCB)->
  Now = aiutp_util:millisecond(),
  PCB0 =
    if State == ?CS_SYN_SENT ->
        % if this is a syn-ack, initialize our ack_nr
        % to match the sequence number we got from the other end
        PCB#aiutp_pcb{ack_nr =  aiutp_util:bit16(PktSeqNR - 1),
                      last_got_packet = Now,time = Now};
       true -> PCB#aiutp_pcb{last_got_packet = Now,time = Now}
    end,
  %% 处理超出reorder范围的Packet
  DiffSeqNR =  aiutp_util:bit16(PktSeqNR - PCB0#aiutp_pcb.ack_nr -1),
% seqnr is the number of packets past the expected
% packet this is. ack_nr is the last acked, seq_nr is the
% current. Subtracring 1 makes 0 mean "this is the next
% expected packet".
  if DiffSeqNR >= ?REORDER_BUFFER_MAX_SIZE ->
      if (DiffSeqNR >= (?SEQ_NR_MASK + 1 - ?REORDER_BUFFER_MAX_SIZE)) and
         PktType /= ?ST_STATE -> PCB0#aiutp_pcb{ida = true};
         true -> PCB0
      end;
     true -> process_packet_1(Packet,PCB0)
  end.

%% 计算dulpicateAck
process_packet_1(#aiutp_packet{type = PktType,ack_nr = PktAckNR } = Packet,
                 #aiutp_pcb{cur_window_packets = CurWindowPackets,
                            duplicate_ack = DuplicateAck,
                            seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0->
  Seq = aiutp_util:bit16(SeqNR -CurWindowPackets -1),
  if (PktAckNR == Seq) and
     PktType == ?ST_STATE ->
      process_packet_2(Packet,PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1});
     true ->
      process_packet_2(Packet,PCB#aiutp_pcb{duplicate_ack = 0})
  end;
% if we get the same ack_nr as in the last packet
% increase the duplicate_ack counter, otherwise reset it to 0.
% It's important to only count ACKs in ST_STATE packets. Any other
% packet (primarily ST_DATA) is likely to have been sent because of the
% other end having new outgoing data, not in response to incoming data.
% For instance, if we're receiving a steady stream of payload with no
% outgoing data, and we suddently have a few bytes of payload to send (say,
% a bittorrent HAVE message), we're very likely to see 3 duplicate ACKs
% immediately after sending our payload packet. This effectively disables
% the fast-resend on duplicate-ack logic for bi-directional connections
% (except in the case of a selective ACK). This is in line with BSD4.4 TCP
% implementation.

process_packet_1(Packet,PCB)-> process_packet_2(Packet,PCB).

caculate_acked_bytes(Acc,Now,AckedPackets,SAckedPackets)->
  Fun = fun(WrapPacket,{Bytes,RTT})->
            TimeSent = WrapPacket#aiutp_packet_wrap.time_sent,
            RTT0 =
              if TimeSent < Now -> ?MIN(RTT,(Now - TimeSent));
                 true -> ?MIN(RTT,50000)
              end,
            {Bytes + WrapPacket#aiutp_packet_wrap.payload,RTT0}
        end,
  Acc0 = lists:foldl(Fun,Acc, AckedPackets),
  lists:foldl(Fun, Acc0, SAckedPackets).


cc_control(Now,AckedBytes,RTT,
           #aiutp_pcb{our_hist = OurHist,target_delay = TargetDelay,
                      clock_drift = ClockDrift,max_window = MaxWindow,
                      last_maxed_out_window = LastMaxedOutWindow,
                      slow_start = SlowStart,ssthresh = SSThresh} = PCB)->
  OurHistValue = aiutp_delay:value(OurHist),
  OurDelay = ?MIN(aiutp_util:bit32(RTT),OurHistValue),
  Target =
    if TargetDelay =< 0 -> 100000;
       true -> TargetDelay
    end,
  Penalty =
    if ClockDrift <  -200000 -> (200000 + ClockDrift) div 7;
       true -> 0
    end,
  OurDelay0 = OurDelay + Penalty,
  OffTarget = Target - OurDelay0,
  Win0 = ?MIN(AckedBytes,MaxWindow),
  Win1 = ?MAX(AckedBytes,MaxWindow),
  WindowFactor = Win0 / Win1,
  DelayFactor = OffTarget / Target,
  ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,
  ScaledGain0 =
    if (ScaledGain > 0) and (Now - LastMaxedOutWindow > 1000) -> 0;
       true -> ScaledGain
    end,
  LedbetCwnd = math:ceil(?MAX(?MIN_WINDOW_SIZE,(MaxWindow + ScaledGain0))),
  {SlowStart0,SSThresh0,MaxWindow0} =
    if SlowStart ->
        SSCwnd = math:ceil(MaxWindow + WindowFactor* ?PACKET_SIZE),
        if SSCwnd > SSThresh-> {false,SSThresh,MaxWindow};
           OurDelay0  > Target * 0.9 -> {false,MaxWindow,MaxWindow};
           true -> {SlowStart,SSThresh,?MAX(SSCwnd,LedbetCwnd)}
        end;
       true -> {SlowStart,SSThresh,LedbetCwnd}
    end,
  PCB#aiutp_pcb{slow_start = SlowStart0,ssthresh = SSThresh0,
                max_window = aiutp_util:clamp(MaxWindow0,?MIN_WINDOW_SIZE,?OUTGOING_BUFFER_MAX_SIZE*?PACKET_SIZE)}.


ack_packet(MicroNow,#aiutp_packet_wrap{transmissions = Transmissions,
                              time_sent = TimeSent,
                         need_resend = NeedResend,payload = Payload},
                 #aiutp_pcb{time = Now,
                            cur_window = CurWindow,
                            rtt = RTT,rto = RTO,
                            rtt_var = RTTVar,rtt_hist = RTTHist} = PCB)->
  {RTT1,RTTVar1,RTO0,RTTHist1} =
    if Transmissions == 1 ->
        {RTT0,RTTVar0,ERTT} = aiutp_rtt:caculate_rtt(RTT,RTTVar,TimeSent,MicroNow),
        RTTHist0 =
          if RTT /= 0 -> aiutp_delay:add_sample(ERTT,Now,RTTHist);
             true -> RTTHist
          end,
        {RTT0,RTTVar0,?MAX((RTT0 + RTTVar0 * 4),1000),RTTHist0};
       true -> {RTT,RTTVar,RTO,RTTHist}
  end,
  CurWindow0 =
    if NeedResend == false -> CurWindow - Payload;
       true -> CurWindow
    end,
  PCB#aiutp_pcb{
    cur_window = CurWindow0,
    rtt = RTT1,rtt_var = RTTVar1,
    rtt_hist = RTTHist1, rto = RTO0,retransmit_count = 0,
    retransmit_timeout = RTO0, rto_timeout = RTO0 + Now}.


maybe_decay_win(#aiutp_pcb {time = Now,
                            max_window = MaxWindow,
                            last_rwin_decay = LastRWinDecay
                           } = PCB)->
  if (Now - LastRWinDecay) < ?MAX_WINDOW_DECAY -> PCB;
     true ->
      MaxWindow0 = math:ceil(MaxWindow * 0.5),
      MaxWindow1 =
        if MaxWindow0 < ?MIN_WINDOW_SIZE -> ?MIN_WINDOW_SIZE;
           true -> MaxWindow0
        end,
      PCB#aiutp_pcb{
        slow_start = false,
        ssthresh = MaxWindow1,
        max_window = MaxWindow1,
        last_rwin_decay = Now
       }
  end.


%% 此处实现和C++版本有差异，会浪费带宽，但是不是Bug
%% 需要在后期进行优化
selective_ack_packet(_,_,#aiutp_pcb{cur_window_packets = CurWindowPackets} = PCB)
  when CurWindowPackets  == 0-> PCB;
selective_ack_packet([],_,PCB)-> PCB;
selective_ack_packet(SAckedPackets,
                     MicroNow,
                     #aiutp_pcb{fast_resend_seq_nr = MinSeq} = PCB)->
  PCB0 = lists:foldr(fun(I,Acc) -> ack_packet(MicroNow,I,Acc) end, PCB, SAckedPackets),
  SSAckeds = erlang:length(SAckedPackets),
  %% 计算出重发最大的序列号
  MaxSeq =
    if SSAckeds > ?DUPLICATE_ACKS_BEFORE_RESEND ->
        El = lists:nth(?DUPLICATE_ACKS_BEFORE_RESEND, SAckedPackets),
        Packet = El#aiutp_packet_wrap.packet,
        Packet#aiutp_packet.seq_nr;
       true -> MinSeq
    end,
  if ?WRAPPING_DIFF_16(MaxSeq,MinSeq) > 0 ->
      {Sent,LastSeq,PCB1} = aiutp_net:send_n_packets(MinSeq, MaxSeq, 4, PCB0),
      PCB2 = PCB1#aiutp_pcb{fast_resend_seq_nr = aiutp_util:bit16(LastSeq + 1),
                            duplicate_ack = SSAckeds},
      if Sent > 0 -> maybe_decay_win(PCB2);
         true ->  PCB2
      end;
     true -> PCB0
  end.

%% 流控和重传
process_packet_2(#aiutp_packet{type = PktType,ack_nr = PktAckNR,
                               wnd = PktMaxWindowUser} = Packet,
                 #aiutp_pcb{state = State,
                            time = Now,
                            fast_resend_seq_nr = FastResendSeqNR,
                            fast_timeout = FastTimeout,
                            zerowindow_time = ZeroWindowTime,
                            fin_sent = FinSent,close_requested = CloseRequested,
                            fin_sent_acked = FinSentAcked} = PCB)->
  MicroNow = aiutp_util:microsecond(),
  {AckedPackets,SAckedPackets,PCB0} = aiutp_tx:pick_acked(Packet,PCB),
  {AckedBytes,MinRTT} = caculate_acked_bytes({0,?RTT_MAX},Now,AckedPackets,SAckedPackets),
  {ActualDelay,PCB1} = aiutp_rtt:caculate_delay(Now,MicroNow,Packet,PCB0),
  OurHist = PCB#aiutp_pcb.our_hist,
  OurHistValue = aiutp_delay:value(OurHist),
  OurHist0 =
    if OurHistValue > MinRTT -> aiutp_delay:shift(aiutp_util:bit32(OurHistValue - MinRTT),OurHist);
       true -> OurHist
    end,
  PCB2 =
    if (ActualDelay /= 0) and (AckedBytes > 0) ->
        cc_control(Now,AckedBytes,MinRTT,PCB1#aiutp_pcb{our_hist = OurHist0});
       true-> PCB1#aiutp_pcb{our_hist = OurHist0}
    end,

  ZeroWindowTime0 =
    if PktMaxWindowUser == 0 -> Now + 15000;
       true -> ZeroWindowTime
    end,
  State0 =
    if (PktType == ?ST_DATA) and
       (State == ?CS_SYN_RECV) -> ?CS_CONNECTED;
       true -> State
    end,
    {State1,FinSentAcked0} =
    if (PktType == ?ST_STATE) and
       (State0 == ?CS_SYN_SENT) -> {?CS_CONNECTED,false};
       (FinSent == true) and (PCB2#aiutp_pcb.cur_window_packets == 0) ->
        if CloseRequested -> {?CS_DESTROY,true};
           true -> {State0,true}
        end;
       true -> {State0,FinSentAcked}
    end,

  FastResendSeqNR0 =
    if ?WRAPPING_DIFF_16(FastResendSeqNR,((PktAckNR + 1) band 16#FFFF)) < 0 -> ((PktAckNR + 1) band 16#FFFF);
       true -> FastResendSeqNR
    end,
  PCB3 = lists:foldr(fun(I,AccPCB) -> ack_packet(MicroNow,I,AccPCB) end,
                     PCB2#aiutp_pcb{
                       max_window_user = PktMaxWindowUser,
                       state = State1,
                       fin_sent_acked = FinSentAcked0,
                       fast_resend_seq_nr = FastResendSeqNR0,
                       zerowindow_time = ZeroWindowTime0
                      }, AckedPackets),
  PCB4 =
    if PCB3#aiutp_pcb.cur_window_packets == 1 ->
        aiutp_net:send_packet(aiutp_buffer:head(PCB3#aiutp_pcb.outbuf),PCB3);
       true -> PCB3
    end,
  PCB5 =
    if FastTimeout ->
        if ?WRAPPING_DIFF_16(PCB4#aiutp_pcb.seq_nr, PCB4#aiutp_pcb.cur_window_packets) /= FastResendSeqNR0 ->
            PCB4#aiutp_pcb{fast_timeout = false};
           true -> aiutp_net:send_packet(aiutp_buffer:head(PCB4#aiutp_pcb.outbuf),PCB4)
        end;
       true-> PCB4
    end,
  PCB6  = selective_ack_packet(SAckedPackets,MicroNow,PCB5),
  process_packet_3(Packet, PCB6).



%% 处理ST_STATE以及CS_CONNECTED状态确认
process_packet_3(#aiutp_packet{type = PktType} = Packet,
                 #aiutp_pcb{state = State} = PCB)->

  {ISFull,PCB0} = aiutp_net:is_full(-1,PCB),
  PCB1 =
    if (ISFull == false) and
       (State == ?CS_CONNECTED_FULL) -> PCB0#aiutp_pcb{state = ?CS_CONNECTED};
       true -> PCB0
    end,
  if PktType == ?ST_STATE-> PCB1;
     (State /= ?CS_CONNECTED)  and (State /= ?CS_CONNECTED_FULL)-> PCB1;
     true -> process_packet_4(Packet, PCB1)
end.

process_packet_4(#aiutp_packet{type = PktType,seq_nr = PktSeqNR} = Packet,
                 #aiutp_pcb{got_fin = GotFin} = PCB)->

  PCB0 =
    if (PktType == ?ST_FIN) and (GotFin == false)->
        PCB#aiutp_pcb{got_fin = true,eof_pkt = PktSeqNR};
       true -> PCB
    end,
  aiutp_rx:in(Packet, PCB0).

check_timeouts(#aiutp_pcb{state = State} = PCB)
  when State /= ?CS_DESTROY ->
  Now = aiutp_util:millisecond(),
  PCB0 = aiutp_net:flush_packets(PCB),
  check_timeouts_0(PCB0#aiutp_pcb{time = Now});
check_timeouts(PCB) -> PCB.

check_timeouts_0(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_UNINITIALIZED ;
       State == ?CS_IDLE;
       State == ?CS_RESET-> PCB;
check_timeouts_0(#aiutp_pcb{time =Now,
                            state = State,
                            zerowindow_time = ZeroWindowTime,
                            max_window_user = MaxWindowUser,
                            rto_timeout = RTOTimeout,
                            fin_sent = FinSent
                           } = PCB)->
  PCB0 =
    if (MaxWindowUser == 0 ) and
       (Now - ZeroWindowTime >=0) -> PCB#aiutp_pcb{max_window_user = ?PACKET_SIZE};
       true -> PCB
    end,
  {Continue,PCB1} =
    if (RTOTimeout >0) and
       (Now - RTOTimeout >= 0 ) ->
        check_timeouts_1(PCB0);
       true -> {true,PCB0}
    end,
  if Continue == true ->
      {ISFull,PCB2} = aiutp_net:is_full(-1,PCB1),
      PCB3 =
        if (State == ?CS_CONNECTED_FULL) and
           (ISFull == false) ->PCB2#aiutp_pcb{state = ?CS_CONNECTED};
           true -> PCB2
        end,
      if (FinSent == false) and
         ((State == ?CS_CONNECTED) or (State == ?CS_CONNECTED_FULL)) and
         (Now - PCB3#aiutp_pcb.last_sent_packet >= ?KEEPALIVE_INTERVAL) ->
          aiutp_net:send_keep_alive(PCB3);
         true -> PCB3
      end;
     true -> PCB1
  end.

mark_need_resend(_,CurWindow,-1,OutBuf)-> {CurWindow,OutBuf};
mark_need_resend(0,CurWindow,_,OutBuf)-> {CurWindow,OutBuf};
mark_need_resend(CurWindowPackets,CurWindow,Iter,OutBuf) ->
  Next = aiutp_buffer:next(Iter, OutBuf),
  WrapPacket = aiutp_buffer:data(Iter, OutBuf),
  #aiutp_packet_wrap{
     transmissions = Transmissions,
     need_resend = NeedResend,
     payload = Payload
    } = WrapPacket,
  if (NeedResend == true) or (Transmissions == 0) ->
      mark_need_resend(CurWindowPackets - 1,CurWindow,Next,OutBuf);
     true ->
      WrapPacket0 = WrapPacket#aiutp_packet_wrap{need_resend = true},
      OutBuf0 = aiutp_buffer:replace(Iter,WrapPacket0,OutBuf),
      mark_need_resend(CurWindowPackets - 1,CurWindow - Payload,Next,OutBuf0)
  end.


check_timeouts_1(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
  {fasle,PCB#aiutp_pcb{state = ?CS_DESTROY}};
check_timeouts_1(#aiutp_pcb{time = Now,
                            last_got_packet = LastGotPacket,
                            close_requested = CloseRequested} = PCB)
  when (LastGotPacket > 0),(Now - LastGotPacket > ?KEEPALIVE_INTERVAL * 1.5) ->
  if CloseRequested == true -> {false,PCB#aiutp_pcb{state = ?CS_DESTROY}};
     true ->  {false,PCB#aiutp_pcb{state = ?CS_RESET}}
  end;

check_timeouts_1(#aiutp_pcb{state = State,
                            close_requested = CloseRequested,
                            retransmit_count = RetransmitCount} = PCB)
  when (RetransmitCount >= 4);
       ((State == ?CS_SYN_SENT) and RetransmitCount > 2) ->
  if CloseRequested == true -> {false,PCB#aiutp_pcb{state = ?CS_DESTROY}};
     true ->  {false,PCB#aiutp_pcb{state = ?CS_RESET}}
  end;
check_timeouts_1(#aiutp_pcb{state = State,
                            close_requested = CloseRequested,
                            retransmit_count = RetransmitCount} = PCB)
  when (RetransmitCount >= 4);
       ((State == ?CS_SYN_SENT) and RetransmitCount > 2) ->
  if CloseRequested == true -> {false,PCB#aiutp_pcb{state = ?CS_DESTROY}};
     true ->  {false,PCB#aiutp_pcb{state = ?CS_RESET}}
  end;
check_timeouts_1(#aiutp_pcb{time=Now,
                            retransmit_timeout = RetransmitTimeout,
                            cur_window_packets = CurWindowPackets,
                            cur_window = CurWindow,
                            max_window = MaxWindow,
                            outbuf = OutBuf,
                            seq_nr = SeqNR,
                            retransmit_count = RetransmitCount
                           } = PCB) ->
  NewTimeout = RetransmitTimeout * 2,

  PCB0 =
    if (CurWindowPackets == 0) and
       (MaxWindow > ?PACKET_SIZE) ->
        PCB#aiutp_pcb{retransmit_timeout = NewTimeout,rto_timeout = Now + NewTimeout,
                      duplicate_ack = 0,
                      max_window = math:ceil(?MAX((MaxWindow * 2 / 3), ?PACKET_SIZE))};
       true -> PCB#aiutp_pcb{retransmit_timeout = NewTimeout,rto_timeout = Now + NewTimeout,
                                duplicate_ack = 0,
                                max_window = ?PACKET_SIZE ,slow_start = true}
    end,
  if CurWindowPackets > 0 ->
      Iter = aiutp_buffer:head(OutBuf),
      {CurWindow0,OutBuf0} = mark_need_resend(CurWindowPackets,CurWindow,Iter,OutBuf),
      PCB1 = PCB0#aiutp_pcb{
               cur_window = CurWindow0,
               outbuf = OutBuf0,
               retransmit_count = RetransmitCount + 1,
               fast_timeout = true,
               timeout_seq_nr = SeqNR
              },
      {true,aiutp_net:send_packet(aiutp_buffer:head(OutBuf0), PCB1)};
     true -> {true,PCB0}
  end.
write(_,#aiutp_pcb{state = State} = PCB)
  when (State /= ?CS_CONNECTED),
       (State /= ?CS_CONNECTED_FULL) -> {{error,not_connected},PCB};
write(_,#aiutp_pcb{fin_sent = FinSent} = PCB)
  when FinSent == true -> {{error,closed},PCB};
write(Data,PCB) -> aiutp_tx:in(Data,PCB).

close(#aiutp_pcb{state = State } = PCB)
  when State == ?CS_UNINITIALIZED;
       State == ?CS_IDLE;
       State == ?CS_DESTROY -> PCB#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{state = State,rto = RTO} = PCB)
  when State == ?CS_SYN_SENT ->
  PCB#aiutp_pcb{
    rto_timeout = ?MIN(RTO * 2, 60) + aiutp_util:millisecond(),
    state = ?CS_DESTROY};
close(#aiutp_pcb{state = State} = PCB)
  when State == ?CS_SYN_RECV ->
  PCB#aiutp_pcb{state = ?CS_DESTROY};
close(#aiutp_pcb{fin_sent_acked = FinSentAcked,fin_sent = FinSent} = PCB)->
  PCB0 = PCB#aiutp_pcb{read_shutdown = true,close_requested = true},
  if FinSent == false ->
      aiutp_net:send_fin(PCB0#aiutp_pcb{fin_sent = true});
     FinSentAcked == true -> PCB0#aiutp_pcb{state = ?CS_DESTROY};
     true -> PCB0
  end.
read(#aiutp_pcb{inque = InQue,max_window = MaxWindow,
                inbuf = InBuf,last_rcv_win = LastRcvWin} = PCB)->
  L = aiutp_queue:to_list(InQue),
  WindowSize = aiutp_net:window_size(MaxWindow, InBuf),
  PCB0 =
    if WindowSize > LastRcvWin->
        if LastRcvWin == 0 -> aiutp_net:send_ack(PCB);
           true ->
            Now = aiutp_util:millisecond(),
            PCB#aiutp_pcb{time=Now,ida = true}
        end;
       true -> PCB
    end,
  QueSize = aiutp_queue:size(InQue),
  if QueSize > 0 ->
      {lists:foldl(
         fun(Bin,Acc) -> <<Acc/binary,Bin/binary>> end,
         <<>>,L),PCB0#aiutp_pcb{inque = aiutp_queue:new()}};
     true -> {undefined,PCB0}
  end.

connect(ConnIdRecv)->
  ConnIdSend = aiutp_util:bit16(ConnIdRecv + 1),
  PCB = new(ConnIdRecv,ConnIdSend),
  #aiutp_pcb{max_window = MaxWindow,inbuf = InBuf,
             conn_id_recv = ConnId,outbuf = OutBuf} = PCB,
  Now = aiutp_util:millisecond(),
  SeqNR = aiutp_util:bit16_random(),
  WindowSize = aiutp_net:window_size(MaxWindow, InBuf),
  Packet = aiutp_packet:syn(SeqNR),
  Packet0 = Packet#aiutp_packet{conn_id = ConnId,wnd = WindowSize,
                                seq_nr = SeqNR},
  WrapPacket = #aiutp_packet_wrap{packet = Packet0},
  OutBuf0 = aiutp_buffer:append(WrapPacket, OutBuf),
  Iter = aiutp_buffer:head(OutBuf0),
  PCB0 = PCB#aiutp_pcb{state = ?CS_SYN_SENT,
                       time=Now,
                       retransmit_timeout = 3000,
                       rto_timeout = 3000 + Now,
                       last_rcv_win = WindowSize,
                       outbuf = OutBuf0, cur_window_packets = 1,
                       seq_nr = SeqNR + 1},
  aiutp_net:send_packet(Iter, PCB0).

accept(#aiutp_packet{conn_id = ConnIdSend} = Packet)->
  ConnIdRecv = aiutp_util:bit16(ConnIdSend + 1),
  PCB = new(ConnIdRecv,ConnIdSend),
  PCB1 = process(Packet,PCB),
  {ConnIdRecv,PCB1}.

flush(PCB)-> aiutp_net:flush_queue(PCB).
