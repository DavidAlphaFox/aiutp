-module(aiutp_pcb).
-include("aiutp.hrl").
-export([new/2,process/2]).

new(ConnIdRecv,ConnIdSend)->
  CurMilli = aiutp_util:millisecond(),
  #aiutp_pcb{
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
     max_window = ?AIUTP_MTU_DEF - ?UTP_HEADER_SIZE,
     inbuf = aiutp_buffer:new(?REORDER_BUFFER_MAX_SIZE + 1),
     outbuf = aiutp_buffer:new(?REORDER_BUFFER_MAX_SIZE + 1)
    }.


curr_window(#aiutp_pcb{cur_window_packets = CurWindowPackets})->
  ?MAX(CurWindowPackets + ?ACK_NR_ALLOWED_WINDOW,?ACK_NR_ALLOWED_WINDOW).
% window packets size is used to calculate a minimum
% permissible range for received acks. connections with acks falling
% out of this range are dropped


diff_seq_nr(PktSeqNR,#aiutp_pcb{ack_nr = AckNR})->
  aiutp_util:bit16(PktSeqNR - AckNR -1).
% seqnr is the number of packets past the expected
% packet this is. ack_nr is the last acked, seq_nr is the
% current. Subtracring 1 makes 0 mean "this is the next
% expected packet".

process(Packet,PCB)-> process([],Packet#aiutp_packet.type,Packet,PCB).

process(Acc,_,Packet,#aiutp_pcb{state = State} = PCB)
  when (State == ?CS_DESTROY);
       (State == ?CS_RESET) ->
  {Acc,PCB};
process(Acc,?ST_RESET,
        #aiutp_packet{conn_id = ConnId},
        #aiutp_pcb{conn_id_send = ConnIdSend,
                   conn_id_recv = ConnIdRecv,
                   close_requested = CloseRequested} = PCB)->
  if (ConnIdSend == ConnId) or (ConnIdRecv == ConnId) ->
      if CloseRequested == true -> {Acc,PCB#aiutp_pcb{state = ?CS_DESTROY}};
         true -> {Acc,PCB#aiutp_pcb{state = ?CS_RESET}}
      end;
     true-> {Acc,PCB}
  end;
%% 处理SYN
process(Acc,?ST_SYN,
        #aiutp_packet{seq_nr = AckNR},
        #aiutp_pcb{state = ?CS_IDLE} = PCB) ->
  PCB0 = PCB#aiutp_pcb{state = ?CS_SYN_RECV,
                       ack_nr = AckNR,
                       seq_nr = aiutp_util:bit16_random(),
                       last_got_packet = aiutp_util:millisecond()},
  aiutp_net:send_ack(Acc,PCB0);
process(Acc,?ST_SYN,
        #aiutp_packet{seq_nr = AckNR},
        #aiutp_pcb{state = ?CS_SYN_RECV,ack_nr = AckNR} = PCB) ->
  PCB0 = PCB#aiutp_pcb{last_got_packet = aiutp_util:millisecond()},
  aiutp_net:send_ack(Acc, PCB0);

%% 处理所有非RESET和非SYN
process(Acc,_,
        #aiutp_packet{seq_nr = PktSeqNR,ack_nr = PktAckNR}=Packet,
        #aiutp_pcb{seq_nr = SeqNR,ack_nr = AckNR} = PCB)->
  CurrWindow = curr_window(PCB),
  if (?WRAPPING_DIFF_16((SeqNR - 1), PktAckNR) < 0) or
     (?WRAPPING_DIFF_16(PktAckNR, (SeqNR -1 -CurrWindow)) < 0) ->
      % ignore packets whose ack_nr is invalid. This would imply a spoofed address
      % or a malicious attempt to attach the uTP implementation.
      % acking a packet that hasn't been sent yet!
      % SYN packets have an exception, since there are no previous packets
      {Acc,PCB};
     true -> process_packet(Acc,Packet,PCB)
  end.

process_packet(Acc,
               #aiutp_packet{type = PktType,seq_nr = PktSeqNR} = Packet,
               #aiutp_pcb{state = State} = PCB)->
  Now = aiutp_util:millisecond(),
  MircoNow = aiutp_util:microsecond(),
  PCB0 =
    if State == ?CS_SYN_SENT ->
        % if this is a syn-ack, initialize our ack_nr
        % to match the sequence number we got from the other end
        PCB#aiutp_pcb{ack_nr =  aiutp_util:bit16(PktSeqNR - 1),
                      last_got_packet = Now,time = {Now,MicroNow}};
       true -> PCB#aiutp_pcb{last_got_packet = Now,time = {Now,MicroNow}}
    end,
  %% 处理超出reorder范围的Packet
  DiffSeqNR = diff_seq_nr(PktSeqNR,PCB0),
  if DiffSeqNR >= ?REORDER_BUFFER_MAX_SIZE ->
      if (DiffSeqNR >= (?SEQ_NR_MASK + 1 - ?REORDER_BUFFER_MAX_SIZE)) and
         PktType /= ?ST_STATE -> aiutp_net:send_ack(Acc, PCB0);
         true -> {Acc,PCB0}
      end;
     true -> process_packet_1(Acc,Packet,PCB0)
  end.

%% 计算dulpicateAck
process_packet_1(Acc,
                 #aiutp_packet{type = PktType,ack_nr = PktAckNR } = Packet,
                 #aiutp_pcb{cur_window_packets = CurWindowPackets,
                            duplicate_ack = DuplicateAck,
                            seq_nr = SeqNR} = PCB)
  when CurWindowPackets > 0->
  if (PktAckNR == aiutp_util:bit16(SeqNR -CurWindowPackets -1)) and
     PktType == ?ST_STATE ->
      process_packet_2(Acc,Packet,
                       PCB#aiutp_pcb{duplicate_ack = DuplicateAck + 1});
     true ->
      process_packet_2(Acc,Packet,
                       PCB#aiutp_pcb{duplicate_ack = 0})
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

process_packet_1(Acc,Packet,PCB)-> process_packet_2(Acc,Packet,PCB).

acks(PktAckNR,#aiutp_pcb{seq_nr = SeqNR,
                         cur_window_packets = CurWindowPackets})->
  Acks = aiutp_util:bit16(PkgAckNR - (SeqNR - 1 - CurWindowPackets)),
  if Acks > CurWindowPackets -> 0; % this happens when we receive an old ack nr
     true -> Acks
  end.
% Process acknowledgment acks is the number of packets that was acked

caculate_bytes_rtt({Bytes,RTT},WrapPacket,Now)->
  TimeSent = WrapPacket#aiutp_packet_wrap.time_sent,
  RTT0 =
    if TimeSent < Now -> ?MIN(RTT,(Now - TimeSent));
       true -> ?MIN(RTT,50000)
    end,
  {Bytes + WrapPacket#aiutp_packet_wrap.payload,RTT0}.


acked_bytes(Acc,Iter,Now,MaxSeq,OutBuf)->
  Iter0 = aiutp_buffer:next(Iter, OutBuf),
  WrapPacket = aiutp_buffer:data(Iter, OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  if Packet#aiutp_packet.seq_nr >= MaxSeq -> Acc;
     true ->
      Acc0 = caculate_bytes_rtt(Acc,WrapPacket,Now),
      acked_bytes(Acc0,Iter0,Now,MaxSeq,OutBuf)
  end.

sack_seq(<<>>,_,_,Acc)-> Acc;
sack_seq(<<Bits/big-unsigned-integer,Rest/binary>>,Index,Base,Acc) ->
  if Bits == 0 -> sack_seq(Rest,Index+ 1,Base,Acc);
     true ->
      Offset = Index * 7,
      Acc1 = list:foldl(fun(I,Acc0)->
                            Hint = Bits band (1 bsl I),
                            if Hint > 0 -> [(Base + Offset + I)|Acc0];
                               true -> Acc0
                            end
                        end,Acc,lists:seq(0, 7)),
      sack_seq(Rest,Index+1,Base,Acc1)
  end.
sack_seq([],_) -> [];
sack_seq([{sack,Bits}|T],Base)-> sack_seq(Bits,0,Base,[]);
sack_seq([H|T],Base) -> sack_seq(T,Base).

sacked_bytes({Bytes,RTT} = Acc,SAcks,Iter,Now,MaxSeq,OutBuf)->
  Iter0 = aiutp_buffer:next(Iter, OutBuf),
  WrapPacket = aiutp_buffer:data(Iter, OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  if Packet#aiutp_packet.seq_nr >= MaxSeq -> Acc;
     true ->
      Member = lists:member(Packet#aiutp_packet.seq_nr, SAcks),
      if Member->
          Acc0 = caculate_bytes_rtt(Acc,WrapPacket,Now),
          sacked_bytes(Acc,SAcks,Iter0,Now,MaxSeq,OutBuf);
         true -> Acc
      end
  end.

sacked_bytes(Acc,[],_,_) -> Acc;
sacked_bytes(Acc,[MaxSeq|_] = SAcks,Now,OutBuf) ->
  Iter = aiutp_buffer:head(OutBuf),
  sacked_bytes(Acc,SAcks,Iter,Now,MaxSeq,OutBuf).


caculate_acked_bytes(Now,#aiutp_packet{ack_nr = PktAckNR,extension = Exts },
      #aiutp_pcb{cur_window_packets = CurWindowPackets,
                 seq_nr = SeqNR,outbuf = OutBuf} = PCB)->
  %% SeqNR = 6 CurWindowPackets = 5 AckNR = 3
  %% Acks = 3 SeqBase = 1 , MaxSeq = 4
  %% SeqNo 为4的包对面没收到
  Acks = acks(PktAckNR,PCB),
  SeqBase = aiutp_util:bit16(SeqNR - CurWindowPackets),
  MaxSeq = aiutp_util:bit16(SeqBase + Acks),
  Iter = aiutp_buffer:head(OutBuf),
  AckResult = acked_bytes({0,?RTT_MAX},Iter,Now,MaxSeq,OutBuf),
  SAcks = sack_seq(Exts,PktAckNR + 2),
  sacked_bytes(AckResult,SAcks,Now,OutBuf).

caculate_delay(Now,MicroNow,
               #aiutp_packet{tv_usec = TS, reply_micro = TSDiff},
               #aiutp_pcb{their_hist = TheirHist,our_hist = OurHist,
                          current_delay_sum = CurrentDelaySum,
                          current_delay_samples = CurrentDelaySamples,
                          average_delay = PrevAverageDelay,
                          average_sample_time = AverageSampleTime,
                          average_delay_base = AverageDelayBase} = PCB)->
  TheirDelay =
    if TS > 0 -> NowMirco - TS;
       true -> 0
    end,
  PrevDelayBase = aiutp_delay:delay_base(TheirHist),
  TheirHist0 =
    if TheirDelay > 0 -> aiutp_delay:add_sample(TheirDelay,Now,TheirHist);
       true -> TheirHist
    end,
  DelayBase = aiutp_delay:delay_base(TheirHist0),
  OurHist0 =
    if (DelayBase /= 0) and
        (?WRAPPING_DIFF_32(DelayBase,PrevDelayBase) < 0) ->
        DelayBaseShift = PrevDelayBase - DelayBase,
        if DelayBaseShit =< 10000 -> aiutp_delay:shift(DelayBaseShit,OurHist);
           true -> OurHist
        end;
       true -> OurHist
    end,
  ActualDelay = TSDiff band TIMESTAMP_MASK,
  PCB0 =
    if ActualDelay /= 0 ->
        OurHist1 = aiutp_delay:add_sample(ActualDelay,Now,OurHist0),
        AverageDelayBase0 =
          if AverageDelayBase == 0 -> ActualDelay;
             true -> AverageDelayBase
          end,
        DistDown = AverageDelayBase0 - ActualDelay,
        DistUp = ActualDelay - AverageDelayBase0,
        AverageDelaySample =
          if DisDown > DistUp -> DistUp;
             true -> 0  - DisDown
          end,
        CurrentDelaySum0 = CurrentDelaySum + AverageDelaySample,
        if Now > AverageSampleTime ->
            AverageDelay  = aiutp_util:bit32(CurrentDelaySum0 / (CurrentDelaySamples + 1)),
            MinSample = ?MIN(PrevAverageDelay,AverageDelay),
            MaxSample = ?MAX(PrevAverageDelay,AverageDelay),
            Adjust =
              if MinSample > 0 -> 0 - MinSample;
                 MaxSample < 0 -> 0 - MaxSample;
                 true -> 0
              end,
            {AverageDelayBase1,AverageDelay0}
              if Adjust /= 0 -> {AverageDelayBase0 - Adjust,AverageDelay + Adjust};
                 true -> {AverageDelayBase0,AverageDelay}
              end,
            PCB#aiutp_pcb{ reply_micro = TheirDelay,last_measured_delay = Now,
                           their_hist = TheirHist0,our_hist = OurHist1,
                           clock_drift = AverageDelay - PreAverageDelay,
                           average_delay_base = AverageDelayBase1,
                           average_delay = AverageDelay0,
                           average_sample_time = AverageSampleTime + 5000,
                           current_delay_sum = 0,
                           current_delay_samples = 0};
           true->
            PCB#aiutp_pcb{ reply_micro = TheirDelay,last_measured_delay = Now,
                           their_hist = TheirHist0,our_hist = OurHist1,
                           average_delay_base = AverageDelayBase0,
                           current_delay_sum = CurrentDelaySum0,
                           crrent_delay_samples = CurrentDelaySamples + 1}
        end;
       true ->
        PCB#aiutp_pcb{ reply_micro = TheirDelay,last_measured_delay = Now,
                       their_hist = TheirHist0}
    end,
  {ActualDelay,PCB0}.

cc_control(Now,AckedBytes,ActualDelay,RTT,
           #aiutp_pcb{our_hist = OurHist,target_deplay = TargetDelay,
                      clock_drift = ClockDrift,max_window = MaxWindow,
                      last_maxed_out_window = LastMaxedOutWindow,
                      slow_start = SlowStart,ssthresh = SSThresh} = PCB)->
  OurHistValue = aiutp_delay:value(OurHist),
  OurDelay = ?MIN(RTT,OurHistValue),
  Target =
    if TargetDelay =<0 -> 100000;
       true -> TargetDelay
    end,
  Penalty =
    if ClockDrift <  -200000 -> (200000 - ClockDrift) div 7;
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
  LedbatCwnd = ?MAX(?MIN_WINDOW_SIZE,(MaxWindow + ScaledGain0)),
  {SlowStart0,SSThresh0,MaxWindow0} =
    if SlowStart ->
        SSCwnd = MaxWindow + WindowFactor* (?AIUTP_MTU_DEF - ?UTP_HEADER_SIZE),
        if SSCwnd > SSThresh-> {false,SSThresh,MaxWindow};
           OurDelay0  > Target * 0.9 -> {false,MaxWindow,MaxWindow};
           true -> {SlowStart,SSThresh,?MAX(SSCwnd,LedbetCwnd)}
        end;
       true -> {SlowStart,SSThresh,LedbetCwnd}
    end,
  PCB#aiutp_pcb{slow_start = SlowStart0,ssthresh = SSThresh,
                max_window = MaxWindow}.


%% 流控
process_packet_2(Acc,
                 #aiutp_packet{ack_nr = PktAckNR} = Packet,
                 #aiutp_pcb{time = {Now,MicroNow}, cur_window_packets = CurWindowPackets} = PCB)->
  {AckedBytes,RTT} = caculate_acked_bytes(Now,PCB),
  {ActualDelay,PCB0} = caculate_delay(Now,MicroNow,Packet,PCB),
  OurHist = PCB#aiutp_pcb.our_hist,
  OurHistValue = aiutp_delay:value(OurHist),
  OurHist0 =
    if OurHistValue > RTT ->
        aiutp_deplay:shift(OurHistValue - RTT,OurHist);
       true -> OurHist
    end,
  PCB1 =
    if (ActualDelay /= 0) and (AckedBytes > 0) ->
        cc_control(Now,AckedBytes,ActualDelay,RTT,PCB0);
       true-> PCB0
    end,
  Acks = acks(PktAckNR,PCB),
  if Acks =< CurWindowPackets -> process_packet_3(Acc,Packet,PCB1);
     true -> process_packet_4(Acc,Packet,CPB1)
  end.

caculate_rtt(RTT,RTTVar,TimeSent)->
  MicroNow = aiutp_util:microseconds(),
  ERTT = aiutp_util:bit32((MicroNow - TimeSent) div 1000),
  if RTT == 0 -> {ERTT,ERTT div 2,ERTT};
     true ->
      Delta = RTT - ERTT,
      RTTVar0 = RTTVar + (math:abs(Delta) - RTTVar) div 4,
      RTT0 = RTT - RTT div 8 + ERTT div 8,
      {RTT0,RTTVar0,ERTT}
  end.

acked(Now,{RTT,RTTVar,RTO,RTTHist,RTOTimeout,
           RetransmitTimeout,CurWindow},
     #aiutp_packet_wrap{transmission = Transmission,time_sent = TimeSent,
                        need_resend = NeedResend,payload = Payload})->
  {RTT1,RTTVar1,RTO0,RTTHist1} =
    if Transmission == 1 ->
        {RTT0,RTTVar0,ERTT} = caculate_rtt(RTT,RTTVAr,TimeSent),
        RTTHist0 =
          if RTT == 0 -> aiutp_delay:add_sample(ERTT,Now,RTTHist);
             true -> RTTHist
          end,
        {RTT0,RTTVar0,?MAX((RTT0 + RTTVar0 * 4),1000),RTTHist1}
  end,
  CurWindow0 =
    if NeedResend -> CurWindow - Payload;
       true -> CurWindow
    end,
  {RTT1,RTTVar1,RTO0,RTTHist1,RTO0 + Now,RTO,CurWindow0}.


acked_in_obuf(_,_,-1,_,Acc,OutBuf)-> {Acc,OutBuf};
acked_in_obuf(Now,MaxSeq,Iter,Prev,Acc,OutBuf)->
  WrapPacket = aiutp_buffer:data(Iter,OutBuf),
  Next = aiutp_buffer:next(Iter,OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  if Packet#aiutp_packet.seq_nr < MaxSeq ->
      Acc0 = acked(Now,Acc,WrapPacket),
      OutBuf0 = aiutp_buffer:delete(Iter,Prev,OutBuf),
      acked_in_obuf(Now,MaxSeq,Next,Iter,Acc0,OutBuf0);
     true -> {Acc,OutBuf}
  end.

caculate_cur_window_packets(SeqNR,OutBuf)->
  Iter = aiutp_buffer:head(OutBuf),
  WrapPacket = aiutp_buffer:data(Iter,OutBuf),
  Packet = WrapPacket#aiutp_packet_wrap.packet,
  aiutp_util:bit16(SeqNR - Packet#aiutp_packet.seq_nr).
%% 处理Acked
process_packet_3(Acc,
                 #aiutp_packet{type = PktType,
                               ack_nr = PktAckNR,wnd = PktMaxWindowUser},
                 #aiutp_pcb{state = State,
                            time = {Now,_},
                            fast_resend_seq_nr = FastResendSeqNR,
                            cur_window_packets = CurWindowPackets,
                            cur_window = CurWindow,
                            zerowindow_time = ZeroWindowTime,
                            fin_sent = FinSent,close_requested = CloseRequested,
                            fin_sent_acked = FinSentAcked,
                            rtt = RTT, rto = RTO,rtt_var = RTTVar,rtt_hist = RTTHist,
                            retransmit_timeout = RetransmitTimeout,
                            rto_timeout = RTOTimeout,
                            seq_nr = SeqNR,outbuf = OutBuf} = PCB)->
  Acks = acks(PktAckNR,PCB),
  SeqBase = aiutp_util:bit16(SeqNR - CurWindowPackets),
  MaxSeq = aiutp_util:bit16(SeqBase + Acks),
  ZeroWindowTime0 =
    if OktMaxWindowUser == 0 -> Now + 15000;
       true -> ZeroWindowTime
    end,
  State0 =
    if (PktType == ?ST_DATA) and
       (State == ?CS_SYN_RECV) -> ?CS_CONNECTED;
       true -> State
    end,
  {State1,FinSentAcked0} =
    if (PktType == ?ST_STATE) and
       (State0 == ?CS_SYN_SENT) -> ?CS_CONNECTED;
       FinSent and (CurWindowPackets == Acks) ->
        if CloseRequested -> {?CS_DESTROY,true};
           true -> {State0,true}
        end;
       true -> {State0,FinSentAcked}
    end,

  FastResendSeqNR0 =
    if ?WRAPPING_DIFF_16(FastResendSeqNR,((PktAckrNR + 1) band 16#FFFF)) < 0 -> ((PktAckrNR + 1) band 16#FFFF);
       true -> FastResendSeqNR
    end,
  AckedAcc = {RTT,RTTVar,RTO,RTTHist,RTOTimeout,RetransmitTimeout,CurWindow},
  Iter = aiutp_buffer:head(OutBuf),
  {{RTT0,RTTVar0,RTO0,RTTHist0,RTOTimeout0,RetransmitTimeout0,CurWindow0},
   OutBuf0} = acked_in_obuf(Now,MaxSeq,0,Iter,-1,AckedAcc,OutBuf),
  CurWindowPackets0 = caculate_cur_window_packets(SeqNR,OutBuf0),
  PCB0 = PCB#aiutp_pcb { state = Stat1,outbuf = OutBuf0,fin_sent_acked = FinSentAcked0,
                         fast_resend_seq_nr = FastResendSeqNR0,
                         rtt = RTT0, rtt_var = RTTVar0,rtt_hist = RTTHist0,
                         rto = RTO0,rto_timeout = RTOTimeout0,
                         retransmit_timeout = RetransmitTimeout0,
                         retransmit_count = 0, cur_window = CurWindow0,
                         cur_window_packets = CurWindowPacket0},

  {Acc0,PCB1} =
    if CurWindowPacket0 == 1 ->
        aiutp_net:send_packet(Acc,aiutp_buffer:head(OutBuf0),PCB0);
       true -> {Acc,PCB0}
    end,
