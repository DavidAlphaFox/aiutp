-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/3]).
  
enqueue_transmit(Seq,Actions)->
  Transmit = proplists:get_value(transmist, Actions, []),
  Transmit0 = [Seq|Transmit],
  [{transmit,Transmit0}| proplists:delete(transmist, Actions)].



update_fast_resend_seq_nr(#utp_net{fast_resend_seq_nr = FastResendSeqNR} = Net,AckNo)->
  NextAckNo = ai_utp_util:bit16(AckNo + 1),
  IsLess = ai_utp_util:wrapping_compare_less(
             FastResendSeqNR,NextAckNo,?ACK_NO_MASK),
  if
    IsLess == true -> Net#utp_net{fast_resend_seq_nr = NextAckNo};
    true -> Net
  end.

update_net(#utp_net{state = 'CS_SYN_SENT'} = Net,
           #utp_packet{seq_no = SeqNo})->
  Net#utp_net{ack_nr = ai_utp_util:bit16(SeqNo-1)};
update_net(Net,_) -> Net.


schedule_ack(Type,PeerUnAcks,Actions)->
  if PeerUnAcks >= ?REORDER_BUFFER_MAX_SIZE ->
      Max = ai_utp_util:bit16((?SEQ_NO_MASK + 1) - ?REORDER_BUFFER_MAX_SIZE),
      if PeerUnAcks >= Max andalso Type == st_state ->
          [{ack,true}|Actions];
         true -> Actions
      end;
     true -> Actions
  end.

ack_count(AckNo,SeqNR,CurWindowPackets)->
  Acks = ai_utp_util:bit16(CurWindowPackets - (SeqNR - 1 - AckNo)),
  if Acks > CurWindowPackets -> 0;
     true -> Acks
  end.

ack_packet(OutBuf,Seq,Acc)->
  case array:get(Seq,OutBuf) of
    undefined -> Acc;
    WrapPacket ->
      if WrapPacket#utp_packet_wrap.transmissions > 0 ->
          [{Seq,WrapPacket}];
         true -> Acc
      end
  end.

%% 常规的ack
ack_packets(_,_,-1,Acc)->Acc;
ack_packets(OutBuf,WindowStart,Acks,Acc) ->
  Seq = ai_utp_util:bit16(WindowStart + Acks),
  Acc0 = ack_packet(OutBuf, Seq, Acc),
  ack_packets(OutBuf,WindowStart,Acks,Acc0).

%% selective ack
sack_bit_set(Bits,Len)->
  Pos = Len bsr 3,
  Mask = 1 bsr (Len band 7),
  if
    Pos > 0 ->
      Ignore = (Pos - 1) * 8,
      <<_:Ignore,Bit/big-integer,_/binary>> = Bits,
      Bit;
    true ->
      <<Bit/big-integer,_/binary>> = Bits,
      Bit
  end,
  Bit band Mask.

sack_need_resend(Index,FastResendSeqNR,Count,Acc)->
  Out = ai_utp_util:bit16(Index - FastResendSeqNR),
  if Out =< ?OUTGOING_BUFFER_MAX_SIZE andalso
     Count >= ?DUPLICATE_ACKS_BEFORE_RESEND ->
      [Index|Acc];
     true  -> Acc
  end.
sack_packets(_,_,_,_,_,_,-1, SAckPackets,Acc)-> {SAckPackets,Acc};
sack_packets(OutBuf,SeqNR,CurWindowPackets,FastResendSeqNR,
             Base,Bits,Len,SAckPackets,{Count,Miss} = Acc) ->
  Index = ai_utp_util:bit16(Base + Len),
  Count = ai_utp_util:bit16(SeqNR - 1 - Index),
  if Count >= CurWindowPackets - 1 -> {SAckPackets,Acc};
     true ->
      BitSet = sack_bit_set(Bits, Len),
      if BitSet == true ->
          SAckPackets0 = ack_packet(OutBuf, Index,SAckPackets),
          sack_packets(OutBuf,SeqNR,CurWindowPackets,FastResendSeqNR,
                       Base,Bits,Len - 1,SAckPackets0,{Count + 1,Miss});
         true ->
          Miss0 = sack_need_resend(Index, FastResendSeqNR, Count, Miss),
          sack_packets(OutBuf,SeqNR,CurWindowPackets,FastResendSeqNR,
                       Base,Bits,Len - 1,SAckPackets,{Count,Miss0})
      end
  end.

sack_packets(_,_,_,_,_,[])-> {[],{0,[]}};
sack_packets(OutBuf,SeqNR,CurWindowPackets,
             FastResendSeqNR,Base,[H|T]) ->
  case H of
    {sack,Bits}->
      Len = erlang:byte_size(Bits) * 8 - 1,
      {SAckPackets,{Count,Miss} } =
        sack_packets(OutBuf,SeqNR,CurWindowPackets,FastResendSeqNR,
                     Base,Bits,Len,[],{0,[]}),
      MissStart = ai_utp_util:bit16(Base - 1),
      Miss1 = sack_need_resend(MissStart, FastResendSeqNR, Count,Miss),
      {SAckPackets,{Count,Miss1}};
    _->
      sack_packets(OutBuf, SeqNR,CurWindowPackets,
                   FastResendSeqNR,Base,T)
  end.

ack_bytes(AckPackets,SAckPackets,Now)->
  Fun = fun({_,WrapPacket},{RTT,Bytes})->
            if WrapPacket#utp_packet_wrap.transmissions > 0 ->
                Bytes0 = Bytes + WrapPacket#utp_packet_wrap.payload,
                SendTime = WrapPacket#utp_packet_wrap.send_time,
                RTT0 =
                  if SendTime < Now -> min(RTT, Now - SendTime );
                     true -> min(RTT,50000)
                  end,
                {RTT0,Bytes0};
               true -> {RTT,Bytes}
            end
        end,
  
  Acc = lists:foldl(Fun,{?RTT_MAX,0},AckPackets),
  lists:foldl(Fun, Acc,SAckPackets).


pure_ack_packet(#utp_net{outbuf = OutBuf,rtt = RTT, rtt_ledbat = RTTLedbat,
                     cur_window = CurWindow,
                     cur_window_packets = CurWindowPackets} = Net,Seq,Now)->
  case array:get(Seq,OutBuf) of
    undefined -> {continue,
                  Net#utp_net{cur_window_packets = CurWindowPackets -1}};
    WrapPacket ->
      if
        WrapPacket#utp_packet_wrap.transmissions == 0 ->
          {stop,Net};
        true ->
          Net0 =
            if WrapPacket#utp_packet_wrap.transmissions == 1 ->
                TimeSent = WrapPacket#utp_packet_wrap.send_time,
                {ok, _NewRTO, NewRTT, NewHistory} =
                  ai_utp_rtt:ack(RTTLedbat,RTT,TimeSent,Now),
                Net#utp_net{rtt = NewRTT,rtt_ledbat = NewHistory};
               true -> Net
            end,
          Net1 =
            if WrapPacket#utp_packet_wrap.need_resend == true -> Net0;
               true ->
                Net0#utp_net{cur_window = CurWindow - WrapPacket#utp_packet.payload}
            end,
          {continue,Net1#utp_net{cur_window_packets = CurWindowPackets -1,
                       retransmit_count = 0,
                       outbuf = array:set(Seq,undefined,OutBuf)}}
      end
  end.

pure_prev_sack(#utp_net{cur_window_packets = CurWindowPackets,
                        seq_nr = SeqNR,outbuf = OutBuf} = Net)
  when CurWindowPackets > 0->
  Seq = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  case array:get(Seq,OutBuf) of
    undefined ->
      pure_prev_sack(Net#utp_net{cur_window_packets = CurWindowPackets - 1});
    _ -> Net
  end;
pure_prev_sack(Net) -> Net.


pure_ack(Net,_,-1)-> Net;
pure_ack(#utp_net{cur_window_packets = CurWindowPackets,
                    seq_nr = SeqNR} = Net,Now,Acks)->
  Seq = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  Net1 =
    case pure_ack_packet(Net, Seq, Now) of
      {stop,Net0} -> Net0;
      {continue,Net0} -> pure_ack(Net0,Now,Acks - 1)
    end,
  pure_prev_sack(Net1).

pure_sack(#utp_net{outbuf = OutBuf} = Net,SAckPackets)->
  OutBuf0 = lists:foldl(
              fun({Index,_},Acc)-> array:set(Index,undefined,Acc) end,
              OutBuf,SAckPackets),
  Net#utp_net{outbuf = OutBuf0}.

%% 对Peer的计算
update_peer_ledbat(Net,0)-> Net;
update_peer_ledbat(#utp_net{ peer_ledbat = none } = Net, Sample) ->
  Net#utp_net{ peer_ledbat = ai_utp_ledbat:new(Sample) };
update_peer_ledbat(#utp_net{ peer_ledbat = Ledbat } = Net, Sample) ->
  Net#utp_net{ peer_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.


%% if their new delay base is less than their previous one
%% we should shift our delay base in the other direction in order
%% to take the clock skew into account
update_clock_skew(#utp_net{ peer_ledbat = none }, NW) -> NW;
update_clock_skew(#utp_net{peer_ledbat = OldPeers },
           #utp_net{peer_ledbat = Peers,
                    our_ledbat   = Ours
                   } = NW) ->
  OldDelayBase = ai_utp_ledbat:base_delay(OldPeers),
  DelayBase = ai_utp_ledbat:base_delay(Peers),
  Diff = OldDelayBase - DelayBase,
  IsLess = ai_utp_ledbat:compare_less(DelayBase,OldDelayBase),
  % never adjust more than 10 milliseconds
  if IsLess == true andalso Diff < 10000 ->
      NW#utp_net{ our_ledbat = ai_utp_ledbat:shift(Ours, Diff) };
     true -> NW
  end.

update_reply_micro(Net,Now,TS)->
  ReplyMicro =
    if TS > 0 -> ai_utp_util:bit32(Now - TS);
       true -> 0
    end,
  Net0 = update_peer_ledbat(Net#utp_net{reply_micro = ReplyMicro},ReplyMicro),
  update_clock_skew(Net, Net0).


%% 对自己的计算
update_our_ledbat(Net,0)-> Net;
update_our_ledbat(#utp_net{ our_ledbat = none } = Net, Sample) ->
  Net#utp_net{ our_ledbat = ai_utp_ledbat:new(Sample) };
update_our_ledbat(#utp_net{ our_ledbat = Ledbat } = Net, Sample) ->
  Net#utp_net{ our_ledbat = ai_utp_ledbat:add_sample(Ledbat, Sample) }.


%% if the delay estimate exceeds the RTT, adjust the base_delay to
%% compensate
update_estimate_exceed(#utp_net{our_ledbat = Ours} = NW,MinRTT) ->
  OurDelay = ai_utp_ledbat:get_value(Ours),
  Diff = OurDelay - MinRTT,
  if
    Diff > 0 -> NW#utp_net{our_ledbat = ai_utp_ledbat:shift(Ours, Diff) };
    true-> NW
  end.

congestion_control(#utp_net{our_ledbat = OurLedbat,max_window = MaxWindow,
                            opt_sndbuf = OptSndBuf,
                            last_maxed_out_window = LastMaxedOutWindow }=Net,
                   AckedBytes, ActualDelay, NowMS,MinRTT)->
  OurDelay = min(MinRTT,ai_utp_ledbat:get_value(OurLedbat)),
  TargetDelay = ?CONGESTION_CONTROL_TARGET,

  TargetOffset = TargetDelay - OurDelay,
  %% this is the same as:
  %%
  %%    (min(off_target, target) / target) * (bytes_acked / max_window) * MAX_CWND_INCREASE_BYTES_PER_RTT
  %%
  %% so, it's scaling the max increase by the fraction of the window this ack represents, and the fraction
  %% of the target delay the current delay represents.
  %% The min() around off_target protects against crazy values of our_delay, which may happen when th
  %% timestamps wraps, or by just having a malicious peer sending garbage. This caps the increase
  %% of the window size to MAX_CWND_INCREASE_BYTES_PER_RTT per rtt.
  %% as for large negative numbers, this direction is already capped at the min packet size further down
  %% the min around the bytes_acked protects against the case where the window size was recently
  %% shrunk and the number of acked bytes exceeds that. This is considered no more than one full
  %% window, in order to keep the gain within sane boundries.
  WindowFactor = min(AckedBytes, MaxWindow) / max(MaxWindow, AckedBytes),

  %% The delay factor is how much we are off the target:
  DelayFactor = TargetOffset / TargetDelay,
  ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,
  ScaledGain0 =
    if (NowMS - LastMaxedOutWindow) > 1000 andalso ScaledGain > 0 -> 0;
       true -> ScaledGain
    end,
  LedbatCwnd = ai_utp_util:clamp(MaxWindow + ScaledGain0,?MIN_WINDOW_SIZE,OptSndBuf),
  Net#utp_net{max_window = LedbatCwnd}.

do_congestion_control(Net,TSDiff,NowMS,MinRTT,AckBytes)->
  TSDiff0 = ai_utp_util:bit32(TSDiff),
  ActualDelay =
    if TSDiff == ?TS_DIFF_MAX -> 0;
       true -> TSDiff0
    end,
  Net0 = update_our_ledbat(Net, ActualDelay),
  Net1 = update_estimate_exceed(Net0, MinRTT),
  if ActualDelay > 0 andalso AckBytes > 0 ->
      congestion_control(Net1,AckBytes,ActualDelay,NowMS,MinRTT);
     true -> Net1
  end.

nagle_send(#utp_net{outbuf = OutBuf,seq_nr = SeqNR,
                    cur_window_packets = CurWindowPackets},Actions)->
  if CurWindowPackets == 1 -> Actions;
     true ->
      case array:get(SeqNR - 1,OutBuf) of
        undefined -> Actions;
        WrapPacket ->
          if WrapPacket#utp_packet_wrap.transmissions == 0 ->
              [{transmist,SeqNR - 1}|Actions];
             true -> Actions
          end
      end
  end.
fast_timeout(#utp_net{fast_timeout = false} = Net,Actions) ->
  {Net,Actions};
fast_timeout(#utp_net{seq_nr = SeqNR, cur_window_packets = CurWindowPackets,
                      outbuf = OutBuf,fast_resend_seq_nr = FastResendSeqNR} = Net,
             Actions) ->
  Oldest = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  if Oldest /= FastResendSeqNR ->
      {Net#utp_net{fast_timeout = false},Actions};
     true ->
      case array:get(Oldest,OutBuf) of
        undefined -> {Net,Actions};
        WrapPacket ->
          if WrapPacket#utp_packet_wrap.transmissions > 0 ->
              {Net,[{transmist,Oldest}|Actions]};
             true -> {Net,Actions}
          end
      end
  end.

resend_on_ack(Net,DuplicateAck,[],Actions)->
  {Net#utp_net{duplicate_ack = DuplicateAck},Actions};
resend_on_ack(Net,DuplicateAck,Miss,Actions) ->
  Miss1 =
    if erlang:length(Miss) > 4 ->
        {Miss0,_} = lists:split(4, Miss),
        Miss0;
       true -> Miss
    end,
  lists:foldl(fun(Seq,{Net0,Actions0})->
                  Actions1 = enqueue_transmit(Seq, Actions0),
                  FastResendSeqNR = ai_utp_util:bit16(Seq + 1),
                  {Net0#utp_net{fast_resend_seq_nr = FastResendSeqNR},Actions1}
              end, {Net,Actions}, Miss1).
is_full(#utp_net{max_window = MaxWindow,opt_sndbuf = OptSndBuf,
                 max_peer_window = MaxPeerWindow,cur_window = CurWindow,
                 cur_window_packets = CurWindowPackets} = Net,Actions)->
  MaxSend = min(MaxWindow, OptSndBuf),
  MaxSend0 = min(MaxSend,MaxPeerWindow),
  {Net0,Actions0} =
    if CurWindowPackets >= ?OUTGOING_BUFFER_MAX_SIZE - 1 ->
        {Net#utp_net{last_maxed_out_window = ai_utp_util:millisecond()},
         [{is_full,true}|Actions]};
       true -> {Net,Actions}
    end,
  if CurWindow + ?PACKET_SIZE > MaxSend0 ->
      {Net0#utp_net{last_maxed_out_window = ai_utp_util:millisecond()},
       [{is_full,true}| proplists:delete(is_full,Actions0)]};
     true -> {Net0,Actions0}
  end.

update_inbuf(#utp_net{ack_nr = AckNR, reorder_count = ReorderCount,
                      got_fin = GotFin,eof_seq_no = EofSeqNo} = Net,
             0,Actions)->
  Net0 = Net#utp_net{ack_nr = AckNR + 1},
  

process_incoming(#utp_net{state = State } = NetBase,
                 #utp_packet{type = Type,seq_no = SeqNo,
                             ack_no = AckNo,win_sz = WndSize,
                             extension = Ext} = Packet,
                 {TS,TSDiff,Now} = Timing) ->
  Net = update_net(NetBase,Packet),
  Net0 = update_fast_resend_seq_nr(Net, AckNo),
  #utp_net{ack_nr = AckNR,seq_nr = SeqNR,
           cur_window_packets = CurWindowPackets,
           outbuf = OutBuf,fin_sent = FinSent,got_fin = GotFin,
           fast_resend_seq_nr = FastResendSeqNR} = Net0,

  NowMS = ai_utp_util:millisecond(),
  %% 计算有多少个包Peer没有收到ack
  PeerUnAcks = ai_utp_util:bit16(SeqNo - AckNR - 1),
  %% 判断是否需要发送ack包
  Actions = schedule_ack(Type, PeerUnAcks, []),
  %% 计算受到Acks的数量
  Acks = ack_count(AckNo, SeqNR, CurWindowPackets),

  Oldest = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  AckPackets = ack_packets(OutBuf,Oldest,Acks -1,[]),

  SAckOldest = ai_utp_util:bit16(AckNo + 2),
  {SAckPackets,{DuplicateAck,Miss}} = 
    sack_packets(OutBuf,SeqNR,CurWindowPackets,FastResendSeqNR,SAckOldest,Ext),
  %% 计算所有已经ack的字节和最小的round trip
  {MinRTT,AckBytes} = ack_bytes(AckPackets,SAckPackets,Now),

  Net1 = update_reply_micro(Net0, Now, TS),
  Net2 = do_congestion_control(Net1, TSDiff, NowMS, MinRTT, AckBytes),
  Net3 = Net2#utp_net{max_peer_window = WndSize},
  Actions0 =
    if WndSize == 0->
        [{max_peer_window,NowMS}|Actions];
       true -> Actions
    end,
  {Net4,Actions1} =
    if State == 'SYN_RECV' andalso Type == st_data ->
        {Net3#utp_net{state = 'CONNECTED'},
         [{next_state,'CONNECTED'}|Actions0]};
       true -> {Net3,Actions0}
    end,
  {Net5,Actions2} =
    if State == 'SYN_SENT' andalso Type == st_state ->
        {Net4#utp_net{state = 'CONNECTED'},
         [{next_state,'CONNECTED'}|Actions1]};
       FinSent == true andalso Acks == CurWindowPackets ->
        {Net4#utp_net{state = 'DESTROY'},
         [{next_state,'DESTROY'}|proplists:delete(next_state, Actions1)]};
       true -> {Net4,Actions1}
    end,
  Net5 = pure_ack(Net4,Now,Acks),
  Net6 = pure_sack(Net5, SAckPackets),
  Actions3 = nagle_send(Net6,Actions2),
  {Net7,Actions4} = fast_timeout(Net6, Actions3),
  {Net8,Actions5} = resend_on_ack(Net7,DuplicateAck,Miss,Actions4),
  {Net9,Actions6} = is_full(Net8,Actions5),
  if Type == st_state -> {Net9,Actions6};
     true ->
      Net10 =
        if Type == st_fin andalso
           GotFin == false ->
            Net9#utp_net{got_fin = true,eof_seq_no = SeqNo};
           true -> Net9
        end,
      update_inbuf(Net10,PeerUnAcks,Actions6)
  end.
