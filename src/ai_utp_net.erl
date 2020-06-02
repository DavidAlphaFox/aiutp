-module(ai_utp_net).
-include("ai_utp.hrl").

-export([process_incoming/3]).

ack_bytes(AckPackets,Now)->
  lists:foldl(
    fun({_,WrapPacket},{RTT,Bytes})->
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
    end, {?RTT_MAX,0}, AckPackets).

window_size(#utp_net{cur_window = CurWindow,
                    max_window = MaxWindow})->
  if MaxWindow > CurWindow ->
      MaxWindow - CurWindow;
     true -> 0
  end.

%% 最后阶段计算并清理所有被Ack过的包
ack(Net,#utp_packet{ack_no = AckNo,
                    win_sz = WndSize,extension = Ext},
    {TS,TSDiff,Now})->
  SAcks = proplists:get_value(sack, Ext,undefined),
  {AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
  {MinRTT,AckBytes} = ack_bytes(AckPackets,Now),
  ai_utp_cc:cc(Net1, TS, TSDiff, Now, MinRTT, AckBytes,WndSize).

send_ack(#utp_net{ack_nr = AckNR,seq_nr = SeqNR} = Net)->
  Bits = ai_utp_buffer:sack(AckNR + 1,Net),
  AckNo = ai_utp_util:bit16(AckNR -1),
  SeqNo = ai_utp_util:bit16(SeqNR -1),
  Packet =
    case Bits of
      undefined -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo);
      _ -> ai_utp_protocol:make_ack_packet(SeqNo, AckNo, [{sack,Bits}])
    end,
  Packet#utp_packet{win_sz = window_size(Net)}.

send_ack(#utp_net{ack_nr = AckNR,reorder = Reorder},
         #utp_net{ack_nr = AckNR0,reorder = Reorder0} = Net1)->
  if (AckNR /= AckNR0) orelse
     (Reorder /= Reorder0) -> send_ack(Net1);
     true -> undefined
  end.

send(#utp_net{outbuf = OutBuf,max_peer_window = MaxPeerWindow,
              seq_nr = SeqNR,ack_nr = AckNR,
              cur_window = CurWindow,
              cur_window_packets = CurWindowPackets} = Net,Timer)->
  SeqNo = ai_utp_util:bit16(SeqNR - 1),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> {Net,[]};
     true ->
      case send(Timer,SeqNo,AckNo,MaxPeerWindow,queue:to_list(OutBuf)) of
        error -> {Net#utp_net{state = {'ERROR',epipe}},[]};
        full -> {Net,[]};
        {Bytes,InFlight,Packets,OutBuf0} ->
          {Net#utp_net{
             outbuf = queue:from_list(lists:reverse(OutBuf0)),
             cur_window = CurWindow + Bytes,
             cur_window_packets = CurWindowPackets + InFlight,
             seq_nr = SeqNR + InFlight
            },Packets}
      end
  end.

send(_,_,_,0,_)-> full;
send(Timer,SeqNo,AckNo,MaxPeerWindow,OutBuf)->
  Now = ai_utp_util:microsecond(),
  lists:foldl(
    fun(WrapPacket,Acc) ->
        if Acc == error -> Acc;
           true->
            {SendBytes,InFlight,Packets,Buf} = Acc,
            #utp_packet_wrap{
               packet = Packet,
               transmissions = Trans,
               payload = Payload
              } = WrapPacket,
            #utp_packet{seq_no = PacketSeqNo} = Packet,
            %% 大于5次，并且是timer触发，说明有可能发送不到了
            if (Trans >= 5) andalso (Timer == true)-> error;
               true ->
                %% 对方还有接收的窗口
                if MaxPeerWindow - SendBytes >= Payload ->
                    Packet0 = Packet#utp_packet{ack_no = AckNo},
                    Resend =
                      ai_utp_util:wrapping_compare_less(PacketSeqNo,
                                                        SeqNo, ?SEQ_NO_MASK),
                    if (Resend == true) orelse
                       (SeqNo == PacketSeqNo) ->
                        %% 重新传输的包不记入窗口
                        {SendBytes,InFlight,[Packet0| Packets],
                        [WrapPacket#utp_packet_wrap{
                           transmissions = Trans + 1,
                           send_time = Now
                          }|Buf]};
                       true ->
                        {SendBytes + Payload,InFlight + 1,[Packet0| Packets],
                        [WrapPacket#utp_packet_wrap{
                           transmissions = Trans + 1,
                           send_time = Now
                          }|Buf]}
                    end;
                   true ->
                    {SendBytes,InFlight,Packets,
                     [WrapPacket|Buf]}
                end
            end
        end
    end,{0,0,[],[]},OutBuf).


process_incoming(#utp_net{state = State }= Net,
                 #utp_packet{type = Type} = Packet,
                 Timing) ->
  Net0 = process_incoming(Type,State,Net,Packet,Timing),
  AckPacket = send_ack(Net,Net0),
  {Net1,Packets} = send(Net0,false),
  {Net1,[AckPacket|Packets],Net1#utp_net.reply_micro}.


process_incoming(st_data,'SYN_RECEIVED',
                 #utp_net{seq_nr = SeqNR,ack_nr = PeerSeqNo} = Net,
                 #utp_packet{ack_no = AckNo,seq_no = PeerSeqNo,
                            win_sz = WndSize} =  Packet,Timing) ->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 ->
      process_incoming(st_data, 'ESTABLISHED',
                       Net#utp_net{state = 'ESTABLISHED',
                                   max_peer_window = WndSize},
                       Packet,Timing);
     true -> Net
  end;
process_incoming(st_data,'ESTABLISHED',Net,
                 #utp_packet{seq_no = SeqNo,payload = Payload
                            }=Packet,Timing) ->
  case ai_utp_buffer:in(SeqNo,Payload,Net) of
    duplicate -> Net;
    {_,Net1} -> ack(Net1,Packet,Timing)
  end;
process_incoming(st_state,'SYN_SENT',
                 #utp_net{seq_nr = SeqNR} = Net,
                 #utp_packet{ack_no = AckNo,seq_no = SeqNo} = Packet,
                 Timing) ->
  Diff = ai_utp_util:bit16(SeqNR - AckNo),
  if Diff == 1 ->
      Net1 = Net#utp_net{state = 'ESTABLISHED',
                         ack_nr = SeqNo},
      ack(Net1,Packet,Timing);
     true -> Net
  end.
