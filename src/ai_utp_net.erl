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



%% 最后阶段计算并清理所有被Ack过的包
ack(Net,#utp_packet{ack_no = AckNo,
                    win_sz = WndSize,extension = Ext},
    {TS,TSDiff,Now})->
  SAcks = proplists:get_value(sack, Ext,undefined),
  {AckPackets,Net1} = ai_utp_buffer:ack_packet(AckNo, SAcks, Net),
  {MinRTT,AckBytes} = ack_bytes(AckPackets,Now),
  ai_utp_cc:cc(Net1, TS, TSDiff, Now, MinRTT, AckBytes,WndSize).

process_incoming(#utp_net{state = State }= Net,
                 #utp_packet{type = Type} = Packet,
                 Timing) ->
  process_incoming(Type,State,Net,Packet,Timing).

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
