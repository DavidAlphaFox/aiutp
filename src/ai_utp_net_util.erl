-module(ai_utp_net_util).
-include("ai_utp.hrl").

-export([send_syn/1,send_ack/2,send_fin/1,
         send_syn_state/1,send/3]).
-export([window_size/1,max_send_bytes/1,sndbuf_remain/1]).
-export([change_state/3,change_state/2]).

send(#utp_net{socket = Socket,remote = Remote},Packet,TSDiff)->
  ai_utp_util:send(Socket, Remote, Packet, TSDiff).

send_ack(#utp_net{ack_nr = AckNR,seq_nr = SeqNR,
                  reply_micro = ReplyMicro,inbuf_size = RSize,
                  peer_conn_id = PeerConnID,
                  ext_bits = ExtBits} = Net,Quick)->
  AckNo = ai_utp_util:bit16(AckNR -1),
  SeqNo = ai_utp_util:bit16(SeqNR -1),
  Packet =
    if (Quick == true) orelse (RSize == 0 ) ->
        ai_utp_protocol:make_ack_packet(SeqNo, AckNo,[{ext_bits,ExtBits}]);
       true ->
        Bits = ai_utp_rx:sack(ai_utp_util:bit16(AckNR + 1),Net),
        case Bits of
          undefined ->
            ai_utp_protocol:make_ack_packet(SeqNo, AckNo,
                                            [{ext_bits,ExtBits}]);
          _ ->
            ai_utp_protocol:make_ack_packet(SeqNo, AckNo,
                                            [{ext_bits,ExtBits},{sack,Bits}])
        end
    end,
  Packet0 = Packet#utp_packet{win_sz = window_size(Net),
                              conn_id = PeerConnID},
  case send(Net,Packet0,ReplyMicro) of
    {ok,SendTimeNow}-> Net#utp_net{last_send = SendTimeNow};
    true -> Net
  end.

send_syn(#utp_net{max_window = MaxWindow,seq_nr = SeqNR,
                  conn_id = ConnID,syn_sent_count = SynSentCount,
                  ext_bits = ExtBits} = Net) ->
  SeqNo = ai_utp_util:bit16(SeqNR - 1),
  Packet = ai_utp_protocol:make_syn_packet(SeqNo),
  case send(Net,Packet#utp_packet{conn_id = ConnID,
                                  win_sz = MaxWindow,
                                  extension = [{ext_bits,ExtBits}]},0) of
    {ok,SendTimeNow} ->
      Net#utp_net{
        last_send = SendTimeNow,
        last_recv = SendTimeNow,
        last_decay_win = SendTimeNow /1000,
        syn_sent_count = SynSentCount + 1};
    _ ->
      Now = ai_utp_util:microsecond(),
      Net#utp_net{
        last_send = Now,
        last_recv = Now,
        last_decay_win = Now /1000,
        syn_sent_count = SynSentCount + 1}
  end.

send_syn_state(#utp_net{seq_nr = SeqNR,
                        ack_nr = AckNR,
                        max_window = MaxWindow,
                        peer_conn_id = PeerConnID,
                        syn_sent_count = SynSentCount,
                        reply_micro = ReplyMicro,
                        ext_bits = ExtBits} = Net)->
  SeqNo = ai_utp_util:bit16(SeqNR - 1),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  Res = ai_utp_protocol:make_ack_packet(SeqNo, AckNo),
  case send(Net,Res#utp_packet{win_sz = MaxWindow,
                               extension = [{ext_bits,ExtBits}],
                               conn_id = PeerConnID},ReplyMicro) of
    {ok,SendTimeNow}->
      Net#utp_net{syn_sent_count = SynSentCount + 1,
                  last_send = SendTimeNow,
                  last_decay_win = SendTimeNow / 1000};
    _ ->
      Now = ai_utp_util:microsecond(),
      Net#utp_net{syn_sent_count = SynSentCount + 1,
                  last_send = Now,
                  last_decay_win = Now / 1000}
  end.
send_fin(#utp_net{last_seq_nr = SeqNR,
                  peer_conn_id = PeerConnID,
                  reply_micro = ReplyMicro,
                  ext_bits = ExtBits,
                  ack_nr = AckNR} = Net)->
  FinSeqNo = ai_utp_util:bit16(SeqNR - 1),
  AckNo = ai_utp_util:bit16(AckNR - 1),
  Fin = ai_utp_protocol:make_fin_packet(FinSeqNo, AckNo),
  Fin0 = Fin#utp_packet{conn_id = PeerConnID,win_sz = 0,
                        extension = [{ext_bits,ExtBits}]},
  case send(Net,Fin0,ReplyMicro) of
    {ok,SendTimeNow}->
      Net#utp_net{fin_sent = true,
                  fin_seq_no = FinSeqNo,
                  last_send = SendTimeNow};
    _ ->
      Net#utp_net{fin_sent = true,
                  fin_seq_no = FinSeqNo,
                  last_send = ai_utp_util:microsecond()}
  end.


window_size(#utp_net{max_window = MaxWindow,
                     recvbuf_size = RecvBufSize}) ->
  if MaxWindow > RecvBufSize -> MaxWindow - RecvBufSize;
     true -> 0
  end.

sndbuf_remain(#utp_net{opt_sndbuf = OptSndBuf,
                       cur_window = CurWindow,
                       sndbuf_size = SndBufSize})->
  BufSize = CurWindow + SndBufSize,
  if OptSndBuf > BufSize ->
      OptSndBuf - BufSize;
     true -> 0
  end.
max_send_bytes(#utp_net{max_peer_window = MaxPeerWindow,
                        cur_window = CurWindow})->
  SendBytes = MaxPeerWindow - CurWindow,
  if SendBytes >= ?PACKET_SIZE -> SendBytes;
     true -> 0
  end.



change_state(Net,State,Error)->
  Net#utp_net{
    state = State,
    error = Error,
    last_state_changed = ai_utp_util:microsecond()
   }.

change_state(Net,State)->
  Net#utp_net{
    state = State,
    last_state_changed = ai_utp_util:microsecond()
   }.
