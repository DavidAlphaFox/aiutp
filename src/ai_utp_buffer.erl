-module(ai_utp_buffer).
-include("ai_utp.hrl").

-export([in/3,ack_packet/3,sack/2]).

%% 需要修改ack_nr
%% ack_nr总是代表下一个需要进行ack的包
%% 此处只应该接收st_data的包
in(SeqNo,Payload,
   #utp_net{state = State,
            ack_nr = SeqNo,
            got_fin = true,
            eof_seq_no = SeqNo} = Net)->
  Net0 = recv(State,Payload,Net),
  {fin,Net0#utp_net{ack_nr = ai_utp_util:bit16(SeqNo+1)}};

in(SeqNo,Payload,
   #utp_net{state = State,
            ack_nr = SeqNo} = Net)->
  Net0 = recv(State,Payload,Net),
  recv_reorder(Net0#utp_net{ack_nr = ai_utp_util:bit16(SeqNo + 1)});
in(SeqNo,Payload,
   #utp_net { inbuf = InBuf,ack_nr = AckNR,
              inbuf_size = RSize} = Net) ->
  Less = ai_utp_util:wrapping_compare_less(SeqNo,AckNR, ?ACK_NO_MASK),
  Diff = ai_utp_util:bit16(SeqNo - AckNR),
  if Less == true -> duplicate;
     Diff > ?REORDER_BUFFER_MAX_SIZE -> {ok,Net};
     true ->
      case array:get(SeqNo, InBuf) of
        undefined ->
          {ok,Net#utp_net{
                inbuf = array:set(SeqNo, Payload,InBuf),
                inbuf_size = RSize + 1
               }};
        _ -> duplicate
      end
  end.

recv(?ESTABLISHED,<<>>,Net) -> Net;
recv(?ESTABLISHED,Payload,
     #utp_net{recvbuf = RecvBuf,
              recvbuf_size = RecvBufSize} = Net) ->
  Size = erlang:byte_size(Payload),
  Net#utp_net{
    recvbuf = queue:in({Size,Payload},RecvBuf),
    recvbuf_size = RecvBufSize + Size};

recv(_,_,Net) -> Net.

recv_reorder(#utp_net{state = State,ack_nr = SeqNo,
                      inbuf = InBuf,inbuf_size = RSize} = Net) ->
  case array:get(SeqNo,InBuf) of
    undefined -> {ok,Net};
    Payload ->
      Net0 = recv(State, Payload, Net),
      recv_reorder(Net0#utp_net{inbuf = array:set(SeqNo,undefined,InBuf),
                                inbuf_size = RSize -1,
                                ack_nr = ai_utp_util:bit16(SeqNo + 1)})
  end.


ack_packet(AckNo,SAcks,#utp_net{cur_window_packets = CurWindowPackets,
                                seq_nr = SeqNR,outbuf = OutBuf} = Net)->
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> {0,[],Net};
     true ->
      %% 最老的序列号
      WindowStart = ai_utp_util:bit16(SeqNR - CurWindowPackets),
      %% AckNo必须小于SeqNR，distance = 0的时候，不应该进行ack
      AckDistance = ack_distance(CurWindowPackets, SeqNR, AckNo),

      {Packets,OutBuf0} =
        if AckDistance == 0 -> {[],queue:to_list(OutBuf)};
           true -> ack_distance_packet(WindowStart,AckDistance,
                                       queue:to_list(OutBuf))
        end,
      {Lost,Acked,Packets0,OutBuf1} = sack_packet(AckNo, SAcks, OutBuf0),
      OutBuf2 = fast_resend(AckNo,WindowStart,Acked,OutBuf1),
      {Lost,Packets ++ Packets0,
       Net#utp_net{outbuf = queue:from_list(OutBuf2),
                   cur_window_packets = CurWindowPackets - AckDistance}}
  end.

ack_distance(CurWindowPackets,SeqNR,AckNo)->
  %% ack的序列号需要小于SeqNo
  Less = ai_utp_util:wrapping_compare_less(AckNo, SeqNR, ?ACK_NO_MASK),
  AckDistance = ai_utp_util:bit16(CurWindowPackets - (SeqNR - 1 - AckNo)),
  if Less == true ->
      if AckDistance > CurWindowPackets -> 0;
         true -> AckDistance
      end;
     true -> 0
  end.

ack_distance_packet(WindowStart,AckDistance,OutBuf)->
  lists:partition(
    fun(#utp_packet_wrap{
           packet = #utp_packet{seq_no = SeqNo}})->
        Distance = ai_utp_util:bit16(SeqNo - WindowStart),
        Distance < AckDistance
    end, OutBuf).

fast_resend(_,_,_,[])-> [];
fast_resend(AckNo,WindowStart,Acked,
            [#utp_packet_wrap{
                wanted = Wanted,
                transmissions = Trans,
                packet = #utp_packet{seq_no = WindowStart}} = H|T] = OutBuf) ->
  Diff = ai_utp_util:bit16(WindowStart - AckNo),
  if Diff == 1 ->
      Packet =
        if ((Wanted >= ?DUPLICATE_ACKS_BEFORE_RESEND) and (Trans =< 1)) orelse
           (Acked > ?DUPLICATE_ACKS_BEFORE_RESEND)->
            H#utp_packet_wrap{wanted = Wanted + 1, need_resend = true};
           true -> H#utp_packet_wrap{wanted = Wanted + 1}
        end,
      [Packet|T];
     true -> OutBuf
  end;
fast_resend(_,_,_,OutBuf) -> OutBuf.


sack_map(<<>>,_,Map) -> Map;
sack_map(<<Bits/big-integer,Rest/binary>>,N,Map) ->
  sack_map(Rest,N+1,maps:put(N,Bits,Map)).

need_resend(Acked,Wrap)->
  if Acked > ?DUPLICATE_ACKS_BEFORE_RESEND ->
      Wrap#utp_packet_wrap{need_resend = true};
     true -> Wrap
  end.

sacked(Map,Index,Max,Wrap,
       {Lost,Acked,Acks0,UnAcks0})->
  #utp_packet_wrap{transmissions = Trans} = Wrap,
  if (Index < Max) andalso ( Trans > 0 ) ->
      Pos = Index bsr 3,
      case maps:get(Pos,Map) of
        0 -> {Lost+1,Acked,Acks0,[need_resend(Acked,Wrap)|UnAcks0]};
        Bit ->
          Mask = 1 bsl (Index band 7),
          Set = Bit band Mask,
          if Set == 0 -> {Lost+1,Acked,Acks0,[need_resend(Acked,Wrap)|UnAcks0]};
             true ->{Lost,Acked+1,[Wrap|Acks0],UnAcks0}
          end
      end;
     true ->{Lost,Acked,Acks0,[Wrap|UnAcks0]}
  end.
sack_packet(_,undefined,OutBuf)-> {0,0,[],OutBuf};
sack_packet(AckNo,Bits,OutBuf)->
  Max = erlang:byte_size(Bits) * 8,
  Map = sack_map(Bits,0,#{}),
  Base = ai_utp_util:bit16(AckNo + 2),
  lists:foldl(fun(#utp_packet_wrap{
                     packet = #utp_packet{seq_no = SeqNo}} = Warp,
                  {Lost,Acked,Acks0,UnAcks0} = Acc)->
                  Less = ai_utp_util:wrapping_compare_less(Base, SeqNo, ?ACK_NO_MASK),
                  if (Less == true) orelse (SeqNo == Base)->
                      Index = ai_utp_util:bit16(SeqNo - Base),
                      sacked(Map,Index,Max,Warp,Acc);
                     true -> {Lost,Acked,Acks0,[Warp|UnAcks0]}
                  end
              end, {0,0,[],[]}, lists:reverse(OutBuf)).

sack(_,#utp_net{inbuf_size = 0})-> undefined;
sack(Base,#utp_net{inbuf = InBuf}) ->
{_,_,Acc} = lists:foldl(
              fun(Index,{Pos,Bit,Acc})->
                  Pos0 = Index bsr 3,
                  Mask = 1 bsl (Index band 7),
                  SeqNo = ai_utp_util:bit16(Base + Index),
                  {Bit0,Acc0} =
                    if Pos0 > Pos -> {0,<<Acc/binary,Bit/big-integer>>};
                       true ->{Bit,Acc}
                    end,
                  case array:get(SeqNo, InBuf) of
                    undefined -> {Pos0,Bit0,Acc0};
                    _ -> {Pos0,Bit0 bor Mask,Acc0}
                  end
              end,{0,0,<<>>},lists:seq(0, 799)),
  if erlang:byte_size(Acc) > 0 -> Acc;
     true -> undefined
  end.

