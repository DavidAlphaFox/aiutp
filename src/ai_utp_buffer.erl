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
   #utp_net { reorder = OD,ack_nr = AckNR,
              reorder_size = RSize} = Net) ->
  Less = ai_utp_util:wrapping_compare_less(SeqNo,AckNR, ?ACK_NO_MASK),
  Diff = ai_utp_util:bit16(SeqNo - AckNR),
  if Less == true -> duplicate;
     Diff > ?REORDER_BUFFER_MAX_SIZE -> {ok,Net};
     true ->
      case array:get(SeqNo, OD) of
        undefined ->
          {ok,Net#utp_net{
                reorder = array:set(SeqNo, Payload, OD),
                reorder_size = RSize + 1
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
                      reorder = OD,reorder_size = RSize} = Net) ->
  case array:get(SeqNo,OD) of
    undefined -> {ok,Net};
    Payload ->
      Net0 = recv(State, Payload, Net),
      recv_reorder(Net0#utp_net{reorder = array:set(SeqNo,undefined,OD),
                                reorder_size = RSize -1,
                                ack_nr = ai_utp_util:bit16(SeqNo + 1)})
  end.


ack_packet(AckNo,SAcks,#utp_net{cur_window_packets = CurWindowPackets,
                                seq_nr = SeqNR,outbuf = OutBuf} = Net)->
  IsEmpty = queue:is_empty(OutBuf),
  if IsEmpty == true -> {[],Net};
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
      OutBuf1 = fast_resend(AckNo,WindowStart,OutBuf0),
      {Packets0,OutBuf2} = sack_packet(AckNo, SAcks, OutBuf1),
      {Packets ++ Packets0,
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

fast_resend(_,_,[])-> [];
fast_resend(AckNo,WindowStart,
            [#utp_packet_wrap{
                wanted = Wanted,
                transmissions = Trans,
                packet = #utp_packet{seq_no = WindowStart}} = H|T] = OutBuf) ->
  Diff = ai_utp_util:bit16(WindowStart - AckNo),
  if Diff == 1 ->
      Packet =
        if (Wanted >= ?DUPLICATE_ACKS_BEFORE_RESEND) and
           (Trans == 1)->
            H#utp_packet_wrap{wanted = Wanted + 1, need_resend = true};
           true -> H#utp_packet_wrap{wanted = Wanted + 1}
        end,
      [Packet|T];
     true -> OutBuf
  end;
fast_resend(_,_,OutBuf) -> OutBuf.


sack_map(<<>>,_,Map) -> Map;
sack_map(<<Bits/big-integer,Rest/binary>>,N,Map) ->
  sack_map(Rest,N+1,maps:put(N,Bits,Map)).

sack_packet(_,undefined,OutBuf)-> {[],OutBuf};
sack_packet(AckNo,Bits,OutBuf)->
  Max = erlang:byte_size(Bits) * 8,
  Map = sack_map(Bits,0,#{}),
  Base = ai_utp_util:bit16(AckNo + 2),
  lists:foldl(fun(#utp_packet_wrap{
                     packet = #utp_packet{seq_no = SeqNo}} = Warp,
                  {Acks0,UnAcks0})->
                  Less = ai_utp_util:wrapping_compare_less(Base, SeqNo, ?ACK_NO_MASK),
                  if (Less == true) orelse (SeqNo == Base)->
                      Index = ai_utp_util:bit16(SeqNo - Base),
                      if
                        Index < Max ->
                          Pos = Index bsr 3,
                          case maps:get(Pos,Map) of
                            0 -> {Acks0,[Warp|UnAcks0]};
                            Bit ->
                              Mask = 1 bsl (Index band 7),
                              Set = Bit band Mask,
                              if Set == 0 -> {Acks0,[Warp|UnAcks0]};
                                 true ->{[Warp|Acks0],UnAcks0}
                              end
                          end;
                        true ->{Acks0,[Warp|UnAcks0]}
                      end;
                     true -> {Acks0,[Warp|UnAcks0]}
                  end
              end, {[],[]}, lists:reverse(OutBuf)).

sack(_,#utp_net{reorder_size = 0})-> undefined;
sack(Base,#utp_net{reorder = Reorder}) ->
{_,_,Acc} = lists:foldl(
              fun(Index,{Pos,Bit,Acc})->
                  Pos0 = Index bsr 3,
                  Mask = 1 bsl (Index band 7),
                  SeqNo = ai_utp_util:bit16(Base + Index),
                  {Bit0,Acc0} =
                    if Pos0 > Pos -> {0,<<Acc/binary,Bit/big-integer>>};
                       true ->{Bit,Acc}
                    end,
                  case array:get(SeqNo, Reorder) of
                    undefined -> {Pos0,Bit0,Acc0};
                    _ -> {Pos0,Bit0 bor Mask,Acc0}
                  end
              end,{0,0,<<>>},lists:seq(0, 799)),
  if erlang:byte_size(Acc) > 0 -> Acc;
     true -> undefined
  end.

