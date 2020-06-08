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
   #utp_net { reorder = OD,ack_nr = AckNR } = Net) ->
  Less = ai_utp_util:wrapping_compare_less(SeqNo,AckNR, ?ACK_NO_MASK),
  Diff = ai_utp_util:bit16(SeqNo - AckNR),
  if Less == true -> duplicate;
     Diff > ?REORDER_BUFFER_MAX_SIZE -> {ok,Net};
     true ->
      case orddict:is_key(SeqNo, OD) of
        true -> duplicate;
        false ->
          {ok,Net#utp_net{ reorder = orddict:store(SeqNo, Payload, OD) }}
      end
  end.

recv(?ESTABLISHED,<<>>,Net) -> Net;
recv(?ESTABLISHED,Payload,
     #utp_net{inbuf = InBuf} = Net) ->
  Net#utp_net{inbuf = <<InBuf/binary,Payload/binary>>};
recv(_,_,Net) -> Net.



recv_reorder(#utp_net{ reorder = [] } = Net) ->{ok,Net};
%% 接收对端最后一个数据包
recv_reorder(#utp_net{state = State,ack_nr = SeqNo,
                      got_fin = true,eof_seq_no = SeqNo,
                      reorder = [{SeqNo, PL} | R]} = Net) ->
  Net0 = recv(State, PL, Net),
  {fin,Net0#utp_net{reorder = R}};
recv_reorder(#utp_net{state = State,ack_nr = SeqNo,
                      reorder = [{SeqNo, PL} | R]} = Net) ->
  Net0 = recv(State, PL, Net),
  recv_reorder(Net0#utp_net{reorder = R,
                            ack_nr = ai_utp_util:bit16(SeqNo + 1)});
recv_reorder(Net)-> {ok,Net}.

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
  io:format("AckNo: ~p Map: ~p~n",[AckNo,Map]),
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

sack(_,#utp_net{reorder = []}) -> undefined;
sack(Base,#utp_net{reorder = Reorder}) ->
  sack(Base,Reorder,0,#{0 => 0}).

sack(_,[],0,#{0 := 0}) -> undefined;
sack(AckNo,[],Pos,Map)->
  io:format("AckNo: ~p Map: ~p~n",[AckNo,Map]),
  Bin0 = lists:foldl(
           fun(BI,BAcc)->
               Bits = maps:get(BI,Map),
               <<BAcc/binary,Bits/big-integer>>
           end, <<>>, lists:seq(0, Pos)),
  Size = erlang:byte_size(Bin0),
  Rem = Size rem 4,
  if Rem > 0->
      Offset = (4 - Rem) * 8,
      <<Bin0/binary,0:Offset>>;
     true -> Bin0
  end;
sack(Base,[{SeqNo,_}|T],Pos,Map)->
  Less = ai_utp_util:wrapping_compare_less(Base, SeqNo, ?ACK_NO_MASK),
  if (Less == true) or (SeqNo == Base)->
      Index = ai_utp_util:bit16(SeqNo - Base),
      %% 0 - 799,共800个元素
      if Index >= 800 -> sack(Base,[],Pos,Map);
         true ->
          Pos0 = Index bsr 3,
          Mask = 1 bsl (Index band 7),
          if
            Pos0 > Pos ->
              Map0 = lists:foldl(
                       fun(BI,BAcc)-> maps:put(BI,0,BAcc) end,
                       Map,lists:seq(Pos +1 ,Pos0)),
              Bits = maps:get(Pos0,Map0),
              Bits0 = Mask bor Bits,
              sack(Base,T,Pos0,maps:put(Pos0,Bits0,Map0));
            true ->
              Bits = maps:get(Pos,Map),
              Bits0 = Mask bor Bits,
              sack(Base,T,Pos,maps:put(Pos0,Bits0,Map))
          end
      end;
     true -> sack(Base,T,Pos,Map)
  end.
