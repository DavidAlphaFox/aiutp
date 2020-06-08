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
  case orddict:is_key(SeqNo, OD) of
    true -> duplicate;
    false ->
      Less = ai_utp_util:wrapping_compare_less(SeqNo,AckNR, ?ACK_NO_MASK),
      if Less == true -> duplicate;
         true ->
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
           true -> ack_distance_packet(WindowStart,AckDistance,queue:to_list(OutBuf))
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
                packet = #utp_packet{seq_no = WindowStart}} = H|T] = OutBuf) ->
  Diff = ai_utp_util:bit16(WindowStart - AckNo),
  if Diff == 1 ->
      Packet =
        if Wanted >= ?DUPLICATE_ACKS_BEFORE_RESEND ->
            H#utp_packet_wrap{wanted = Wanted + 1, need_resend = true};
           true -> H#utp_packet_wrap{wanted = Wanted + 1}
        end,
      [Packet|T];
     true -> OutBuf
  end;
fast_resend(_,_,OutBuf) -> OutBuf.

sack_packet(_,undefined,OutBuf)-> {[],OutBuf};
sack_packet(AckNo,Bits,OutBuf)->
  Max = erlang:byte_size(Bits) * 8,
  {_,Acks,UnAcks} =
    lists:foldl(fun(#utp_packet_wrap{
                       packet = #utp_packet{seq_no = SeqNo}} = Warp,
                    {Count,Acks0,UnAcks0} = Acc)->
                    Pos = ai_utp_util:bit16(SeqNo - AckNo - 2),
                    if Pos < Max ->sack_packet(Bits,Pos,Warp,Acc);
                       true ->{Count,Acks0,[Warp|UnAcks0]}
                    end
                end, {0,[],[]}, lists:reverse(OutBuf)),
  {Acks,UnAcks}.

ack_bit(Bits,Pos)->
  Pos0 = (Pos bsr 3) * 8 + 7 - (Pos band 7),
  if
    Pos0 > 0 ->
      <<_:Pos0,Bit:1,_/bits>> = Bits,
      Bit;
    true ->
      <<Bit:1,_/bits>> = Bits,
      Bit
  end.

sack_packet(Bits,Pos,Warp,{Count,Acks,UnAcks})->
  Bit = ack_bit(Bits,Pos),
  if Bit == 1 -> {Count + 1,[Warp|Acks],UnAcks};
     true ->
      if Count > ?DUPLICATE_ACKS_BEFORE_RESEND ->
          {Count,Acks,[Warp#utp_packet_wrap{need_resend = true}|UnAcks]};
         true ->
          {Count,Acks,[Warp|UnAcks]}
      end
  end.

sack(_,#utp_net{reorder = []}) -> undefined;
sack(Base,#utp_net{reorder = Reorder}) ->
  sack(Base,Reorder,undefined,<<>>).


sack(_,[],I,Bin)->
  Bin0 =
    if I == undefined -> Bin;
       true -> <<Bin/binary,I/big-integer>>
    end,
  Size = erlang:byte_size(Bin0),
  Rem = Size rem 4,
  if (Size > 0) and (Rem > 0)->
      Offset = (4 - Rem) * 8,
      <<Bin0/binary,0:Offset>>;
     true -> Bin0
  end;
sack(Base,[{SeqNo,_}|T],I,Bin)->
  Index = ai_utp_util:bit16(SeqNo - Base),
  if Index >= 100 -> sack(Base,[],I,Bin);
     Index >= 0 ->
      Size = erlang:byte_size(Bin),
      IndexDiff = (Index bsr 3) - (Size - 1),
      {Bin0,I0} =
        if (IndexDiff > 1) andalso (I == undefined)->
            Offset = (IndexDiff - 1) * 8,
            {<<Bin/bits,0:Offset>>,0};
           IndexDiff > 1 ->
            Offset = (IndexDiff - 2) * 8,
            {<<Bin/bits,I/big-integer,0:Offset>>,0};
           true -> {Bin,I}
        end,
      Mask = 1 bsl (Index band 7),
      I1 =
        if I0 == undefined -> Mask bor 0;
           true -> Mask bor I0
        end,
      sack(Base,T,I1,Bin0);
     true -> sack(Base,T,I,Bin)
  end.
