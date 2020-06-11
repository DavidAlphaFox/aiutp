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
     Diff > ?HALF_CIRCLE -> {ok,Net};
     RSize >= ?REORDER_BUFFER_MAX_SIZE -> {ok,Net};
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
  %% 最老的序列号
  WindowStart = ai_utp_util:bit16(SeqNR - CurWindowPackets),
  %% AckNo必须小于SeqNR，distance = 0的时候，不应该进行ack
  AckDistance = ack_distance(CurWindowPackets, SeqNR, AckNo),
  {Packets,OutBuf0} =
    if AckDistance == 0 -> {[],OutBuf};
       true -> ack_packet(WindowStart,AckNo,OutBuf,[])
    end,
  {Lost,Packets0,OutBuf1} = sack_packet(AckNo,SeqNR, SAcks, OutBuf0),
  OutBuf2 = fast_resend(AckNo,SeqNR,OutBuf1),
  {Lost,Packets ++ Packets0,
   Net#utp_net{outbuf = OutBuf2,
               cur_window_packets = CurWindowPackets - AckDistance}}.

fast_resend(AckNo,SeqNR,OutBuf)->
  SeqNo = ai_utp_util:bit16(AckNo + 1),
  Diff = ai_utp_util:bit16(SeqNR - SeqNo),
  if Diff >= 1 ->
      Wrap = array:get(SeqNo,OutBuf),
      #utp_packet_wrap{ wanted = Wanted} = Wrap,
      if Wanted >= ?DUPLICATE_ACKS_BEFORE_RESEND ->
          array:set(SeqNo,Wrap#utp_packet_wrap{
                            need_resend = true },OutBuf);
         true ->
          array:set(SeqNo,Wrap#utp_packet_wrap{
                            wanted = Wanted + 1},OutBuf)
      end;

     true -> OutBuf
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

ack_packet(AckNo,AckNo,OutBuf,Packets)->
  case array:get(AckNo,OutBuf) of
    undefined -> {lists:reverse(Packets),OutBuf};
    Packet ->
      {lists:reverse([Packet|Packets]),
       array:set(AckNo,undefined,OutBuf)}
  end;
ack_packet(Index,AckNo,OutBuf,Packets)->
  case array:get(Index,OutBuf) of
    undefined ->
      %% 前面某个Ack包通过SACK，Ack掉这个包了
      ack_packet(ai_utp_util:bit16(Index + 1),AckNo,
                 OutBuf,Packets);
     Packet ->
      ack_packet(ai_utp_util:bit16(Index + 1),AckNo,
                 array:set(Index,undefined,OutBuf),
                 [Packet|Packets])
  end.
  
sack_map(<<>>,_,Map) -> Map;
sack_map(<<Bits/big-integer,Rest/binary>>,N,Map) ->
  sack_map(Rest,N+1,maps:put(N,Bits,Map)).

need_resend(Index,Acked,Wrap,OutBuf)->
  if Acked > ?DUPLICATE_ACKS_BEFORE_RESEND ->
      array:set(Index,Wrap#utp_packet_wrap{need_resend = true},
                OutBuf);
     true -> OutBuf
  end.

sack_packet(Last,_,Last,_,OutBuf,Packets,Acked,Lost)->
  {Lost,Acked,lists:reverse(Packets),OutBuf};
sack_packet(Index,Base, Last,Map, OutBuf, Packets,Acked,Lost)->
  case array:get(Index,OutBuf) of
    undefined ->
      %% 之前的包被Ack了
      sack_packet(ai_utp_util:bit16(Index - 1),Base, Last,
                  Map,OutBuf, Packets,Acked + 1,Lost);
    Packet ->
      BitIndex = ai_utp_util:bit16(Index - Base),
      Pos = BitIndex bsr 3,
      case maps:get(Pos,Map) of
        0 ->
          OutBuf0 = need_resend(Index,Acked,Packet,OutBuf),
          sack_packet(ai_utp_util:bit16(Index - 1),Base, Last,
                      Map,OutBuf0, Packets,Acked,Lost + 1);
        Bit ->
          Mask = 1 bsl (Index band 7),
          Set = Bit band Mask,
          if Set == 0 ->
              OutBuf0 = need_resend(Index,Acked,Packet,OutBuf),
              sack_packet(ai_utp_util:bit16(Index - 1),Base, Last,
                          Map,OutBuf0, Packets,Acked,Lost + 1);
             true ->
              sack_packet(ai_utp_util:bit16(Index - 1),Base, Last,
                          Map,array:set(Index,undefined,OutBuf),
                          [Packet|Packets],Acked + 1,Lost)
          end
      end
  end.


%% AckNo+2 =< SACK < SeqNR - 1
sack_packet(_, _, undefined, OutBuf)-> {0,[],OutBuf};
sack_packet(AckNo,SeqNR,Bits,OutBuf)->
  Max = erlang:byte_size(Bits) * 8,
  Map = sack_map(Bits,0,#{}),
  Base0 = ai_utp_util:bit16(AckNo + 1),
  Base = ai_utp_util:bit16(AckNo + 2),
  Last = ai_utp_util:bit16(Base + Max - 1),
  SeqNo = ai_utp_util:bit16(SeqNR - 1),
  Less = ai_utp_util:wrapping_compare_less(Last,SeqNo,?ACK_NO_MASK),
  if Less /= true -> {0,[],OutBuf};
     true ->
      {Lost,_,Packets,OutBuf0} = sack_packet(Last,Base,Base0,Map, OutBuf,[],0,0),
      case array:get(Base0,OutBuf0) of
        undefined -> {Lost,Packets,OutBuf}; %% 此种情况不应该发生
        Wrap ->
          {Lost,Packets,
           array:set(Base0,Wrap#utp_packet_wrap{need_resend = true},OutBuf0)}
      end
  end.
%% 生成SACK
%% AckNo +2 =< SACK =< AckNo + 801
sack(_,#utp_net{inbuf_size = 0})-> undefined;
sack(Base,#utp_net{inbuf = InBuf,inbuf_size = RSize}) ->
  build_sack(Base,InBuf,RSize,0,0,#{0 => 0}).
build_sack(_,_,_,_,0,#{0 := 0}) -> undefined;
build_sack(_,_,0,_,Pos,Map)->
  lists:foldl(fun(BI,BAcc)->
                  Bits = maps:get(BI,Map),
                  <<BAcc/binary,Bits/big-integer>>
              end, <<>>, lists:seq(0, Pos));
build_sack(_,_,_,63,Pos,Map)->
  lists:foldl(fun(BI,BAcc)->
                  Bits = maps:get(BI,Map),
                  <<BAcc/binary,Bits/big-integer>>
              end, <<>>, lists:seq(0, Pos));
build_sack(Base,InBuf,RSize,Index,Pos,Map)->
  SeqNo = ai_utp_util:bit16(Base + Index),
  Pos0 = Index bsr 3,
  Mask = 1 bsl (Index band 7),
  Map0 =
    if Pos0 > Pos ->
        lists:foldl(fun(BI,BAcc)-> maps:put(BI,0,BAcc) end,
                    Map,lists:seq(Pos +1 ,Pos0));
       true -> Map
    end,
  case array:get(SeqNo,InBuf) of
    undefined -> build_sack(Base,InBuf,RSize,Index + 1,Pos0,Map0);
    {SeqNo,_} ->
      Bits = maps:get(Pos0,Map0),
      Bits0 = Mask bor Bits,
      build_sack(Base,InBuf,RSize - 1,Index + 1, Pos0,maps:put(Pos,Bits0,Map0))
  end.
