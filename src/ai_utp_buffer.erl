-module(ai_utp_buffer).
-include("ai_utp.hrl").

-export([in/3]).

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

in(_,<<>>,Net)->{ok,Net};
in(SeqNo,Payload,
   #utp_net{state = State,
            ack_nr = SeqNo} = Net)->
  Net0 = recv(State,Payload,Net),
  recv_reorder(Net0#utp_net{ack_nr = ai_utp_util:bit16(SeqNo + 1)});
in(SeqNo,Payload,
   #utp_net { reorder = OD } = Net) ->
  case orddict:is_key(SeqNo, OD) of
    true -> duplicate;
    false -> {ok,Net#utp_net{ reorder = orddict:store(SeqNo, Payload, OD) }}
  end.

recv('FIN_SENT',_,Net)-> Net;
recv('CONNECTED',<<>>,Net) -> Net;
recv('CONNECTED',Payload,
     #utp_net{inbuf = InBuf} = Net) ->
  Net#utp_net{inbuf = <<InBuf/binary,Payload/binary>>}.


recv_reorder({ reorder = [] } = Net) ->{ok,Net};
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
