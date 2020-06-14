-module(ai_utp_process).

-export([new/0,
         enqueue_sender/3,
         enqueue_receiver/3,
         reenqueue_receiver/4,
         dequeue_receiver/1,
         fill_send_window/2,
         bytes_in_recv_buffer/1,
         recv_buffer_empty/1,
         apply_all/2,
         apply_senders/2,
         apply_receivers/2,
         clear_senders/1,
         error_all/2,
         flush/1
        ]).
-record(utp_process,{
                     receiver :: queue:queue(),
                     sender   :: queue:queue()
                    }).

new()->
  #utp_process{receiver = queue:new(),
               sender = queue:new()}.

flush(#utp_process{sender = Sender} = PI)->
  Payload = lists:foldl(
    fun({sender,From,Data},Acc)->
        gen_server:reply(From, ok),
        <<Acc/binary,Data/binary>>
    end,<<>>,queue:to_list(Sender)),
  {Payload,PI#utp_process{sender = queue:new()}}.

apply_all(PI, F) ->
  apply_senders(PI, F),
  apply_receivers(PI, F).

apply_senders(PI, F) ->
  [F(From) || From <- all_senders(PI)].

apply_receivers(PI, F) ->
  [F(From) || From <- all_receivers(PI)].

clear_senders(PI) ->
  PI#utp_process{sender = queue:new()}.

all_senders(#utp_process{ sender = SQ }) ->
  [From || {sender, From, _Data} <- queue:to_list(SQ)].

all_receivers(#utp_process{ receiver = RQ }) ->
  [From || {receiver, From, _, _} <- queue:to_list(RQ)].

enqueue_receiver(From, Length,
                 #utp_process{ receiver = RQ } = PI) ->
  NQ = queue:in({receiver, From, Length, <<>>}, RQ),
  PI#utp_process{receiver = NQ }.

dequeue_receiver(#utp_process{receiver= RQ } = PI) ->
  case queue:out(RQ) of
    {{value, Item}, NQ} ->
      {ok, Item, PI#utp_process{ receiver = NQ }};
    {empty, _Q} ->empty
  end.

reenqueue_receiver(From, Length, Data,
                 #utp_process{ receiver = RQ} = PI) ->
  NQ = queue:in_r({receiver, From, Length, Data}, RQ),
  PI#utp_process{ receiver = NQ }.

enqueue_sender(From, Data,
               #utp_process{ sender = SQ } = PI) when is_binary(Data) ->
  NQ = queue:in({sender, From, Data}, SQ),
  PI#utp_process{ sender = NQ }.

fill_send_window(N, #utp_process{ sender = SQ } = PI) when is_integer(N) ->
  case dequeue(N, SQ, <<>>) of
    {done, Bin, SQ1} ->
      {filled, Bin, PI#utp_process{ sender = SQ1}};
    {partial, Bin, SQ1} ->
      {partial, Bin, PI#utp_process{ sender = SQ1}};
    zero -> zero
  end.

dequeue(0, _Q, Bin) when erlang:byte_size(Bin) == 0 -> zero;
dequeue(0, Q, Bin) -> {done, Bin, Q};
dequeue(N, Q, Acc) ->
  {R, NQ} = queue:out(Q),
  case R of
    empty when erlang:byte_size(Acc) == 0 -> zero;
    empty when erlang:byte_size(Acc) > 0 -> {partial, Acc, Q};
    {value, {sender, From, Data}} when byte_size(Data) =< N ->
      gen_server:reply(From, ok),
      dequeue(N - byte_size(Data), NQ, <<Acc/binary, Data/binary>>);
    {value, {sender, From, Data}} when byte_size(Data) > N ->
      <<Take:N/binary, Rest/binary>> = Data,
      dequeue(0, queue:in_r({sender, From, Rest}, NQ),
              <<Acc/binary, Take/binary>>)
  end.

%% @doc Predicate: is the receive buffer empty
%% This function is a faster variant of `bytes_in_recv_buffer/1` for the 0 question case
%% @end
recv_buffer_empty(#utp_process{ receiver = RQ }) ->
  queue:is_empty(RQ).

%% @doc Return how many bytes there are left in the receive buffer
%% @end
bytes_in_recv_buffer(#utp_process { receiver = RQ }) ->
  L = queue:to_list(RQ),
  lists:sum([byte_size(Payload) || {receiver, _From, _Sz, Payload} <- L]).




error_all(ProcessInfo, ErrorReason) ->
  apply_all(ProcessInfo,
            fun(From) ->
                gen_server:reply(From, {error, ErrorReason})
            end),
  new().
