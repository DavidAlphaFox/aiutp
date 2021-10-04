-module(aiutp_queue).

-export([new/0,
         size/1,
         pop_front/1,
         pop_back/1,
         push_front/2,
         push_back/2,
         front/1,
         back/1,
         empty/1,
         to_list/1]).

-record(aiutp_queue, {queue = queue:new(), size = 0}).

new() -> #aiutp_queue{}.

size(Queue) -> Queue#aiutp_queue.size.

pop_front(#aiutp_queue{queue = Q, size = Size} = Queue) ->
  Q2 = queue:drop(Q),
  Queue#aiutp_queue{queue = Q2, size = Size - 1}.

pop_back(#aiutp_queue{queue = Q, size = Size} = Queue) ->
  Q2 = queue:drop_r(Q),
  Queue#aiutp_queue{queue = Q2, size = Size - 1}.

push_back(Val,#aiutp_queue{queue = Q, size = Size}= Queue) ->
  Q2 = queue:in(Val, Q),
  Queue#aiutp_queue{queue = Q2, size = Size + 1}.

push_front(Val,#aiutp_queue{queue = Q, size = Size}= Queue) ->
  Q2 = queue:in_r(Val, Q),
  Queue#aiutp_queue{queue = Q2, size = Size + 1}.

empty(#aiutp_queue{size = Size}) when Size =:= 0 -> true;
empty(_) -> false.

front(#aiutp_queue{queue = Q}) -> queue:get(Q).
back(#aiutp_queue{queue = Q})-> queue:get_r(Q).
to_list(#aiutp_queue{queue  = Q}) -> queue:to_list(Q).
