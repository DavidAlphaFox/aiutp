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

-export_type([aiutp_queue/0, aiutp_queue/1]).

-record(aiutp_queue, {queue = queue:new() :: queue:queue(),
                      size = 0 :: non_neg_integer()}).

-opaque aiutp_queue() :: #aiutp_queue{}.
-opaque aiutp_queue(T) :: #aiutp_queue{queue :: queue:queue(T)}.

-spec new() -> aiutp_queue().
new() -> #aiutp_queue{}.

-spec size(aiutp_queue()) -> non_neg_integer().
size(Queue) -> Queue#aiutp_queue.size.

-spec pop_front(aiutp_queue()) -> aiutp_queue().
pop_front(#aiutp_queue{queue = Q, size = Size} = Queue) ->
  Q2 = queue:drop(Q),
  Queue#aiutp_queue{queue = Q2, size = Size - 1}.

-spec pop_back(aiutp_queue()) -> aiutp_queue().
pop_back(#aiutp_queue{queue = Q, size = Size} = Queue) ->
  Q2 = queue:drop_r(Q),
  Queue#aiutp_queue{queue = Q2, size = Size - 1}.

-spec push_back(term(), aiutp_queue()) -> aiutp_queue().
push_back(Val,#aiutp_queue{queue = Q, size = Size}= Queue) ->
  Q2 = queue:in(Val, Q),
  Queue#aiutp_queue{queue = Q2, size = Size + 1}.

-spec push_front(term(), aiutp_queue()) -> aiutp_queue().
push_front(Val,#aiutp_queue{queue = Q, size = Size}= Queue) ->
  Q2 = queue:in_r(Val, Q),
  Queue#aiutp_queue{queue = Q2, size = Size + 1}.

-spec empty(aiutp_queue()) -> boolean().
empty(#aiutp_queue{size = Size}) when Size =:= 0 -> true;
empty(_) -> false.

-spec front(aiutp_queue()) -> term().
front(#aiutp_queue{queue = Q}) -> queue:get(Q).

-spec back(aiutp_queue()) -> term().
back(#aiutp_queue{queue = Q})-> queue:get_r(Q).

-spec to_list(aiutp_queue()) -> [term()].
to_list(#aiutp_queue{queue  = Q}) -> queue:to_list(Q).
