%% @doc LEDBAT calculations
%%
%% == Overview ==
%% The code in this module maintains a LEDBAT calculation based upon
%% delay samples. A sample is the difference between two clocks, A and
%% B which we assume are moving onwards at the same speed, though we
%% do account for skew.
%%
%% The difference between our and their clock (A and B) is our base
%% delay. Any deviation from this difference in the positive direction
%% means we have a queuing delay of that amount. A deviation in the
%% negative direction means that we have to update our base delay to
%% the new value.
%%
%% == Base Delay and Current Delay ==
%% When we take in samples, we store these samples historically. This
%% is done such that small perturbances in the delay will not affect
%% the greater scheme of things. We track two kinds of delays: The
%% base history and the current (queue) history. Every minute, we
%% update and rotate the history tables so we never latch onto a
%% simplified view of the real world.
%% @end
-module(ai_utp_ledbat).

-define(BASE_DELAY_HISTORY_SIZE, 12).
-define(CUR_DELAY_SIZE, 2).

-export([
         new/1,
         add_sample/2,
         shift/2,
         base_delay/1,
         get_value/1,
         clock_tick/1,
         compare_less/2
        ]).

-record(ai_utp_ledbat, {
                        base_history:: queue:queue(),
                        delay_base :: integer(),
                        last_sample :: integer(),
                        cur_delay_history   :: queue:queue()
                       }).

%% @doc Create a new LEDBAT structure based upon the first sample
%% @end
new(Sample) ->
  BaseQueue =
    lists:foldr(fun(_E, Q) -> queue:in(Sample, Q) end,
                queue:new(),
                lists:seq(1, ?BASE_DELAY_HISTORY_SIZE)),
  DelayQueue =
    lists:foldr(fun(_E, Q) -> queue:in(0, Q) end,
                queue:new(),
                lists:seq(1, ?CUR_DELAY_SIZE)),
  #ai_utp_ledbat{
     base_history = BaseQueue,
     delay_base     = Sample,
     last_sample    = Sample,
     cur_delay_history = DelayQueue
    }.

%% @doc Shift the base delay by an `Offset' amount.  Note that
%% shifting by a positive value moves the base delay such that the
%% queueing delay gets smaller. This is used in several spots to
%% account for the fact that we just realized our queueing delay is
%% too large. To make it smaller we shift the base delay up, which
%% affects the queueing delay down.
%% @end
shift(none,_) -> none;
shift(#ai_utp_ledbat { base_history = BQ } = LEDBAT, Offset) ->
  New_Queue =
    queue_map(fun(E) -> ai_utp_util:bit32(E + Offset) end,BQ),
  LEDBAT#ai_utp_ledbat { base_history = New_Queue }.

%% @doc Add a new sample to the LEDBAT structure
%% @end
add_sample(none, Sample) -> new(Sample);
add_sample(#ai_utp_ledbat{ base_history = BQ,
                           delay_base= DelayBase,
                           cur_delay_history   = DQ } = LEDBAT, Sample) ->
  %% 取出采样的第一个
  {{value, BaseIncumbent}, BQ2} = queue:out(BQ),
  %% 当新的采样比历史采样小的时候才进行基本队列更新
  NewBQ =
    case compare_less(Sample, BaseIncumbent) of
      true -> queue:in_r(Sample, BQ2);
      false ->BQ
    end,
  %% 当新采样比延迟采样小的时候，使用新的采样
  NewDelayBase =
    case compare_less(Sample, DelayBase) of
      true -> Sample;
      false -> DelayBase
    end,
  Delay = ai_utp_util:bit32(Sample - NewDelayBase),
  NewDQ = update_history(Delay, DQ),
  LEDBAT#ai_utp_ledbat{
    base_history = NewBQ,
    delay_base = Delay,
    last_sample = Sample,
    cur_delay_history = NewDQ }.

%% @doc Bump the internal structure by rotating the history tables
%% @end
clock_tick(none) ->none;
clock_tick(#ai_utp_ledbat{
              base_history  = BaseQ,
              last_sample = Sample,
              delay_base = DelayBase } = LEDBAT) ->
    NewBaseQ = queue:in_r(Sample, queue:drop(rotate(BaseQ))),
    NewDelayBase = minimum_by(fun compare_less/2, queue:to_list(NewBaseQ)),
    LEDBAT#ai_utp_ledbat { base_history = NewBaseQ,
                    delay_base = min(NewDelayBase, DelayBase) }.

%% @doc Fetch the delay base of the LEDBAT structure
%% @end
base_delay(#ai_utp_ledbat{ delay_base = DB}) ->DB.

get_value(none)-> undefined;
%% @doc Get out the current estimate.
%% @end
get_value(#ai_utp_ledbat { cur_delay_history = DelayQ }) ->
  lists:min(queue:to_list(DelayQ)).

%% @doc Compare if L < R taking wrapping into account
%% @end
compare_less(L, R) ->
    %% To see why this is correct, imagine a unit circle
    %% One direction is walking downwards from the L clockwise until
    %%  we hit the R
    %% The other direction is walking upwards, counter-clockwise
    Down = ai_utp_util:bit32(L - R),
    Up   = ai_utp_util:bit32(R - L),

    %% If the walk-up distance is the shortest, L < R, otherwise R < L
    Up < Down.


%% ----------------------------------------------------------------------
update_history(Sample, Queue) ->
    {{value, _ThrowAway}, RestOfQueue} = queue:out(Queue),
    queue:in(Sample, RestOfQueue).

minimum_by(_F, []) -> error(badarg);
minimum_by(F, [H | T]) -> minimum_by(F, T, H).

minimum_by(_Comparator, [], M) -> M;
minimum_by(Comparator, [H | T], M) ->
  case Comparator(H, M) of
    true -> minimum_by(Comparator, T, H);
    false -> minimum_by(Comparator, T, M)
  end.

rotate(Q) ->
    {{value, E}, RQ} = queue:out(Q),
    queue:in(E, RQ).

queue_map(F, Q) ->
    L = queue:to_list(Q),
    queue:from_list(lists:map(F, L)).









