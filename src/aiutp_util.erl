-module(aiutp_util).

-export([bit16/1,bit32/1,getaddr/1]).
-export([bit16_random/0,bit32_random/0]).
-export([microsecond/0,millisecond/0]).
-export([clamp/3,wrapping_compare_less/3]).


-spec bit16(integer()) -> integer().
bit16(N) when is_integer(N) ->
    N band 16#FFFF.

-spec bit32(integer()) -> integer().
bit32(N) when is_integer(N) ->
    N band 16#FFFFFFFF.

getaddr(S) when is_list(S) ->
    {ok, CAddr} = inet:getaddr(S, inet),
    CAddr;
getaddr({_, _, _, _} = Addr) ->
    Addr.
microsecond()-> erlang:system_time(microsecond).
millisecond()-> erlang:system_time(millisecond).


bit16_random() ->
  <<N:16/integer>> = crypto:strong_rand_bytes(2),
  N.
bit32_random()->
  <<N:32/integer>> = crypto:strong_rand_bytes(4),
  N.

clamp(Val, Min, _Max) when Val < Min -> Min;
clamp(Val, _Min, Max) when Val > Max -> Max;
clamp(Val, _Min, _Max) -> Val.

%% @doc Compare two sequence numbers with wrapping semantics.
%% Returns true if L < R considering wrapping around Mask.
%% Mask should be 16#FFFF for 16-bit sequence numbers.
%% Example: wrapping_compare_less(65534, 1, 16#FFFF) -> true
%% because 65534 is "before" 1 when the sequence wraps at 65535.
-spec wrapping_compare_less(integer(), integer(), integer()) -> boolean().
wrapping_compare_less(L, R, Mask) ->
  Down = (L - R) band Mask,
  Up = (R - L) band Mask,
  Up < Down.
