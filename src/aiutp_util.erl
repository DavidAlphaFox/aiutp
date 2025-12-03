-module(aiutp_util).

-export([bit16/1,bit32/1,getaddr/1]).
-export([bit16_random/0,bit32_random/0]).
-export([microsecond/0,millisecond/0]).
-export([clamp/3,wrapping_compare_less/3]).

%% 类型定义
-type ipv4_address() :: {0..255, 0..255, 0..255, 0..255}.

-spec bit16(integer()) -> 0..65535.
bit16(N) when is_integer(N) ->
    N band 16#FFFF.

-spec bit32(integer()) -> 0..4294967295.
bit32(N) when is_integer(N) ->
    N band 16#FFFFFFFF.

-spec getaddr(string() | ipv4_address()) -> ipv4_address().
getaddr(S) when is_list(S) ->
    {ok, CAddr} = inet:getaddr(S, inet),
    CAddr;
getaddr({_, _, _, _} = Addr) ->
    Addr.

-spec microsecond() -> integer().
microsecond()-> erlang:system_time(microsecond).

-spec millisecond() -> integer().
millisecond()-> erlang:system_time(millisecond).

-spec bit16_random() -> 0..65535.
bit16_random() ->
  <<N:16/integer>> = crypto:strong_rand_bytes(2),
  N.

-spec bit32_random() -> 0..4294967295.
bit32_random()->
  <<N:32/integer>> = crypto:strong_rand_bytes(4),
  N.

-spec clamp(number(), number(), number()) -> number().
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
