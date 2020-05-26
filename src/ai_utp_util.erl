-module(ai_utp_util).
-export([bit16/1,bit32/1,getaddr/1]).
-export([microsecond/0,millisecond/0]).

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
microsecond()-> os:system_time(microsecond).
millisecond()-> os:system_time(millisecond).
