-module(ai_utp).

-export([open/1,open/2]).
-export([connect/3,accept/1]).
-export([listen/1,listen/2]).

open(Port) -> ai_utp_socket_sup:open(Port,[]).
open(Port,Options) -> ai_utp_socket_sup:open(Port,Options).

connect({utp,UTPSocket},Address,Port)->
  ai_utp_socket:connect(UTPSocket, Address, Port).

listen({utp,UTPSocket})->
  ai_utp_socket:listen(UTPSocket, []).
listen({utp,UTPSocket},Options)->
  ai_utp_socket:listen(UTPSocket, Options).

accept({utp,UTPSocket})->
  ai_utp_socket:accept(UTPSocket).
