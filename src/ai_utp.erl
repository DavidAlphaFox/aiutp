-module(ai_utp).

-export([open/1,open/2]).
-export([connect/3,accept/1]).
-export([listen/1,listen/2,send/2,recv/2,close/1]).
-export([active/2,controlling_process/2]).

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
send({utp,_,Worker},Data)->
  ai_utp_worker:send(Worker,Data).
recv({utp,_,Worker},Len)->
  ai_utp_worker:recv(Worker,Len).
active({utp,_,Worker},V)->
  ai_utp_worker:active(Worker, V).
controlling_process({utp,_,Worker},NewOwner)->
  ai_utp_worker:controlling_process(Worker, NewOwner).

close({utp,_,Worker})->
  Caller = self(),
  ai_utp_worker:close(Worker, Caller).
