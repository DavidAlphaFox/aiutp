-module(aiutp).

-export([open/1,open/2]).
-export([connect/3,accept/1]).
-export([listen/1,listen/2,send/2,recv/2,close/1]).
-export([active/2,controlling_process/2]).

open(Port) -> aiutp_socket_sup:open(Port,[]).
open(Port,Options) -> aiutp_socket_sup:open(Port,Options).

connect({utp,UTPSocket},Address,Port)->
  aiutp_socket:connect(UTPSocket, Address, Port).

listen({utp,UTPSocket})->
  aiutp_socket:listen(UTPSocket, []).
listen({utp,UTPSocket},Options)->
  aiutp_socket:listen(UTPSocket, Options).

accept({utp,UTPSocket})->
  aiutp_socket:accept(UTPSocket).
send({utp,_,Worker},Data)->
  aiutp_worker:send(Worker,Data).
recv({utp,_,Worker},Len)->
  aiutp_worker:recv(Worker,Len).
active({utp,_,Worker},V)->
  aiutp_worker:active(Worker, V).
controlling_process({utp,_,Worker},NewOwner)->
  aiutp_worker:controlling_process(Worker, NewOwner).

close({utp,_,Worker})->
  Caller = self(),
  aiutp_worker:close(Worker, Caller).
