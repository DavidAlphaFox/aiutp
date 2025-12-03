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
send({utp,_,Channel},Data)->
  aiutp_channel:send(Channel,Data).
recv({utp,_,Channel},Len)->
  aiutp_channel:recv(Channel,Len).
active({utp,_,Channel},V)->
  aiutp_channel:active(Channel, V).
controlling_process({utp,_,Channel},NewOwner)->
  aiutp_channel:controlling_process(Channel, NewOwner).

close({utp,_,Channel})->
  Caller = self(),
  aiutp_channel:close(Channel, Caller).
