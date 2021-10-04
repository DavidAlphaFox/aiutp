-module(aiutp_test).

-export([server/0,client/0]).

server()->
  application:start(aiutp),
  {ok,Socket} = aiutp:open(6666),
  aiutp:listen(Socket),
  {ok,NewSocket} = aiutp:accept(Socket),
  aiutp:active(NewSocket, true),
  {ok,NewSocket}.
client()->
  application:start(aiutp),
  {ok,Socket} = aiutp:open(6667),
  {ok,NewSocket} = aiutp:connect(Socket,{127,0,0,1},6666),
  aiutp:send(NewSocket,<<"hello">>),
  aiutp:active(NewSocket, true),
  {ok,NewSocket}.
