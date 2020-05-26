-module(aiutp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 1,
               period => 5},
  ConnManager = #{id => ai_utp_conn,
                  start => {ai_utp_conn,start_link,[]},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => [ai_utp_conn]
                 },
  SocketSup = #{id => ai_utp_socket_sup,
                start => {ai_utp_socket_sup,start_link,[]},
                restart => transient,
                shutdown => 5000,
                type => supervisor,
                modules => [ai_utp_socket_sup]
               },
  WorkerSup = #{id => aiutp_woker_sup,
                start => {ai_utp_worker_sup,start_link,[]},
                restart => transient,
                shutdown => 5000,
                type => supervisor,
                modules => [ai_utp_worker_sup]
               },
  {ok, {SupFlags, [ConnManager,SocketSup,WorkerSup]}}.
