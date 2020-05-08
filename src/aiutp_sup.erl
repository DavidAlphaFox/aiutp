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
  ConnManager = #{id => aiutp_conn_manager,
                  start => {aiutp_conn_manager,start_link,[]},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => [aiutp_conn_manager]
                 },
  SocketSup = #{id => aiutp_socket_sup,
                start => {aiutp_socket_sup,start_link,[]},
                restart => transient,
                shutdown => 5000,
                type => supervisor,
                modules => [aiutp_socket_sup]
               },
  WorkerSup = #{id => aiutp_woker_sup,
                start => {aiutp_worker_sup,start_link,[]},
                restart => transient,
                shutdown => 5000,
                type => supervisor,
                modules => [aiutp_worker_sup]
               },
  {ok, {SupFlags, [ConnManager,SocketSup,WorkerSup]}}.
