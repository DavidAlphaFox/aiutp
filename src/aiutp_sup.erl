-module(aiutp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc Start the top-level supervisor
-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize the supervisor with child specifications
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  %% Use rest_for_one strategy:
  %% - Channels depend on sockets (need UDP to send/receive)
  %% - If socket_sup crashes, channel_sup must restart (channels become invalid)
  %% - If channel_sup crashes, socket_sup continues working (sockets are independent)
  SupFlags = #{strategy => rest_for_one,
               intensity => 1,
               period => 5},

  SocketSup = #{id => aiutp_socket_sup,
                start => {aiutp_socket_sup,start_link,[]},
                restart => transient,
                shutdown => 5000,
                type => supervisor,
                modules => [aiutp_socket_sup]
               },
  ChannelSup = #{id => aiutp_channel_sup,
                 start => {aiutp_channel_sup,start_link,[]},
                 restart => transient,
                 shutdown => 5000,
                 type => supervisor,
                 modules => [aiutp_channel_sup]
                },
  {ok, {SupFlags, [SocketSup,ChannelSup]}}.
