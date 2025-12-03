-module(aiutp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc 启动顶层监督者
-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 使用子进程规范初始化监督者
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  %% 使用 rest_for_one 策略：
  %% - Channel 依赖于 socket（需要 UDP 来发送/接收）
  %% - 如果 socket_sup 崩溃，channel_sup 必须重启（channel 变得无效）
  %% - 如果 channel_sup 崩溃，socket_sup 继续工作（socket 是独立的）
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
