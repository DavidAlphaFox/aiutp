%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2025, David Gao
%%% @doc
%%% uTP channel 进程的监督者（gen_statem）
%%% @end
%%% Created : 3 Dec 2025
%%%-------------------------------------------------------------------
-module(aiutp_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([new/2]).

%% 监督者回调
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API 函数
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 启动一个新的 channel 进程
%% @end
%%--------------------------------------------------------------------
-spec new(pid(), port()) -> {ok, pid()} | {error, term()}.
new(Parent, Socket) ->
    supervisor:start_child(?SERVER, [Parent, Socket]).

%%--------------------------------------------------------------------
%% @doc 启动监督者
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
        {error, {already_started, Pid :: pid()}} |
        {error, {shutdown, term()}} |
        {error, term()} |
        ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% 监督者回调
%%%===================================================================

-spec init(Args :: term()) ->
        {ok, {SupFlags :: supervisor:sup_flags(),
              [ChildSpec :: supervisor:child_spec()]}} |
        ignore.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 5
    },
    Channel = #{
        id => aiutp_channel,
        start => {aiutp_channel, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [aiutp_channel]
    },
    {ok, {SupFlags, [Channel]}}.
