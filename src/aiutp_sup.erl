%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc aiutp 顶层监督者
%%%
%%% 使用 simple_one_for_one 策略动态管理 socket 监督树实例。
%%% 每次调用 new/1 都会创建一个新的 aiutp_socket_sup 子树。
%%%
%%% 监督树结构：
%%% ```
%%% aiutp_sup (simple_one_for_one)
%%% └── aiutp_socket_sup (one_for_all) ← 每个 socket 一个
%%%     ├── aiutp_socket
%%%     └── aiutp_channel_sup (simple_one_for_one)
%%%         └── aiutp_channel (多个)
%%% ```
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_sup).

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

%% @doc 启动顶层监督者
-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc 创建一个新的 socket 监督树
%%
%% 每次调用会创建一个独立的 aiutp_socket_sup 实例，
%% 该实例管理一个 socket 进程及其所有 channel。
%%
%% @param Port UDP 端口号
%% @param Options 选项列表
%% @returns {ok, SocketPid} | {error, Reason}
-spec new(inet:port_number(), list()) -> {ok, pid()} | {error, term()}.
new(Port, Options) ->
    case supervisor:start_child(?SERVER, [Port, Options]) of
        {ok, SocketSup} ->
            %% 从 socket_sup 获取 socket 进程的 pid
            aiutp_socket_sup:get_socket(SocketSup);
        {error, _} = Error ->
            Error
    end.

%%%===================================================================
%%% 监督者回调
%%%===================================================================

%% @doc 使用子进程规范初始化监督者
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 5
    },
    SocketSup = #{
        id => aiutp_socket_sup,
        start => {aiutp_socket_sup, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => supervisor,
        modules => [aiutp_socket_sup]
    },
    {ok, {SupFlags, [SocketSup]}}.
