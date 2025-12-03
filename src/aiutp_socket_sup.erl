%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc uTP 套接字监督者
%%%
%%% 每个 aiutp_socket_sup 实例管理一个 socket 及其所有 channel。
%%% 使用 one_for_all 策略确保故障隔离：
%%% - socket 崩溃时，channel_sup 也会终止，所有 channel 被清理
%%% - channel_sup 崩溃时，socket 也会终止，整个连接组被重置
%%%
%%% 监督树结构：
%%% ```
%%% aiutp_socket_sup (one_for_all)
%%% ├── aiutp_channel_sup (simple_one_for_one)
%%% │   └── aiutp_channel (多个)
%%% └── aiutp_socket (依赖 channel_sup)
%%% ```
%%%
%%% 注意：channel_sup 必须先于 socket 启动，因为 socket 启动时需要
%%% 知道 channel_sup 的 pid 以便创建新 channel。
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([get_socket/1]).
-export([get_channel_sup/1]).

%% 监督者回调
-export([init/1]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动 socket 监督者
%%
%% @param Port UDP 端口号
%% @param Options 选项列表
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(inet:port_number(), list()) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(Port, Options) ->
    supervisor:start_link(?MODULE, [Port, Options]).

%% @doc 获取 socket 进程的 pid
%%
%% @param Sup 监督者 pid
%% @returns {ok, SocketPid} | {error, not_found}
-spec get_socket(pid()) -> {ok, pid()} | {error, not_found}.
get_socket(Sup) ->
    find_child(Sup, aiutp_socket).

%% @doc 获取 channel_sup 进程的 pid
%%
%% @param Sup 监督者 pid
%% @returns {ok, ChannelSupPid} | {error, not_found}
-spec get_channel_sup(pid()) -> {ok, pid()} | {error, not_found}.
get_channel_sup(Sup) ->
    find_child(Sup, aiutp_channel_sup).

%%%===================================================================
%%% 监督者回调
%%%===================================================================

%% @private
%% @doc 初始化监督者
%%
%% 启动顺序：先 channel_sup，后 socket。
%% socket 启动时会通过 supervisor:which_children 获取 channel_sup 的 pid。
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Port, Options]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5
    },

    %% channel_sup 必须先启动，socket 依赖它
    ChannelSup = #{
        id => aiutp_channel_sup,
        start => {aiutp_channel_sup, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => supervisor,
        modules => [aiutp_channel_sup]
    },

    %% socket 启动时需要知道自己的监督者 pid 以获取 channel_sup
    Socket = #{
        id => aiutp_socket,
        start => {aiutp_socket, start_link, [Port, Options]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [aiutp_socket]
    },

    %% 注意启动顺序：channel_sup 在前，socket 在后
    {ok, {SupFlags, [ChannelSup, Socket]}}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% @doc 在监督者的子进程中查找指定 ID 的进程
-spec find_child(pid(), atom()) -> {ok, pid()} | {error, not_found}.
find_child(Sup, ChildId) ->
    Children = supervisor:which_children(Sup),
    case lists:keyfind(ChildId, 1, Children) of
        {ChildId, Pid, _Type, _Modules} when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            {error, not_found}
    end.
