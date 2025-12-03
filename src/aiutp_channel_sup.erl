%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2025, David Gao
%%% @doc uTP channel 进程的监督者
%%%
%%% 每个 aiutp_socket 拥有独立的 channel_sup 实例，
%%% 不再使用全局注册，实现完全的故障隔离。
%%%
%%% @end
%%% Created : 3 Dec 2025
%%%-------------------------------------------------------------------
-module(aiutp_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([new/3]).

%% 监督者回调
-export([init/1]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动 channel 监督者
%%
%% 注意：不使用全局注册，每个 socket 有自己的 channel_sup 实例
-spec start_link() ->
    {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @doc 启动一个新的 channel 进程
%%
%% @param ChannelSup channel 监督者 pid
%% @param Parent 父进程（aiutp_socket）pid
%% @param UDPSocket UDP 套接字
%% @returns {ok, ChannelPid} | {error, Reason}
-spec new(pid(), pid(), port()) -> {ok, pid()} | {error, term()}.
new(ChannelSup, Parent, UDPSocket) ->
    supervisor:start_child(ChannelSup, [Parent, UDPSocket]).

%%%===================================================================
%%% 监督者回调
%%%===================================================================

%% @private
%% @doc 初始化监督者
-spec init(term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
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
