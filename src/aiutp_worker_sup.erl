%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([new/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
new(Socket)-> supervisor:start_child(?SERVER, [Socket]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
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
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
        {ok, {SupFlags :: supervisor:sup_flags(),
              [ChildSpec :: supervisor:child_spec()]}} |
        ignore.
init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 1,
               period => 5},
  Worker = #{id => aiutp_worker,
             start => {aiutp_worker,start_link,[]},
             restart => temporary,
             shutdown => 5000,
             type => worker,
             modules => [aiutp_worker]
            },
  {ok, {SupFlags, [Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
