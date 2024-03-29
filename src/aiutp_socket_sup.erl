%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([open/1,open/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
open(Port)-> open(Port,[]).
open(Port,Options)->
  case supervisor:start_child(?SERVER, [Port,Options]) of
    {ok,UTPSocket} -> {ok,{utp,UTPSocket}};
    Error -> Error
  end.

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
  Socket = #{id => aiutp_socket,
             start => {aiutp_socket,start_link,[]},
             restart => temporary,
             shutdown => 5000,
             type => worker,
             modules => [aiutp_socket]
            },
  {ok, {SupFlags, [Socket]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
