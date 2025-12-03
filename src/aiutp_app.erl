-module(aiutp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% @doc Start the aiutp application
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_Type, _Args) ->
	aiutp_sup:start_link().

%% @doc Stop the aiutp application
-spec stop(term()) -> ok.
stop(_State) ->
	ok.
