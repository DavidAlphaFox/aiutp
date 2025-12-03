-module(aiutp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% @doc 启动 aiutp 应用程序
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_Type, _Args) ->
	aiutp_sup:start_link().

%% @doc 停止 aiutp 应用程序
-spec stop(term()) -> ok.
stop(_State) ->
	ok.
