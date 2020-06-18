%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(ai_utp_conn).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([alloc/2,free/2,lookup/2]).

-define(SERVER, ?MODULE).
-define(TAB,?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%%% C: ConnectionID = 1, ReceiverID = ConnectionID, SenderID = ConnectionID + 1
%%% S: ConnectionID = 2, ReceiverID = ConnectionID, SenderID = ConnectionID - 1
-spec alloc({inet:ip_address() ,inet:port_number()},integer())->
        ok | {error,exist}.
alloc(Remote,ConnectionID)->
  Socket = self(),
  gen_server:call(?SERVER,{alloc,Socket,Remote,ConnectionID}).


-spec free({inet:ip_address() ,inet:port_number()},integer())-> ok.
free(Remote,ConnectionID)->
  Socket = self(),
  gen_server:call(?SERVER,{free,Socket,Remote,ConnectionID}).

-spec lookup({inet:ip_address(),inet:port_number()},integer())->
        {ok,pid()} | {error,not_exist}.
lookup(Remote,ConnectionID)->
  Conn = conn(ConnectionID,Remote),
  case ets:lookup(?TAB, Conn) of
    [] -> {error,not_exist};
    [{Conn,{socket,Socket}}] -> {ok,Socket}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
        {ok, State :: term(), Timeout :: timeout()} |
        {ok, State :: term(), hibernate} |
        {stop, Reason :: term()} |
        ignore.
init([]) ->
  ?TAB = ets:new(?TAB, [ordered_set, protected, named_table]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
        {reply, Reply :: term(), NewState :: term()} |
        {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
        {reply, Reply :: term(), NewState :: term(), hibernate} |
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
        {stop, Reason :: term(), NewState :: term()}.

handle_call({free,Socket,Remote,ConnectionID},_From,State)->
  Reply = free(Socket,Remote,ConnectionID),
  {reply,Reply,State};
handle_call({alloc,Socket,Remote,ConnectionID},_From,State)->
  Reply = alloc(Socket,Remote,ConnectionID),
  {reply, Reply, State}.
  
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, S) ->
  close(MonitorRef),
  {noreply, S};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
        {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
alloc(Socket,Remote,ConnectionID)->
  Conn = conn(ConnectionID,Remote),
  case ets:member(?TAB,Conn) of
    true -> {error,exist};
    false ->
      try
        true = ets:insert(?TAB, {Conn,{socket,Socket}}),
        Ref = erlang:monitor(process, Socket),
        RefSocket = {ref, Socket},
        true = ets:insert(?TAB, {RefSocket, Ref}),
        true = ets:insert(?TAB, {{ref, Ref}, Socket}),
        ok
      catch
        _:Reason -> {error,Reason}
      end
  end.

free(Socket,Remote,ConnectionID)->
  try
    Conn = conn(ConnectionID,Remote),
    true = ets:delete(?TAB,Conn),
    RefSocket = {ref, Socket},
    [{RefSocket,Ref}] = ets:lookup(?TAB, RefSocket),
    erlang:demonitor(Ref, [flush]),
    true = ets:delete(?TAB, {ref, Ref}),
    true = ets:delete(?TAB, RefSocket),
    ok
  catch _:_ ->
      ok
  end.
  
close(Ref) ->
  [{{ref, Ref}, Socket}] = ets:lookup(?TAB, {ref, Ref}),
  ets:delete(?TAB,{ref,Ref}),
  ets:delete(?TAB,{ref,Socket}),
  case [Conn || [Conn] <-  ets:match(?TAB, {'$1', {socket,Socket}})] of
    [ConnKey] ->
      ets:delete(?TAB, ConnKey);
     _-> ok
  end.



conn(ConnectionID,Remote)-> {conn,ConnectionID,Remote}.
