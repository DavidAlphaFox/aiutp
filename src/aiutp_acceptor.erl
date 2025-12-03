%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_acceptor).

-behaviour(gen_server).

-include("aiutp.hrl").
%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([accept/2,incoming/2]).

-define(SERVER, ?MODULE).
-define(BACKLOG, 128).
-record(state, {
                parent,
                socket,
                acceptors,
                monitors,
                syns,
                syn_len,
                max_syn_len,
                options
               }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Register an acceptor waiting for incoming connections
-spec accept(pid(), {pid(), term()}) -> ok.
accept(Pid,Acceptor)->
  gen_server:cast(Pid,{accept,Acceptor}).

%% @doc Handle incoming SYN request
-spec incoming(pid(), term()) -> ok.
incoming(Pid,Req)->
  gen_server:cast(Pid,Req).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),port(),list(),list()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Parent,Socket,Options,UTPOptions) ->
  gen_server:start_link(?MODULE, [Parent,Socket,Options,UTPOptions], []).

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
init([Parent,Socket,Options,UTPOptions]) ->
  Backlog =
    case Options of
      undefined -> ?BACKLOG;
      _ ->proplists:get_value(backlog, Options,?BACKLOG)
    end,
  {ok, #state{
          parent = Parent,
          socket = Socket,
          acceptors = queue:new(),
          syns = queue:new(),
          syn_len = 0,
          max_syn_len = Backlog,
          options = UTPOptions}}.

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
handle_call(_Request, _From, State) ->
  Reply = ok,
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
handle_cast({accept,Acceptor},
            #state{acceptors = Acceptors, syn_len = SynLen} = State)->
  if
    SynLen > 0 -> accept_incoming(Acceptor,State);
    true -> {noreply,State#state{ acceptors = queue:in(Acceptor, Acceptors) }}
  end;
handle_cast({?ST_SYN,Remote,Packet},State)->
  pair_incoming(Remote,Packet,State);
handle_cast(_, State) ->
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
handle_info(timeout,#state{acceptors = Acceptors} = State)->
  {{value,Acceptor},Acceptors0} = queue:out(Acceptors),
  accept_incoming(Acceptor, State#state{acceptors = Acceptors0});
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

%% @private Accept an incoming connection from pending SYN requests
-spec accept_incoming({pid(), term()}, #state{}) ->
    {noreply, #state{}} | {noreply, #state{}, timeout()}.
accept_incoming(Acceptor,#state{acceptors = Acceptors, syn_len = 0 } = State)->
  {noreply,State#state{acceptors = queue:in(Acceptor,Acceptors) }};
accept_incoming({Caller,_} = Acceptor,
                #state{ syns = Syns,syn_len = SynLen,
                       parent = Parent,socket = Socket} = State)->
  {ok,Channel} = aiutp_channel_sup:new(Parent,Socket),
  {{value,Req},Syns0} = queue:out(Syns),
  {Remote,{SYN,_} = P} = Req,
  case aiutp_channel:accept(Channel, Caller,Remote, P) of
    ok ->
      gen_server:reply(Acceptor, {ok,{utp,Parent,Channel}}),
      {noreply,State#state{syns = Syns0, syn_len = SynLen - 1}};
    _ ->
      Packet = aiutp_packet:reset(SYN#aiutp_packet.conn_id,SYN#aiutp_packet.seq_nr),
      Bin = aiutp_packet:encode(Packet),
      gen_udp:send(Socket,Remote,Bin),
      accept_incoming(Acceptor,State#state{syns = Syns0,syn_len = SynLen -1 })
  end.

%% @private Pair incoming SYN with pending acceptor or queue it
-spec pair_incoming({inet:ip_address(), inet:port_number()},
                    {#aiutp_packet{}, integer()}, #state{}) ->
    {noreply, #state{}} | {noreply, #state{}, timeout()}.
pair_incoming(Remote,{SYN,_},
              #state{socket = Socket,
                     syn_len = SynLen,
                     max_syn_len = MaxSynLen} = State) when SynLen >= MaxSynLen ->
  Packet = aiutp_packet:reset(SYN#aiutp_packet.conn_id,SYN#aiutp_packet.seq_nr),
  Bin = aiutp_packet:encode(Packet),
  gen_udp:send(Socket,Remote,Bin),
  {noreply,State};

pair_incoming(Remote,{Packet,_} = P,
              #state{acceptors = Acceptors,syns = Syns} = State) ->
  Syns0 =
    queue:filter(
      fun({Remote0,{SYN,_}})->
          if (Remote ==  Remote0) andalso
             (SYN#aiutp_packet.conn_id == Packet#aiutp_packet.conn_id) -> false;
            true -> true
          end
      end, Syns),
  Syns1 = queue:in({Remote,P},Syns0),
  Empty = queue:is_empty(Acceptors),
  if
    Empty == true -> {noreply,State#state{syns = Syns1,syn_len = queue:len(Syns1)}};
    true ->
      {{value,Acceptor},Acceptors0} = queue:out(Acceptors),
      accept_incoming(Acceptor,State#state{syns = Syns1,
                                           syn_len = queue:len(Syns1),
                                           acceptors = Acceptors0})
  end.
