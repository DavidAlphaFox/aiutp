%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_socket).

-behaviour(gen_server).
-include("aiutp.hrl").
%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).
-export([connect/3,listen/2,accept/1,
         add_conn/3,free_conn/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_CONNS, 100).

%% State is a map with the following keys:
%% - socket: gen_udp:socket() | undefined
%% - conns: #{conn_key() => {pid(), reference()}}
%% - monitors: #{reference() => conn_key()}
%% - max_conns: pos_integer()
%% - acceptor: pid() | closed
%% - options: list()
-type conn_key() :: {{inet:ip_address(), inet:port_number()}, non_neg_integer()}.
-type state() :: #{
    socket := gen_udp:socket() | undefined,
    conns := #{conn_key() => {pid(), reference()}},
    monitors := #{reference() => conn_key()},
    max_conns := pos_integer(),
    acceptor := pid() | closed,
    options := list()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Connect to a remote uTP endpoint
-spec connect(pid(), inet:ip_address() | string(), inet:port_number()) ->
    {ok, {utp, pid(), pid()}} | {error, term()}.
connect(UTPSocket,Address,Port)->
  {ok,{Socket,_}} = gen_server:call(UTPSocket,socket),
  {ok,Channel} = aiutp_channel_sup:new(UTPSocket, Socket),
  Address0 = aiutp_util:getaddr(Address),
  Caller = self(),
  case aiutp_channel:connect(Channel,Caller,Address0, Port) of
    ok -> {ok,{utp,UTPSocket,Channel}};
    Error -> Error
  end.

%% @doc Start listening for incoming connections
-spec listen(pid(), list()) -> ok | {error, term()}.
listen(UTPSocket,Options)-> gen_server:call(UTPSocket,{listen,Options}).

%% @doc Accept an incoming connection
-spec accept(pid()) -> {ok, {utp, pid(), pid()}} | {error, term()}.
accept(UTPSocket)-> gen_server:call(UTPSocket,accept,infinity).

%% @doc Register a connection ID for a worker process
-spec add_conn(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) ->
    ok | exists | overflow.
add_conn(UTPSocket,Remote,ConnId) ->
  Worker = self(),
  gen_server:call(UTPSocket,{add_conn,Remote,ConnId,Worker},infinity).

%% @doc Free a connection ID
-spec free_conn(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) -> ok.
free_conn(UTPSocket,Remote,ConnId) -> gen_server:call(UTPSocket,{free_conn,Remote,ConnId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(integer(),list()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Port,Options) ->
  gen_server:start_link(?MODULE, [Port,Options], []).

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
init([Port,Options]) ->
  process_flag(trap_exit, true),
  UDPOptions = proplists:get_value(udp, Options,[]),
  UDPOptions0 =
    case proplists:is_defined(binary,UDPOptions) of
      false -> [binary|UDPOptions];
      true -> UDPOptions
    end,
  UTPOptions = proplists:get_value(utp, Options,[]),
  case gen_udp:open(Port,[{sndbuf,6553600},
                          {recbuf,6553500} | UDPOptions0]) of
    {ok,Socket} ->
      ok = inet:setopts(Socket, [{active,once},
                                 {high_msgq_watermark,6553500}]),
      {ok, #{socket => Socket,
             conns => #{},
             monitors => #{},
             max_conns => ?DEFAULT_MAX_CONNS,
             acceptor => closed,
             options => UTPOptions}};
    {error,Reason} -> {stop,Reason}
  end.

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
handle_call({add_conn,Remote,ConnId,Worker},_From,State)->
  {Result,State0} = add_conn_inner(Remote,ConnId,Worker,State),
  {reply,Result,State0};
handle_call({free_conn,Remote,ConnId},_From,State) ->
  {reply,ok,free_conn_inner(Remote,ConnId,State)};

handle_call(socket,_From,#{socket := Socket, options := Options} = State)->
  {reply,{ok,{Socket,Options}},State};
handle_call({listen,Options},_From,
            #{socket := Socket, acceptor := closed,
              options := UTPOptions} = State)->
  Parent = self(),
  {ok,Acceptor} = aiutp_acceptor:start_link(Parent,Socket,Options,UTPOptions),
  {reply,ok,State#{acceptor := Acceptor}};
handle_call({listen,_},_From,State) ->
  {reply,{error,listening},State};
handle_call(accept,_From,#{acceptor := closed} = State)->
  {reply,{error,not_listening},State};
handle_call(accept,From,#{acceptor := Acceptor} = State)->
  aiutp_acceptor:accept(Acceptor,From),
  {noreply,State};
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

handle_info({udp, Socket, IP, Port, Payload},
            #{socket := Socket} = State)->
  case aiutp_packet:decode(Payload) of
    {ok,Packet} -> dispatch({IP,Port},Packet, State);
    {error, Reason} ->
      %% BEP-29: Log decode errors for debugging, but don't crash
      %% This can happen with malformed packets or protocol version mismatch
      logger:debug("uTP packet decode failed from ~p:~p, reason: ~p, size: ~p",
                   [IP, Port, Reason, byte_size(Payload)])
  end,
  ok = inet:setopts(Socket, [{active,once}]),
  {noreply,State};
handle_info({'DOWN',MRef,process,_Worker,_Reason},
            #{monitors := Monitors, conns := Conns} = State)->
  case maps:get(MRef,Monitors,undefined) of
    undefined -> {noreply,State};
    Key -> {noreply,
            State#{monitors := maps:remove(MRef, Monitors),
                   conns := maps:remove(Key,Conns)}}
  end;
handle_info({'EXIT',Acceptor,Reason},
            #{acceptor := Acceptor, socket := Socket} = State) ->
  if Socket /= undefined -> gen_udp:close(Socket);
     true -> ok
  end,
  {stop,Reason,State#{socket := undefined}};
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
terminate(_Reason, #{socket := undefined})-> ok;
terminate(_Reason, #{socket := Socket}) ->
  gen_udp:close(Socket),
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
-spec format_status(Status :: map()) -> Status :: map().
format_status(Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Send a reset packet to the remote peer
-spec reset_conn(gen_udp:socket(), {inet:ip_address(), inet:port_number()},
                 non_neg_integer(), non_neg_integer()) -> ok | {error, term()}.
reset_conn(Socket,Remote,ConnID,AckNR)->
  Packet = aiutp_packet:reset(ConnID, AckNR),
  Bin = aiutp_packet:encode(Packet),
  gen_udp:send(Socket,Remote,Bin).

%% @private Add a connection to the internal state
-spec add_conn_inner({inet:ip_address(), inet:port_number()}, non_neg_integer(),
                     pid(), state()) -> {ok | exists | overflow, state()}.
add_conn_inner(Remote, ConnId, Worker,
               #{conns := Conns, monitors := Monitors, max_conns := MaxConns} = State)->
  Key = {Remote, ConnId},
  case maps:is_key(Key, Conns) of
    true -> {exists, State};
    false ->
      ConnsSize = maps:size(Conns),
      if ConnsSize > MaxConns -> {overflow, State};
         true ->
          Monitor = erlang:monitor(process, Worker),
          {ok, State#{conns := maps:put(Key, {Worker, Monitor}, Conns),
                      monitors := maps:put(Monitor, Key, Monitors)}}
      end
  end.

%% @private Free a connection from the internal state
-spec free_conn_inner({inet:ip_address(), inet:port_number()}, non_neg_integer(),
                      state()) -> state().
free_conn_inner(Remote, ConnId, #{conns := Conns, monitors := Monitors} = State)->
  Key = {Remote, ConnId},
  case maps:is_key(Key, Conns) of
    false -> State;
    true ->
      {_, Monitor} = maps:get(Key, Conns),
      erlang:demonitor(Monitor, [flush]),
      State#{monitors := maps:remove(Monitor, Monitors),
             conns := maps:remove(Key, Conns)}
  end.

%% @private Dispatch incoming packet to appropriate handler
-spec dispatch({inet:ip_address(), inet:port_number()}, #aiutp_packet{}, state()) -> ok.
dispatch(Remote, #aiutp_packet{conn_id = ConnId, type = PktType, seq_nr = AckNR} = Packet,
         #{socket := Socket, conns := Conns, acceptor := Acceptor, max_conns := MaxConns})->
  Key = {Remote, ConnId},
  RecvTime = aiutp_util:microsecond(),
  case maps:get(Key, Conns, undefined) of
    undefined ->
      if (PktType == ?ST_SYN) and
         (Acceptor /= closed) ->
          ConnsSize = maps:size(Conns),
          if ConnsSize >= MaxConns -> reset_conn(Socket, Remote, ConnId, AckNR);
             true -> aiutp_acceptor:incoming(Acceptor, {?ST_SYN, Remote, {Packet, RecvTime}})
          end;
         (PktType == ?ST_RESET) -> ok;
         true -> reset_conn(Socket, Remote, ConnId, AckNR)
      end;
    {Channel, _} -> aiutp_channel:incoming(Channel, {Packet, RecvTime})
  end.
