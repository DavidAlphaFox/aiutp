%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(ai_utp_socket).

-behaviour(gen_server).
-include("ai_utp.hrl").
%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([incoming/4,connect/3,listen/2,accept/1]).

-define(SERVER, ?MODULE).

-record(state, {
                socket,
                dispatch,
                acceptor = closed
               }).

%%%===================================================================
%%% API
%%%===================================================================
connect(UTPSocket,Address,Port)->
  {ok,Socket} = gen_server:call(UTPSocket,socket),
  {ok,Worker} = ai_utp_worker_sup:new(UTPSocket, Socket),
  Address0 = ai_utp_util:getaddr(Address),
  Caller = self(),
  case ai_utp_worker:connect(Worker,Caller,Address0, Port) of
    ok -> {ok,{utp,UTPSocket,Worker}};
    Error -> Error
  end.

listen(UTPSocket,Options)-> gen_server:call(UTPSocket,{listen,Options}).
accept(UTPSocket)-> gen_server:call(UTPSocket,accept,infinity).
incoming(Socket,Remote,#utp_packet{type = Type} = Packet,Timing)->
  case Type of
    st_reset -> ok;
    st_syn ->
      gen_server:cast(Socket,{syn,Remote,Packet,Timing});
    _ ->
      gen_server:cast(Socket,{reset,Remote,Packet})
  end.
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
  Parent = self(),
  Options0 =
    case proplists:is_defined(binary,Options) of
      false -> [binary|Options];
      true -> Options
    end,
  case gen_udp:open(Port,Options0 ++ [{sndbuf,?OPT_SEND_BUF * 4},
                                      {recbuf,?OPT_RECV_BUF * 4}]) of
    {ok,Socket} ->
      {ok,Dispatch} = ai_utp_dispatch:start_link(Parent),
      ok = inet:setopts(Socket, [{active,once}]),
      {ok, #state{socket = Socket,dispatch = Dispatch}};
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
handle_call(socket,_From,#state{socket = Socket} = State)->
  {reply,{ok,Socket},State};
handle_call({listen,Options},_From,
            #state{socket = Socket,acceptor = closed} = State)->
  Parent = self(),
  {ok,Acceptor} = ai_utp_acceptor:start_link(Parent,Socket,Options),
  {reply,ok,State#state{acceptor = Acceptor}};
handle_call({listen,_},_From,State) ->
  {reply,{error,is_listen_socket},State};
handle_call(accept,_From,#state{acceptor = closed} = State)->
  {reply,{error,not_listen_socket},State};
handle_call(accept,From,#state{acceptor = Acceptor} = State)->
  ai_utp_acceptor:accept(Acceptor,From),
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
handle_cast({reset,Remote,#utp_packet{conn_id = ConnID,seq_no = SeqNo}},
            #state{socket = Socket} = State)->
  reset(Socket, Remote, ConnID, SeqNo),
  {noreply,State};

handle_cast({syn,Remote,#utp_packet{conn_id = ConnID,seq_no = SeqNo}},
            #state{socket = Socket,acceptor = closed} = State)->
  reset(Socket,Remote,ConnID,SeqNo),
  {noreply,State};
handle_cast({syn,_,_,_} = Req,#state{acceptor = Acceptor} = State)->
  ai_utp_acceptor:incoming(Acceptor,Req),
  {noreply,State};
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
handle_info({'EXIT',Dispatch,Reason},
            #state{dispatch = Dispatch} = State)->
  {stop,Reason,State};
handle_info({'EXIT',Acceptor,Reason},
            #state{acceptor = Acceptor} = State) ->
  {stop,Reason,State};
handle_info({udp, Socket, IP, InPortNo, Packet},
            #state{socket = Socket,dispatch = Dispatch} = State)->
  Now = ai_utp_util:microsecond(),
  ai_utp_dispatch:dispatch(Dispatch,{IP,InPortNo}, Packet,Now),
  ok = inet:setopts(Socket, [{active,once}]),
  {noreply,State};
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
terminate(_Reason,#state{socket = undefined})-> ok;
terminate(_Reason, #state{socket = Socket }) ->
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
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
reset(Socket,Remote,ConnID,AckNo)->
  Packet = ai_utp_protocol:make_reset_packet(ConnID, AckNo),
  ai_utp_util:send(Socket, Remote, Packet, 0).
