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
-export([incoming/3,connect/3]).

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
  ai_utp_worker:connect(Worker, Address0, Port).
 
incoming(Socket,#packet{type = Type} = Packet,Remote)->
  case Type of
    st_reset -> ok;
    st_syn ->
      gen_server:cast(Socket,{syn,Packet,Remote});
    _ ->
      gen_server:cast(Socket,{reset,Packet,Remote})
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
  {ok,Socket} = gen_udp:open(Port,Options),
  {ok,Dispatch} = ai_utp_dispatch:start_link(Parent),
  ok = inet:setopts(Socket, [{active,once}]),
  {ok, #state{socket = Socket,dispatch = Dispatch}}.

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
handle_cast({reset,#packet{conn_id = ConnID,seq_no = SeqNo},Remote},
            #state{socket = Socket} = State)->
  reset(Socket, Remote, ConnID, SeqNo),
  {noreply,State};
handle_cast({syn,#packet{conn_id = ConnID,seq_no = SeqNo},Remote},
            #state{socket = Socket,acceptor = closed} = State)->
  reset(Socket,Remote,ConnID,SeqNo),
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
handle_info({udp, Socket, IP, InPortNo, Packet},
            #state{socket = Socket,dispatch = Dispatch} = State)->
  ai_utp_dispatch:dispatch(Dispatch,{IP,InPortNo}, Packet),
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
