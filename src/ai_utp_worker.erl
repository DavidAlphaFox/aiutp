%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(ai_utp_worker).
-behaviour(gen_server).

-include("ai_utp.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([connect/4,accept/5,incoming/3,
         send/2,recv/2]).

-define(SERVER, ?MODULE).
-define(SYN_TIMEOUT, 3000).
-define(SYN_TIMEOUT_THRESHOLD, ?SYN_TIMEOUT*2).

-record(state, {
                parent :: pid(),
                socket :: port(),
                parent_monitor :: reference(),
                controller :: pid(),
                controller_monitor :: reference(),
                remote :: tuple(),
                net,
                rto_timer :: reference(),
                connector,
                conn_id,
                process
               }).

%%%===================================================================
%%% API
%%%===================================================================
connect(Pid,Caller,Address,Port)->
  gen_server:call(Pid, {connect,Caller,{Address,Port}},infinity).
accept(Pid,Caller,Remote,Packet,Timing)->
  gen_server:call(Pid,{accept,Caller,Remote,Packet,Timing},infinity).
send(Pid,Data)->
  gen_server:call(Pid,{send,Data},infinity).
recv(Pid,Len)->
  gen_server:call(Pid,{recv,Len},infinity).

incoming(Pid,Packet,Timing)->
  gen_server:cast(Pid,{packet,Packet,Timing}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),port()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Parent,Socket) ->
  gen_server:start_link(?MODULE, [Parent,Socket], []).

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
init([Parent,Socket]) ->
  ParentMonitor = erlang:monitor(process,Parent),
  {ok, #state{
          parent = Parent,
          socket = Socket,
          parent_monitor = ParentMonitor,
          net = #utp_net{},
          process = ai_utp_process:new()
         }}.

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
handle_call({connect,Control,Remote},From,
            #state{net = Net,socket = Socket} = State)->
  case register_conn(Remote) of
    {ok,ConnID} ->
      {Net0,Packet} = ai_utp_net:connect(Net,ConnID),
      ai_utp_util:send(Socket, Remote, Packet, 0),
      {noreply,State#state{
                 remote = Remote,
                 controller = Control,
                 controller_monitor = erlang:monitor(process,Control),
                 connector = From,
                 net = Net0,
                 conn_id = ConnID,
                 rto_timer = start_rto_timer(?SYN_TIMEOUT, undefined)
                }};
    _ -> {reply,{error,eagain},0}
  end;
handle_call({accept,Control, Remote, Packet,Timing},From,
           #state{net = Net,socket = Socket} = State)->
  {Net0,SynPacket,ConnID,TSDiff} = ai_utp_net:accept(Net,Packet,Timing),
  case ai_utp_conn:alloc(Remote, ConnID) of
    ok ->
      ai_utp_util:send(Socket,Remote,SynPacket,TSDiff),
      {reply,ok,State#state{
                  remote = Remote,
                  net = Net0,controller = Control,
                  controller_monitor = erlang:monitor(process,Control),
                  conn_id = ConnID
                 }};
    Error ->
      gen_server:reply(From, Error),
      handle_info(timeout, State#state{net = Net0,
                                       controller = undefined})
  end;
handle_call({send,Data},From,
            #state{socket = Socket,remote = Remote,
                   process = Proc,net = Net} = State)->
  Proc0 = ai_utp_process:enqueue_sender(From, Data, Proc),
  {{Net0,Packets,TS,TSDiff},Proc1} = ai_utp_net:do_send(Net, Proc0),
  send(Socket,Remote,Packets,TS,TSDiff),
  {noreply,State#state{net = Net0,process = Proc1}}.




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
%% 已经链接了
handle_cast({packet,Packet,Timing},
            #state{net = Net,socket = Socket,
                   remote = Remote, connector = undefined,
                   rto_timer = Timer} = State)->
  cancle_rto_timer(Timer),
  io:format("~p~n",[Packet]),
  {Net0,Packets,TS,TSDiff} = ai_utp_net:process_incoming(Net,Packet, Timing),
  case ai_utp_net:state(Net0) of
    {'ERROR',_} -> handle_info(timeout, State#state{net = Net0});
    NetState ->
      Packets0 =
        case Packets of
          {P,_} -> P;
          R -> R
        end,
      send(Socket,Remote,Packets0,TS,TSDiff),
      io:format("Send: ~p~n",[Packets]),
      if NetState == ?CLOSED ->
          handle_info(timeout, State#state{net = Net0,rto_timer = undefined});
         true ->
          RTO = ai_utp_net:rto(Net0),
          RTOTimer =
            case Packets of
              {P0,ack} when erlang:length(P0) > 1 ->
                start_rto_timer(RTO, undefined);
              {_,ack} -> undefined;
              P0 when erlang:length(P0) > 0 ->
                start_rto_timer(RTO, undefined);
              _ -> undefined
            end,
          {noreply,State#state{net = Net0,rto_timer = RTOTimer}}
      end
  end;
%% 正在进行链接
handle_cast({packet,Packet,Timing},
            #state{net = Net,connector = Connector,
                   rto_timer = Timer} = State)->
  cancle_rto_timer(Timer),
  {Net0,_,_,_} = ai_utp_net:process_incoming(Net,Packet, Timing),
  case ai_utp_net:state(Net0) of
    {'ERROR',Reason} ->
      gen_server:reply(Connector, {error,Reason}),
      handle_info(timeout,State#state{connector = undefined,
                                      controller = undefined,
                                      rto_timer = undefined});
    ?ESTABLISHED ->
      gen_server:reply(Connector, ok),
      {noreply,State#state{net = Net0,connector = undefined,
                           rto_timer = undefined}};
    _ ->
      gen_server:reply(Connector, {error,econnaborted}),
      handle_info(timeout,State#state{connector = undefined,
                                      controller = undefined,
                                      rto_timer = undefined})
  end.

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
handle_info(timeout,#state{parent_monitor = ParentMonitor,
                           rto_timer = Timer,
                           conn_id = ConnID,
                           remote = Remote,
                           controller_monitor = CMonitor} =  State)->
  if ParentMonitor /= undefined ->
      erlang:demonitor(ParentMonitor,[flush]);
     true -> ok
  end,
  if CMonitor /= undefined ->
      erlang:demonitor(ParentMonitor,[flush]);
     true -> ok
  end,
  cancle_rto_timer(Timer),
  ai_utp_conn:free(Remote, ConnID),
  {stop,normal,State#state{parent_monitor = undefined,
                           controller_monitor = undefined}};
handle_info({timeout,TRef,{rto,N}},
            #state{net = Net,rto_timer = {set,TRef}} = State)->
  NetState = ai_utp_net:state(Net),
  rto_timer(NetState,N,State);

handle_info({'DOWN', MRef, process, Parent, _Reason},
            #state{parent = Parent,
                   parent_monitor = MRef,
                   controller_monitor = CMonitor,
                   connector = Connector
                  } = State)->
  if CMonitor == undefined -> State;
     true -> erlang:demonitor(CMonitor,[flush])
  end,
  State0 =
    if Connector == undefined -> State;
       true ->
        gen_server:reply(Connector, {error,eagain}),
        State#state{connector = undefined,controller = undefined}
    end,
  {noreply,State0#state{parent_monitor = undefined,
                        controller_monitor = undefined},0};
handle_info({'DOWN', MRef, process, Control, _Reason},
            #state{controller = Control,
                   controller_monitor = MRef
                  } = State)->
  {noreply,State#state{controller = undefined,
                       controller_monitor = undefined},0};
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
terminate(_Reason, #state{controller = undefined})-> ok;
terminate(_Reason, #state{controller = Control,
                          parent = Parent,net = Net}) ->
  Self = self(),
  UTPSocket = {utp,Parent,Self},
  case ai_utp_net:state(Net) of
    {'Error',Reason} -> Control ! {utp_error,UTPSocket,Reason};
    'CLOSED' -> Control ! {utp_close,UTPSocket};
    _ -> Control ! {utp_error,UTPSocket,econnaborted}
  end,
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
register_conn(Remote)-> register_conn(Remote,3).
register_conn(_,0)-> {error,exist};
register_conn(Remote,N)->
  ConnID = ai_utp_util:bit16_random(),
  case ai_utp_conn:alloc(Remote, ConnID) of
    ok -> {ok,ConnID};
    _ -> register_conn(Remote,N-1)
  end.


start_rto_timer(N, Timer) ->
  set_rto_timer(N, N, Timer).
set_rto_timer(N, K, undefined) ->
  Ref = erlang:start_timer(N, self(),{rto,K}),
  {set, Ref};
set_rto_timer(N, K, {set, Ref}) ->
  erlang:cancel_timer(Ref),
  NewRef = erlang:start_timer(N, self(),{rto,K}),
  {set, NewRef}.
cancle_rto_timer(undefined) -> undefined;
cancle_rto_timer({set,Ref}) ->
  erlang:cancel_timer(Ref),
  undefined.

rto_timer('SYN_SENT',N,
          #state{ socket = Socket,
                  remote = Remote,
                  conn_id = ConnID,
                  net = Net,
                  connector = Connector,
                  controller_monitor = CMonitor
            } = State)->
  if N > ?SYN_TIMEOUT_THRESHOLD ->
      erlang:demonitor(CMonitor,[flush]),
      gen_server:reply(Connector, {error,etimeout}),
      {noreply,State#state{
                 controller_monitor = undefined,
                 controller = undefined,
                 connector = undefined,
                 rto_timer = undefined
                },0};
     true ->
      {Net0,Packet} = ai_utp_net:connect(ConnID, Net),
      ok = ai_utp_util:send(Socket, Remote, Packet, 0),
      {noreply,State#state{
                 net = Net0,
                 rto_timer = start_rto_timer(N *2, undefined)
                }}
  end.


send(Socket,Remote,Packets,TS,TSDiff)->
  lists:foreach(
    fun(Packet)-> ai_utp_util:send(Socket, Remote,
                                   Packet, TS,TSDiff) end,
    Packets).
