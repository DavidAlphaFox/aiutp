%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_worker).
-behaviour(gen_server).

-include("aiutp.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([connect/4,accept/5,incoming/2,
         send/2,recv/2,active/2,close/2,
         controlling_process/2]).

-define(SERVER, ?MODULE).

-record(state, {parent :: pid(),
                socket :: port(),
                parent_monitor :: reference(),
                controller :: pid(),
                controller_monitor :: reference(),
                remote :: tuple(),
                pcb,
                tick_timer :: reference(),
                connector,
                closer,
                conn_id,
                process,
                active = false
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
active(Pid,V)->
  gen_server:call(Pid,{active,V},infinity).
close(Pid,Caller)->
  gen_server:call(Pid,{close,Caller},infinity).

controlling_process(Pid,Proc)->
  Caller = self(),
  {ok,Control} = gen_server:call(Pid,controller),
  if Control == Proc -> ok;
     Control /= Caller -> {error,not_owner};
     true ->
      {ok,Active} = gen_server:call(Pid,active),
      if Active == true -> active(Pid,false);
         true -> ok
      end,
      Closed =  sync_input(Pid,Proc,false),
      if
        Closed == true -> ok;
        true -> gen_server:call(Pid,{controlling_process,Caller,Proc,Active},infinity)
      end
  end.

  
incoming(Pid,Packet)->
  gen_server:cast(Pid,{packet,Packet}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),port(),list()) -> {ok, Pid :: pid()} |
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
          parent_monitor = ParentMonitor}}.


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
            #state{net = Net} = State)->
  case register_conn(Remote) of
    {ok,ConnID} ->
      Net0 = ai_utp_net:connect(Net#utp_net{remote = Remote},ConnID),
      {noreply,State#state{
                 remote = Remote,
                 controller = Control,
                 controller_monitor = erlang:monitor(process,Control),
                 connector = From,
                 net = Net0,
                 conn_id = ConnID,
                 tick_timer = start_tick_timer(?TIMER_TIMEOUT, undefined)}};
    _ -> {reply,{error,eagain},0}
  end;
handle_call({accept,Control, Remote, Packet,Timing},From,
           #state{net = Net} = State)->
  {Net0,ConnID} = ai_utp_net:accept(Net#utp_net{remote = Remote},Packet,Timing),
  case ai_utp_conn:alloc(Remote, ConnID) of
    ok -> {reply,ok,State#state{
                      remote = Remote,
                      net = Net0,controller = Control,
                      controller_monitor = erlang:monitor(process,Control),
                      conn_id = ConnID,
                      tick_timer = start_tick_timer(?TIMER_TIMEOUT, undefined)
                     }};
    Error ->
      gen_server:reply(From, Error),
      handle_info(timeout, State#state{net = Net0,controller = undefined})
  end;
handle_call({controlling_process,OldControl,NewControl,Active},_From,
           #state{controller = OldControl,controller_monitor = CMonitor} = State)->
  erlang:demonitor(CMonitor,[flush]),
  CMonitor0 = erlang:monitor(process,NewControl),
  {reply,ok,active_read(State#state{controller = NewControl,
                                    active = Active,
                                    controller_monitor = CMonitor0})};
handle_call({send,Data},From,
            #state{process = Proc,net = Net} = State)->
  Proc0 = ai_utp_process:enqueue_sender(From, Data, Proc),
  {Net0,Proc1} = ai_utp_net:do_send(Net, Proc0),
  Proc2 =
    case ai_utp_net:state(Net0) of
      ?CLOSED ->
        Proc3 = ai_utp_process:error_all(Proc1, closed),
        timer:sleep(100),
        self() ! timeout,
        Proc3;
      _ -> Proc1
    end,
  {noreply,active_read(State#state{net = Net0,process = Proc2})};

handle_call({active,true},_From,State) ->
  {reply,ok,active_read(State#state{active = true})};
handle_call(controller,_From,#state{controller = Controller} = State)->
  {reply,{ok,Controller},State};
handle_call(active,_From,#state{active = Active}=State) ->
  {reply,{ok,Active},State};
handle_call({close,Controll},From,#state{controller = Controll,
                                         process = Proc,
                                         net = Net} = State) ->
  Net0 = ai_utp_net:close(Net,Proc),
  case ai_utp_net:state(Net0) of
    ?CLOSED ->
      gen_server:reply(From, ok),
      timer:sleep(100),
      self() ! timeout,
      {reply,ok,State#state{active = false,net = Net0,
                            process = ai_utp_process:new()}};
    _ ->
      {noreply,State#state{net = Net0,closer = From,active = false,
                           process = ai_utp_process:new()}}
  end;
handle_call({close,_},_From,State) ->
  {reply,{error,not_owner},State}.


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
handle_cast({packet,Packet},
            #state{pcb = PCB,connector = undefined} = State)->
  PCB0 = aiutp_pcb:process(Packet, PCB),
  {Buffers,PCB1} = aiutp_pcb:swap_socket(PCB0),

  case ai_utp_net:state(Net0) of
     ?CLOSED ->
      cancle_tick_timer(Timer),
      if Closer /= undefined ->
          self() ! timeout,
          gen_server:reply(Closer, ok);
         true -> ok
      end,
      {noreply,active_read(State#state{net = Net0,
                                       tick_timer = undefined,
                                       closer = undefined,
                                       process = Proc0})};
    _ ->
      {noreply,active_read(State#state{process = Proc0,net = Net0})}
  end;
%% 正在进行链接
handle_cast({packet,Packet,Timing},
            #state{net = Net,connector = Connector,
                   tick_timer = Timer,process = Proc} = State)->
  cancle_tick_timer(Timer),
  {Net0,Proc0} = ai_utp_net:process_incoming(Net,Packet, Timing,Proc),
  case ai_utp_net:state(Net0) of
    ?CLOSED ->
      gen_server:reply(Connector,
                       {error,ai_utp_net:net_error(Net0)}),
      handle_info(timeout,
                  State#state{connector = undefined,
                              controller = undefined,
                              tick_timer = undefined,
                              net = Net0,
                              process = Proc0});
    ?ESTABLISHED ->
      gen_server:reply(Connector, ok),
      {noreply,State#state{net = Net0,connector = undefined,process = Proc0,
                           tick_timer = start_tick_timer(?TIMER_TIMEOUT,undefined)}};
    _ ->
      gen_server:reply(Connector, {error,econnaborted}),
      handle_info(timeout,State#state{connector = undefined,
                                      controller = undefined,
                                      tick_timer = undefined,
                                      net = Net0,process = Proc0})
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
                           tick_timer = Timer,
                           conn_id = ConnID,
                           remote = Remote,
                           process = Proc,
                           net = Net,
                           controller_monitor = CMonitor} =  State)->
  if ParentMonitor /= undefined ->
      erlang:demonitor(ParentMonitor,[flush]);
     true -> ok
  end,
  if CMonitor /= undefined ->
      erlang:demonitor(ParentMonitor,[flush]);
     true -> ok
  end,
  cancle_tick_timer(Timer),
  Reason =
    case ai_utp_net:net_error(Net) of
      normal -> closed;
      R -> R
    end,
  Proc0 = ai_utp_process:error_all(Proc, Reason),
  ai_utp_conn:free(Remote, ConnID),
  {stop,normal,State#state{parent_monitor = undefined,
                           process = Proc0,
                           controller_monitor = undefined}};
handle_info({timeout,TRef,{check_interval,N}},
            #state{pcb = PCB,tick_timer = {set,TRef}}= State)->
  PCB1 = aiutp_pcb:check_timeouts(PCB),
  Buffers = aiutp_pcb:swap_socket(PCB1),


%% 端口崩溃了，就什么好说的了
handle_info({'DOWN', MRef, process, Parent, _Reason},
            #state{parent = Parent,
                   parent_monitor = MRef,
                   controller_monitor = CMonitor,
                   connector = Connector,
                   process = Proc,
                   net = Net
                  } = State)->
  State0 =
    if Connector == undefined ->
        State#state{net = ai_utp_net:close(Net),
                    process = ai_utp_process:error_all(Proc, econnaborted)};
       true ->
        gen_server:reply(Connector, {error,eagain}),
        if CMonitor == undefined -> State;
           true -> erlang:demonitor(CMonitor,[flush])
        end,
        State#state{connector = undefined,controller = undefined,
                    controller_monitor = undefined}
    end,
  handle_info(timeout,State0#state{parent = undefined,
                                  parent_monitor = undefined});
%% 控制进程崩溃了，那么快速失败吧
handle_info({'DOWN', MRef, process, Control, _Reason},
            #state{controller = Control,
                   controller_monitor = MRef,
                   net = Net,process = Proc
                  } = State)->
  case ai_utp_net:state(Net) of
    ?CLOSED -> ok;
    _ ->
      %%其它状态
      ai_utp_process:error_all(Proc, econnaborted),
      ai_utp_net:close(Net)
  end,
  handle_info(timeout,State#state{
                        controller = undefined,
                        process = ai_utp_process:new(),
                        controller_monitor = undefined
                       });

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
terminate(_Reason, #state{controller = Control,active = Active,
                          parent = Parent,net = Net}) ->
  if Active == true ->
      Self = self(),
      UTPSocket = {utp,Parent,Self},
      case ai_utp_net:state(Net) of
        ?CLOSED -> Control ! {utp_close, UTPSocket, ai_utp_net:net_error(Net)};
        _ -> Control ! {utp_close,UTPSocket,econnaborted}
      end;
     true -> ok
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


start_tick_timer(N, Timer) -> start_tick_timer(N, N, Timer).
start_tick_timer(N, K, undefined) ->
  if N == undefined -> undefined;
     true ->
      Ref = erlang:start_timer(N, self(),{check_interval,K}),
      {set, Ref}
  end;
start_tick_timer(N, K, {set, Ref}) ->
  erlang:cancel_timer(Ref),
  if N == undefined -> undefined;
     true ->
      NewRef = erlang:start_timer(N, self(),{check_interval,K}),
      {set, NewRef}
  end.
cancle_tick_timer(undefined) -> undefined;
cancle_tick_timer({set,Ref}) ->
  erlang:cancel_timer(Ref),
  undefined.

on_tick(NetState,_,#state{net = Net,
                          process = Proc,
                          closer = Closer,
                          connector = Connector,
                          controller_monitor = CMonitor
                         } = State) ->
  
  {Net0,Proc0} = ai_utp_net:on_tick(NetState,Net,Proc),
  NetState0 = ai_utp_net:state(Net0),
  State0 =
    if (NetState0 == ?CLOSED) and (Closer /= undefined)->
        self() ! timeout,
        gen_server:reply(Closer,ok),
        State#state{tick_timer = undefined};
       (NetState0 == ?CLOSED) and (Connector /= undefined)->
        erlang:demonitor(CMonitor,[flush]),
        self() ! timeout,
        gen_server:reply(Connector, {error,etimeout}),
        State#state{controller_monitor = undefined,
                    controller = undefined,
                    connector = undefined,
                    tick_timer = undefined};
       true -> State
    end,
  Timer =
    if NetState0 == ?CLOSED -> undefined;
       true -> start_tick_timer(?TIMER_TIMEOUT,undefined)
    end,
  Proc1 =
    if NetState0 == ?CLOSED->
        Error = ai_utp_net:net_error(Net0),
        ai_utp_process:error_all(Proc0, Error);
       true -> Proc0
    end,
  {noreply,
   active_read(State0#state{tick_timer = Timer,net = Net0,
                            process = Proc1})}.

active_read(#state{parent = Parent,
                   net = Net,
                   controller = Control,
                   process = Proc,
                   active = true} = State)->
  Self = self(),
  case ai_utp_net:do_read(Net) of
    {Net0,Buffer} ->
      Control ! {utp,{utp,Parent,Self},Buffer},
      State#state{net = Net0,active = false};
     _ ->
      Proc0 =
        case ai_utp_net:state(Net) of
          ?CLOSED ->
            Self ! timeout,
            ai_utp_process:error_all(Proc, ai_utp_net:net_error(Net));
          _ -> Proc
        end,
      State#state{process = Proc0}
  end;
active_read(State)-> State.



sync_input(Socket,NewOwner,Flag)->
  receive
    {utp,Socket,_} = M ->
      NewOwner ! M,
      sync_input(Socket,NewOwner,Flag);
    {utp_close,Socket,_} = M->
      NewOwner ! M,
      sync_input(Socket,NewOwner,true)
  after 0 ->
      Flag
  end.
