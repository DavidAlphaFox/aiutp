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

-export([connect/4,accept/4,incoming/2,
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
                blocker,
                conn_id,
                active = false
               }).

%%%===================================================================
%%% API
%%%===================================================================

connect(Pid,Caller,Address,Port)->
  gen_server:call(Pid, {connect,Caller,{Address,Port}},infinity).
accept(Pid,Caller,Remote,Packet)->
  gen_server:call(Pid,{accept,Caller,Remote,Packet},infinity).
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
            #state{parent = Parent,socket = Socket} = State) ->
  case add_conn(Parent,Remote) of
    {ok,ConnId} ->
      PCB = aiutp_pcb:connect({Socket,Remote},ConnId),
      {noreply,State#state{
                 remote = Remote,
                 controller = Control,
                 controller_monitor = erlang:monitor(process,Control),
                 blocker = From,
                 pcb = PCB,
                 conn_id = ConnId,
                 tick_timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL, undefined)}};
    Error -> {stop,normal,Error,State}
  end;
handle_call({accept,Control, Remote, Packet},_From,
            #state{parent = Parent,socket = Socket} = State) ->

  {ConnId,PCB} = aiutp_pcb:accept({Socket,Remote},Packet),
  case aiutp_socket:add_conn(Parent,Remote,ConnId) of
    ok ->
      {reply,ok,State#state{pcb = PCB,
                             remote = Remote,
                             controller = Control,
                             controller_monitor = erlang:monitor(process,Control),
                             tick_timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL, undefined),
                             conn_id = ConnId
                            }};
    Error ->
      {stop,normal,Error,State}
  end;
handle_call({controlling_process,OldControl,NewControl,Active},_From,
           #state{controller = OldControl,controller_monitor = CMonitor} = State)->
  erlang:demonitor(CMonitor,[flush]),
  CMonitor0 = erlang:monitor(process,NewControl),
  {reply,ok,active_read(State#state{controller = NewControl,
                                    active = Active,
                                    controller_monitor = CMonitor0})};
handle_call({send,Data},_From,#state{pcb = PCB} = State) ->
  case aiutp_pcb:write(Data, PCB) of
    {Error,PCB1} -> {reply,Error,State#state{pcb = PCB1}};
    PCB1 -> {reply,ok,State#state{pcb = PCB1}}
  end;

handle_call({active,Active},_From,State) ->
  {reply,ok,active_read(State#state{active = Active})};
handle_call(controller,_From,#state{controller = Controller} = State)->
  {reply,{ok,Controller},State};
handle_call(active,_From,#state{active = Active}=State) ->
  {reply,{ok,Active},State};
handle_call({close,Controll},_From,#state{controller = Controll,
                                          controller_monitor = CMonitor,
                                          pcb = PCB} = State) ->
  if CMonitor /= undefiend -> erlang:demonitor(CMonitor,[flush]);
     true -> ok
  end,
  PCB1 = aiutp_pcb:close(PCB),
  {reply,ok,State#state{
              pcb = PCB1,
              controller = undefined,
              controller_monitor = undefined
             }};

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
            #state{pcb = PCB,blocker = undefined} = State)->
  PCB0 = aiutp_pcb:process(Packet, PCB),
  {noreply,active_read(State#state{pcb = PCB0})};

%% 正在进行链接
handle_cast({packet,Packet},
            #state{pcb = PCB,blocker = Connector} = State)->
  PCB0 = aiutp_pcb:process(Packet, PCB),
  case aiutp_pcb:state(PCB0) of
    ?CS_CONNECTED ->
      gen_server:reply(Connector, ok),
      {noreply,State#state{blocker = undefined,pcb =PCB0}};
    _ -> {noreply,State#state{pcb = PCB0}}
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


handle_info({stop,Reason},
            #state{parent = Parent,parent_monitor = ParentMonitor,
                   controller = Controller,controller_monitor = ControlMonitor,
                   blocker = Blocker,tick_timer = Timer,
                   conn_id = ConnId,remote = Remote,
                   active = Active})->
  if ParentMonitor /= undefined -> erlang:demonitor(ParentMonitor,[flush]);
     true -> ok
  end,
  if ControlMonitor /= undefined -> erlang:demonitor(ControlMonitor,[flush]);
     true -> ok
  end,
  cancle_tick_timer(Timer),
  %% 如果是connector存在，直接报错
  if Blocker /= undefined -> gen_server:reply(Blocker, {error,Reason});
     (Active == true) and
     (Controller /= undefined) ->  Controller ! {utp_closed,{utp,Parent,self()},Reason};
     true -> ok
  end,
  if ConnId /= undefined -> aiutp_socket:free_conn(Parent,Remote,ConnId);
     true -> ok
  end,
  {stop,normal,undefined};

handle_info({timeout,TRef,{check_interval,_}},
            #state{pcb = PCB,tick_timer = {set,TRef}}= State)->
  PCB2 = aiutp_pcb:check_timeouts(PCB),
  OutQue = PCB2#aiutp_pcb.outque,
  io:format("RTT: ~p RTTVar: ~p RTO: ~p MaxWindow: ~p CurWindowPackets: ~p CurWindow: ~p  OutQueSize: ~p ~n",
            [PCB2#aiutp_pcb.rtt, PCB2#aiutp_pcb.rtt_var,PCB2#aiutp_pcb.rto,
             PCB2#aiutp_pcb.max_window,PCB2#aiutp_pcb.cur_window_packets,PCB2#aiutp_pcb.cur_window,
            aiutp_queue:size(OutQue)]),
  %% 检查是否退出
  case aiutp_pcb:closed(PCB2) of
    {closed,Reason} ->
      self() ! {stop,Reason},
      {noreply,State#state{tick_timer = undefined,pcb = PCB2}};
    not_closed-> {noreply,State#state{
                            tick_timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL, undefined),
                            pcb = PCB2
                           }}
  end;
%% 端口崩溃了，就没什么好说的了
handle_info({'DOWN', MRef, process, Parent, _Reason},
            #state{parent = Parent,
                   parent_monitor = MRef,
                   controller = Controller,
                   controller_monitor = CMonitor,
                   blocker = Blocker,
                   active = Active,
                   tick_timer = Timer
                  })->
  if CMonitor /= undefined -> erlang:demonitor(CMonitor,[flush]);
     true -> ok
  end,
  cancle_tick_timer(Timer),
  if Blocker /= undefined ->
      gen_server:reply(Blocker, {error,crash});
     (Controller /= undefined) and
     (Active == true) ->
      Controller ! {utp_closed,{utp,Parent,self()},crash};
     true -> ok
  end,
  {stop,crash,undefiend};

%% 控制进程崩溃了，那么快速失败吧
handle_info({'DOWN', MRef, process, Control, _Reason},
            #state{controller = Control,
                   remote = Remote,
                   controller_monitor = MRef,
                   pcb = PCB,
                   tick_timer = Timer,
                   conn_id = ConnId
                  } = State)->
  PCB0 = aiutp_pcb:close(PCB),
  if Timer == undefined ->
      %% 此处不应当发生
      %% contrller被monitor的时候，timer也同步建立了
      if ConnId /= undefined -> aiutp_socket:remove_conn(Remote,ConnId);
         true -> ok
      end,
      {stop,normal,undefined};
     true ->
      {noreply,State#state{controller = undefined,
                                     controller_monitor = undefined,
                                     blocker = undefined,pcb = PCB0}}
  end;
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
terminate(_Reason, _State)-> ok.

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

add_conn(Parent,Remote) -> add_conn(Parent,Remote,3).
add_conn(_,_,0)-> {error,eagain};
add_conn(Parent,Remote,N)->
  ConnID = aiutp_util:bit16_random(),
  case aiutp_socket:add_conn(Parent,Remote, ConnID) of
    ok -> {ok,ConnID};
    _ -> add_conn(Parent,Remote,N-1)
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


active_read(#state{parent = Parent,
                   pcb = PCB,
                   controller = Control,
                   active = true} = State)->
  {Active,PCB2} =
    case aiutp_pcb:read(PCB) of
      {undefined,PCB1} -> {true,PCB1};
      {Payload,PCB1} ->
        UTPSocket = {utp,Parent,self()},
        Control ! {utp_data,UTPSocket,Payload},
        {false,PCB1}
    end,
  PCB3 = aiutp_pcb:flush(PCB2),
  State#state{ active = Active,pcb = PCB3};
active_read(#state{pcb = PCB} = State)->
  PCB1 = aiutp_pcb:flush(PCB),
  State#state{pcb = PCB1}.



sync_input(Socket,NewOwner,Flag)->
  receive
    {utp_data,Socket,_} = M ->
      NewOwner ! M,
      sync_input(Socket,NewOwner,Flag);
    {utp_closed,Socket,_} = M->
      NewOwner ! M,
      sync_input(Socket,NewOwner,true)
  after 0 ->
      Flag
  end.
