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
         terminate/2, code_change/3, format_status/2]).
-export([connect/3,listen/2,accept/1,
         add_conn/3,free_conn/3]).

-define(SERVER, ?MODULE).

-record(state, {socket,
                conns,
                monitors,
                max_conns = 100, %% 可以增加接口后期修改
                acceptor = closed,
                options}).

%%%===================================================================
%%% API
%%%===================================================================
connect(UTPSocket,Address,Port)->
  {ok,{Socket,_}} = gen_server:call(UTPSocket,socket),
  {ok,Worker} = aiutp_worker_sup:new(UTPSocket, Socket),
  Address0 = aiutp_util:getaddr(Address),
  Caller = self(),
  case aiutp_worker:connect(Worker,Caller,Address0, Port) of
    ok -> {ok,{utp,UTPSocket,Worker}};
    Error -> Error
  end.

listen(UTPSocket,Options)-> gen_server:call(UTPSocket,{listen,Options}).
accept(UTPSocket)-> gen_server:call(UTPSocket,accept,infinity).
add_conn(UTPSocket,Remote,ConnId) ->
  Worker = self(),
  gen_server:call(UTPSocket,{add_conn,Remote,ConnId,Worker},infinity).
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
      {ok, #state{socket = Socket,
                  conns = maps:new(),
                  monitors = maps:new(),
                  options = UTPOptions}};
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

handle_call(socket,_From,#state{socket = Socket,options = Options} = State)->
  {reply,{ok,{Socket,Options}},State};
handle_call({listen,Options},_From,
            #state{socket = Socket,acceptor = closed,
                   options = UTPOptions} = State)->
  Parent = self(),
  {ok,Acceptor} = aiutp_acceptor:start_link(Parent,Socket,Options,UTPOptions),
  {reply,ok,State#state{acceptor = Acceptor}};
handle_call({listen,_},_From,State) ->
  {reply,{error,listening},State};
handle_call(accept,_From,#state{acceptor = closed} = State)->
  {reply,{error,not_listening},State};
handle_call(accept,From,#state{acceptor = Acceptor} = State)->
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
            #state{socket = Socket} = State)->
  case aiutp_packet:decode(Payload) of
    {ok,Packet} -> dispatch({IP,Port},Packet, State);
    _ -> ok
    %{error,_Reason}->
      %error_logger:info_report([decode_error,Reason]),
     % ok
  end,
  ok = inet:setopts(Socket, [{active,once}]),
  {noreply,State};
handle_info({'DOWN',MRef,process,_Worker,_Reason},
            #state{monitors = Monitors,conns = Conns} = State)->
  case maps:get(MRef,Monitors,undefined) of
    undefined -> {noreply,State};
    Key -> {noreply,
            State#state{monitors = maps:remove(MRef, Monitors),
                        conns = maps:remove(Key,Conns)}}
  end;
handle_info({'EXIT',Acceptor,Reason},
            #state{acceptor = Acceptor,socket = Socket} = State) ->
  if Socket /= undefined -> gen_udp:close(Socket);
     true -> ok
  end,
  {stop,Reason,State#state{socket = undefined}};
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
reset_conn(Socket,Remote,ConnID,AckNR)->
  Packet = aiutp_packet:reset(ConnID, AckNR),
  Bin = aiutp_packet:encode(Packet),
  gen_udp:send(Socket,Remote,Bin).

add_conn_inner(Remote,ConnId,Worker,
         #state{conns = Conns,monitors = Monitors,max_conns = MaxConns} = State)->
  Key = {Remote,ConnId},
  case maps:is_key(Key, Conns) of
    true -> {exists,State};
    false ->
      ConnsSize = maps:size(Conns),
      if ConnsSize > MaxConns -> {overflow,State};
         true ->
          Monitor = erlang:monitor(process, Worker),
          Conns0 = maps:put(Key, {Worker,Monitor}, Conns),
          Monitors0 = maps:put(Monitor,Key,Monitors),
          {ok,State#state{conns = Conns0,monitors = Monitors0}}
      end
  end.
free_conn_inner(Remote,ConnId,#state{conns = Conns,monitors = Monitors} = State)->
  Key = {Remote,ConnId},
  case maps:is_key(Key,Conns) of
    false -> State;
    true ->
      {_,Monitor} = maps:get(Key,Conns),
      erlang:demonitor(Monitor,[flush]),
      State#state{
        monitors = maps:remove(Monitor, Monitors),
        conns = maps:remove(Key,Conns)}
  end.

dispatch(Remote,#aiutp_packet{conn_id = ConnId,type = PktType,seq_nr = AckNR}= Packet,
         #state{socket = Socket,conns = Conns,acceptor = Acceptor,max_conns = MaxConns})->
  Key = {Remote,ConnId},
  RecvTime = aiutp_util:microsecond(),
  case maps:get(Key,Conns,undefined) of
    undefined ->
      if (PktType == ?ST_SYN) and
         (Acceptor /= closed) ->
          ConnsSize = maps:size(Conns),
          if ConnsSize >= MaxConns -> reset_conn(Socket, Remote, ConnId, AckNR);
             true -> aiutp_acceptor:incoming(Acceptor, {?ST_SYN,Remote,{Packet,RecvTime}})
          end;
         (PktType == ?ST_RESET) -> ok;
         true -> reset_conn(Socket, Remote, ConnId, AckNR)
      end;
    {Worker,_}-> aiutp_worker:incoming(Worker, {Packet,RecvTime})
  end.

%% {ip,port}
%% #{
%%   {ip,port} => handler_process
%% }
%%  socket                session                            handler               pcb      
%%   |  ---查询session-->    |       
%%   |  <---返回handler--    |
%%   | -----------------------传递封包数据------------------->  |
%%   |                      | <-----注册session，删除session----|
%%                                                             | -----创建 --------> |
%%                                                             | -----转发数据包---> |
%%                                                             | <---- 入队数据包--- |
%%   | <--------------------发送数据封包----------------------- | 
%%
%%  handler可以负责数据包的解析，acceptor和reset，同时可以将部分数据封包进行合并，从而提高udp利用效率
%%  如果某个UDP的端口没有回应了可以将整个handler及子进程全部杀死，这样可以减少端口风暴防止被发现封禁                                                          