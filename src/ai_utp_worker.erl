%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(ai_utp_worker).

-behaviour(gen_statem).

%% API
-export([start_link/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([idle/3,syn_sent/3]).
-export([connect/3,accept/2]).

-define(SERVER, ?MODULE).
-define(SYN_TIMEOUT, 3000).

-record(data, {
               parent :: pid(),
               socket :: port(),
               monitor :: reference(),
               remote,
               conn_id,
               stream,
               restransmit_timer,
               connector
              }).

%%%===================================================================
%%% API
%%%===================================================================
connect(Pid,Address,Port)->
  gen_statem:call(Pid, {connect,Address,Port}).
accept(Pid,Packet)->
  gen_statem:call(Pid,{accept,Packet}).
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),port()) ->
        {ok, Pid :: pid()} |
        ignore |
        {error, Error :: term()}.
start_link(Parent,Socket) ->
  gen_statem:start_link(?MODULE, [Parent,Socket], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
        gen_statem:init_result(atom()).
init([Parent,Socket]) ->
  Monitor = erlang:monitor(process, Parent),
  {ok, idle, #data{
                parent = Parent,
                socket = Socket,
                monitor = Monitor
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec idle('enter',
                 OldState :: atom(),
                 Data :: term()) ->
        gen_statem:state_enter_result('state_name');
                (gen_statem:event_type(),
                 Msg :: term(),
                 Data :: term()) ->
        gen_statem:event_handler_result(atom()).
idle({call,Caller}, {connect,Address,Port}, #data{socket = Socket } = Data) ->
  case connection_id({Address,Port}) of
    {ok,ConnID}->
      Remote = {Address,Port},
      Stream = ai_utp_stream:new(ConnID + 1,2),
      Packet = ai_utp_protocol:make_syn_packet(),
      {ok,_} = ai_utp_stream:send_packet(Socket, Remote,
                                         Packet, ConnID, Stream),
      Data0 = Data#data{remote = Remote,
                        conn_id = ConnID,
                        stream = Stream,
                        retransmit_timer = set_retransmit_timer(?SYN_TIMEOUT, undefined),
                        connector = Caller},
      {next_state,syn_sent,Data0};
    Error -> {stop,Error}
  end;
idle(info,Msg,Data) -> handle_info(Msg,Data).
syn_sent(info,{timeout, _TimerRef,Msg},Data)->
  

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
        any().
terminate(_Reason, _State, #data{monitor = undefined}) -> void;
terminate(_Reason, _State, #data{monitor = MRef}) ->
  erlang:demonitor(MRef,[flush]),
  void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
        {ok, NewState :: term(), NewData :: term()} |
        (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_info({'DOWN', MRef, process, Parent, _Reason},
            #data{parent = Parent,monitor = MRef})->
  {stop,shutdown,#data{}};
handle_info(Info,Data) ->
  error_logger:info_report({error,unknown,Info}),
  {keep_state,Data}.


connection_id(Remote)->
  connection_id(Remote,3).
connection_id(_,0)-> {error,exist};
connection_id(Remote,N)->
  ConnID = ai_utp_util:connection_id(),
  case ai_utp_conn:alloc(Remote, ConnID) of
    ok -> {ok,ConnID};
    _ -> connection_id(Remote,N-1)
  end.


set_retransmit_timer(N, Timer) ->
  set_retransmit_timer(N, N, Timer).

set_retransmit_timer(N, K, undefined) ->
  Ref = erlang:send_after(N, self(),{retransmit_timeout,K}),
  {set, Ref};
set_retransmit_timer(N, K, {set, Ref}) ->
  erlang:cancel_timer(Ref)
  NewRef = erlang:send_after(N, self(),{retransmit_timeout,K}),
  {set, NewRef}.
