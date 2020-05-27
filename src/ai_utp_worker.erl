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
-include("ai_utp.hrl").

%% API
-export([start_link/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([idle/3,syn_sent/3]).

-export([incoming/3,connect/3,accept/3,connected/3]).

-define(SERVER, ?MODULE).

-define(SYN_TIMEOUT, 3000).
-define(SYN_TIMEOUT_THRESHOLD, ?SYN_TIMEOUT*2).

-record(data, {
               parent :: pid(),
               socket :: port(),
               monitor :: reference(),
               remote,
               conn_id,
               stream,
               retransmit_timer,
               connector
              }).

%%%===================================================================
%%% API
%%%===================================================================
connect(Pid,Address,Port)->
  gen_statem:call(Pid, {connect,Address,Port}).
accept(Pid,Remote,Packet)->
  gen_statem:call(Pid,{accept,Packet,Remote}).
incoming(Pid,Packet,Timing)->
  gen_statem:cast(Pid,{packet,Packet,Timing}).
  
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
  case register_conn({Address,Port}) of
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
                        connector = {Caller,[]}},
      {next_state,syn_sent,Data0};
    _ -> {stop_and_reply,normal,[{reply,Caller,system_limit}]}
  end;
idle({call,Caller},{accept,#packet{
                              conn_id = ConnID,
                              seq_no = SeqNo,
                              win_sz = WindowSize
                             },Remote},#data{socket = Socket} = Data)->
  case ai_utp_conn:alloc(Remote, ai_utp_util:bit16(ConnID + 1)) of
    ok ->
      OurSeqNo = ai_utp_util:bit16_random(),
      Stream = ai_utp_stream:new(ConnID,OurSeqNo + 1),
      Stream0 = ai_utp_stream:connected(Stream,WindowSize,SeqNo + 1,
                                        undefined,undefined),
      Packet = ai_utp_protocol:make_ack_packet(OurSeqNo, SeqNo),
      ai_utp_stream:send_packet(Socket, Remote, Packet, Stream0),
      {next_state,connected,
       Data#data{stream = Stream0,remote = Remote},[{reply,Caller,ok}]};
    Error ->
      {stop_and_reply,normal,[{reply,Caller,Error}]}
  end;
idle(info,Msg,Data) -> handle_info(Msg,Data).
syn_sent(cast,{packet,#packet{type = st_reset},_},
         #data{ connector = {Caller,_}} = Data)->
  Actions = [{reply,Caller,econnrefused}],
  {stop_and_reply,normal,Actions,
   Data#data{connector = undefined}};
syn_sent(cast,{packet,#packet{
                         type = st_state,
                         win_sz = WindowSize,
                         seq_no = SeqNo},
               {TS,TSDiff,RecvTime}},
         #data{
            stream = Stream,
            retransmit_timer = RTimer,
            connector = {Caller,Packets}
           } = Data)->
  ReplyMicro = ai_utp_util:bit32(TS - RecvTime),
  AckNo = ai_utp_util:bit16(SeqNo + 1),
  Stream0 = ai_utp_stream:connected(Stream,WindowSize,AckNo,
                                    ReplyMicro,TSDiff),
  Self = self(),
  [incoming(Self, P, T) || {packet, P, T} <- lists:reverse(Packets)],
  {next_state,connected,
   Data#data{
     stream = Stream0,
     retransmit_timer = clear_retransmit_timer(RTimer),
     connector = undefined},[{reply,Caller,ok}]};
syn_sent(cast,{packet,_,_} = Packet,
         #data{ connector = {Caller,Packets} } =Data )->
  {keep_state,Data#data{connector = {Caller,[Packet|Packets]}}};
syn_sent(info,{timeout, TRef,{retransmit_timeout,N}},
         #data{socket = Socket,
               remote = Remote,
               conn_id = ConnID,
               retransmit_timer = {set,TRef},
               stream = Stream,
               connector = {Caller,_}
              } = Data)->
  if
    N > ?SYN_TIMEOUT_THRESHOLD ->
      Actions = [{reply,Caller,etimeout}],
      {stop_and_reply,normal,
       Actions,Data#data{connector = undefined}};
    true ->
      Packet = ai_utp_protocol:make_syn_packet(),
      {ok,_} = ai_utp_stream:send_packet(Socket, Remote,
                                         Packet, ConnID, Stream),
      {keep_state,Data#data{
                    retransmit_timer = set_retransmit_timer(N*2, undefined)
                   }}
  end;
syn_sent(info,Msg,Data) -> handle_info(Msg,Data).
connected(_Type,_Msg,Data)-> {keep_state,Data}.


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


register_conn(Remote)-> register_conn(Remote,3).
register_conn(_,0)-> {error,exist};
register_conn(Remote,N)->
  ConnID = ai_utp_util:bit16_random(),
  case ai_utp_conn:alloc(Remote, ConnID) of
    ok -> {ok,ConnID};
    _ -> register_conn(Remote,N-1)
  end.


set_retransmit_timer(N, Timer) ->
  set_retransmit_timer(N, N, Timer).
set_retransmit_timer(N, K, undefined) ->
  Ref = erlang:start_timer(N, self(),{retransmit_timeout,K}),
  {set, Ref};
set_retransmit_timer(N, K, {set, Ref}) ->
  erlang:cancel_timer(Ref),
  NewRef = erlang:start_timer(N, self(),{retransmit_timeout,K}),
  {set, NewRef}.
clear_retransmit_timer(undefined) ->undefined;
clear_retransmit_timer({set, Ref}) ->
  erlang:cancel_timer(Ref),
  undefined.
