%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2025, David Gao
%%% @doc
%%% uTP Channel - Connection state machine using gen_statem
%%%
%%% This module manages a single uTP connection using gen_statem behavior.
%%% It replaces the previous gen_server based aiutp_worker with clearer
%%% state management.
%%%
%%% State Machine:
%%%   idle -> connecting -> connected -> closing -> closed
%%%        -> accepting  -> connected -> closing -> closed
%%%
%%% States:
%%%   - idle: Initial state, waiting for connect or accept
%%%   - connecting: Client initiated, SYN sent, waiting for SYN-ACK
%%%   - accepting: Server received SYN, sent ACK, waiting for data
%%%   - connected: Connection established, bidirectional data transfer
%%%   - closing: FIN sent or received, graceful shutdown in progress
%%%   - closed: Terminal state, process will stop
%%%
%%% @end
%%% Created : 20 Jun 2025 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_channel).

-behaviour(gen_statem).

-include("aiutp.hrl").

%% API
-export([start_link/2]).
-export([connect/4, accept/4, incoming/2]).
-export([send/2, recv/2, active/2, close/2]).
-export([controlling_process/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% State functions
-export([idle/3, connecting/3, accepting/3, connected/3, closing/3]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% Channel State Data (map)
%%
%% Keys:
%% - parent: pid() - Parent socket process
%% - parent_monitor: reference() - Monitor ref for parent
%% - socket: gen_udp:socket() - UDP socket
%% - remote: {inet:ip_address(), inet:port_number()} | undefined - Remote address
%% - conn_id: non_neg_integer() | undefined - Connection ID
%% - controller: pid() | undefined - Controlling process
%% - controller_monitor: reference() | undefined - Monitor ref for controller
%% - pcb: #aiutp_pcb{} | undefined - Protocol Control Block
%% - tick_timer: reference() | undefined - Timer reference
%% - blocker: gen_statem:from() | undefined - Blocked caller
%% - active: boolean() - Active mode flag
%%------------------------------------------------------------------------------
-type data() :: #{
    parent := pid(),
    parent_monitor := reference(),
    socket := gen_udp:socket(),
    remote => {inet:ip_address(), inet:port_number()} | undefined,
    conn_id => non_neg_integer() | undefined,
    controller => pid() | undefined,
    controller_monitor => reference() | undefined,
    pcb => #aiutp_pcb{} | undefined,
    tick_timer => reference() | undefined,
    blocker => gen_statem:from() | undefined,
    active := boolean()
}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start a new channel process
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), port()) -> {ok, pid()} | {error, term()}.
start_link(Parent, Socket) ->
    gen_statem:start_link(?MODULE, [Parent, Socket], []).

%%--------------------------------------------------------------------
%% @doc Initiate outbound connection
%% @end
%%--------------------------------------------------------------------
-spec connect(pid(), pid(), inet:ip_address() | string(), inet:port_number()) ->
    ok | {error, term()}.
connect(Pid, Caller, Address, Port) ->
    gen_statem:call(Pid, {connect, Caller, {Address, Port}}, infinity).

%%--------------------------------------------------------------------
%% @doc Accept inbound connection
%% @end
%%--------------------------------------------------------------------
-spec accept(pid(), pid(), tuple(), term()) -> ok | {error, term()}.
accept(Pid, Caller, Remote, Packet) ->
    gen_statem:call(Pid, {accept, Caller, Remote, Packet}, infinity).

%%--------------------------------------------------------------------
%% @doc Handle incoming packet
%% @end
%%--------------------------------------------------------------------
-spec incoming(pid(), term()) -> ok.
incoming(Pid, Packet) ->
    gen_statem:cast(Pid, {packet, Packet}).

%%--------------------------------------------------------------------
%% @doc Send data
%% @end
%%--------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) ->
    gen_statem:call(Pid, {send, Data}, infinity).

%%--------------------------------------------------------------------
%% @doc Receive data (placeholder - actual recv logic in connected state)
%% @end
%%--------------------------------------------------------------------
-spec recv(pid(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv(Pid, Len) ->
    gen_statem:call(Pid, {recv, Len}, infinity).

%%--------------------------------------------------------------------
%% @doc Set active mode
%% @end
%%--------------------------------------------------------------------
-spec active(pid(), boolean()) -> ok.
active(Pid, V) ->
    gen_statem:call(Pid, {active, V}, infinity).

%%--------------------------------------------------------------------
%% @doc Close connection
%% @end
%%--------------------------------------------------------------------
-spec close(pid(), pid()) -> ok | {error, term()}.
close(Pid, Caller) ->
    gen_statem:call(Pid, {close, Caller}, infinity).

%%--------------------------------------------------------------------
%% @doc Transfer control to another process
%% @end
%%--------------------------------------------------------------------
-spec controlling_process(pid(), pid()) -> ok | {error, term()}.
controlling_process(Pid, NewOwner) ->
    Caller = self(),
    case gen_statem:call(Pid, controller) of
        {ok, Control} when Control == NewOwner ->
            ok;
        {ok, Control} when Control /= Caller ->
            {error, not_owner};
        {ok, _Control} ->
            {ok, Active} = gen_statem:call(Pid, active),
            if Active == true -> active(Pid, false);
               true -> ok
            end,
            Closed = sync_input(Pid, NewOwner, false),
            if Closed == true -> ok;
               true -> gen_statem:call(Pid, {controlling_process, Caller, NewOwner, Active}, infinity)
            end
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> [state_functions, state_enter].

-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([Parent, Socket]) ->
    ParentMonitor = erlang:monitor(process, Parent),
    Data = #{
        parent => Parent,
        socket => Socket,
        parent_monitor => ParentMonitor,
        active => false
    },
    {ok, idle, Data}.

%%--------------------------------------------------------------------
%% State: idle
%% Initial state, waiting for connect or accept command
%%--------------------------------------------------------------------
idle(enter, _OldState, Data) ->
    {keep_state, Data};

idle({call, From}, {connect, Controller, Remote},
     #{parent := Parent, socket := Socket} = Data) ->
    case add_conn(Parent, Remote) of
        {ok, ConnId} ->
            PCB = aiutp_pcb:connect({Socket, Remote}, ConnId),
            ControllerMonitor = erlang:monitor(process, Controller),
            Timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL),
            NewData = Data#{
                remote => Remote,
                controller => Controller,
                controller_monitor => ControllerMonitor,
                blocker => From,
                pcb => PCB,
                conn_id => ConnId,
                tick_timer => Timer
            },
            {next_state, connecting, NewData};
        Error ->
            {stop_and_reply, normal, [{reply, From, Error}]}
    end;

idle({call, From}, {accept, Controller, Remote, Packet},
     #{parent := Parent, socket := Socket} = Data) ->
    {ConnId, PCB} = aiutp_pcb:accept({Socket, Remote}, Packet),
    case aiutp_socket:add_conn(Parent, Remote, ConnId) of
        ok ->
            ControllerMonitor = erlang:monitor(process, Controller),
            Timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL),
            NewData = Data#{
                remote => Remote,
                controller => Controller,
                controller_monitor => ControllerMonitor,
                pcb => PCB,
                conn_id => ConnId,
                tick_timer => Timer
            },
            {next_state, accepting, NewData, [{reply, From, ok}]};
        Error ->
            {stop_and_reply, normal, [{reply, From, Error}]}
    end;

idle({call, From}, _Msg, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

idle(cast, _Msg, _Data) ->
    keep_state_and_data;

idle(info, {'DOWN', MRef, process, Parent, _Reason},
     #{parent := Parent, parent_monitor := MRef}) ->
    {stop, normal};

idle(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% State: connecting
%% Client initiated connection, SYN sent, waiting for SYN-ACK
%%--------------------------------------------------------------------
connecting(enter, idle, _Data) ->
    {keep_state_and_data, []};

connecting(cast, {packet, Packet}, #{pcb := PCB, blocker := Blocker} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(Packet, PCB),
    case aiutp_pcb:state(PCB1) of
        ?CS_CONNECTED ->
            NewData = Data#{pcb := PCB1, blocker := undefined},
            {next_state, connected, NewData, [{reply, Blocker, ok}]};
        ?CS_RESET ->
            NewData = Data#{pcb := PCB1, blocker := undefined},
            {next_state, closing, NewData, [{reply, Blocker, {error, reset}}]};
        _ ->
            {keep_state, Data#{pcb := PCB1}}
    end;

connecting(info, {timeout, TRef, tick}, #{tick_timer := TRef, pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:check_timeouts(PCB),
    case aiutp_pcb:closed(PCB1) of
        {closed, Reason} ->
            NewData = Data#{pcb := PCB1, tick_timer := undefined},
            case maps:get(blocker, Data, undefined) of
                undefined ->
                    {next_state, closing, NewData};
                Blocker ->
                    {next_state, closing, NewData#{blocker := undefined},
                     [{reply, Blocker, {error, Reason}}]}
            end;
        not_closed ->
            Timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end;

connecting(info, {'DOWN', MRef, process, Parent, _Reason},
           #{parent := Parent, parent_monitor := MRef} = Data) ->
    cleanup_monitors(Data),
    Actions = case maps:get(blocker, Data, undefined) of
        undefined -> [];
        Blocker -> [{reply, Blocker, {error, crash}}]
    end,
    {stop_and_reply, normal, Actions};

connecting(info, {'DOWN', MRef, process, Controller, _Reason},
           #{controller := Controller, controller_monitor := MRef} = Data) ->
    %% Controller crashed during connect, close the connection
    {next_state, closing, Data#{controller := undefined,
                                controller_monitor := undefined,
                                blocker := undefined}};

connecting({call, From}, _Msg, _Data) ->
    {keep_state_and_data, [{reply, From, {error, connecting}}]};

connecting(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% State: accepting
%% Server received SYN, sent ACK, transitioning to connected
%%--------------------------------------------------------------------
accepting(enter, idle, #{pcb := PCB} = Data) ->
    %% Check if already connected (SYN-ACK exchange complete)
    case aiutp_pcb:state(PCB) of
        ?CS_CONNECTED ->
            {next_state, connected, Data};
        ?CS_SYN_RECV ->
            {keep_state_and_data, []};
        _ ->
            {keep_state_and_data, []}
    end;

accepting(cast, {packet, Packet}, #{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(Packet, PCB),
    case aiutp_pcb:state(PCB1) of
        ?CS_CONNECTED ->
            {next_state, connected, Data#{pcb := PCB1}};
        ?CS_RESET ->
            {next_state, closing, Data#{pcb := PCB1}};
        _ ->
            {keep_state, Data#{pcb := PCB1}}
    end;

accepting(info, {timeout, TRef, tick}, #{tick_timer := TRef, pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:check_timeouts(PCB),
    case aiutp_pcb:closed(PCB1) of
        {closed, _Reason} ->
            {next_state, closing, Data#{pcb := PCB1, tick_timer := undefined}};
        not_closed ->
            Timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end;

accepting(info, {'DOWN', MRef, process, Parent, _Reason},
          #{parent := Parent, parent_monitor := MRef} = Data) ->
    cleanup_monitors(Data),
    {stop, normal};

accepting(info, {'DOWN', MRef, process, Controller, _Reason},
          #{controller := Controller, controller_monitor := MRef} = Data) ->
    {next_state, closing, Data#{controller := undefined,
                                controller_monitor := undefined}};

accepting({call, From}, _Msg, _Data) ->
    {keep_state_and_data, [{reply, From, {error, accepting}}]};

accepting(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% State: connected
%% Connection established, bidirectional data transfer
%%--------------------------------------------------------------------
connected(enter, _OldState, Data) ->
    {keep_state, active_read(Data)};

connected({call, From}, {send, SendData}, #{pcb := PCB} = Data) ->
    case aiutp_pcb:write(SendData, PCB) of
        {Error, PCB1} ->
            {keep_state, Data#{pcb := PCB1}, [{reply, From, Error}]};
        PCB1 ->
            {keep_state, Data#{pcb := PCB1}, [{reply, From, ok}]}
    end;

connected({call, From}, {active, Active}, Data) ->
    NewData = active_read(Data#{active := Active}),
    {keep_state, NewData, [{reply, From, ok}]};

connected({call, From}, controller, #{controller := Controller}) ->
    {keep_state_and_data, [{reply, From, {ok, Controller}}]};

connected({call, From}, active, #{active := Active}) ->
    {keep_state_and_data, [{reply, From, {ok, Active}}]};

connected({call, From}, {controlling_process, OldController, NewController, Active},
          #{controller := OldController, controller_monitor := OldMon} = Data) ->
    erlang:demonitor(OldMon, [flush]),
    NewMon = erlang:monitor(process, NewController),
    NewData = active_read(Data#{
        controller := NewController,
        controller_monitor := NewMon,
        active := Active
    }),
    {keep_state, NewData, [{reply, From, ok}]};

connected({call, From}, {close, Controller},
          #{pcb := PCB, controller := Controller,
            controller_monitor := ControllerMonitor} = Data) ->
    PCB1 = aiutp_pcb:close(PCB),
    case aiutp_pcb:state(PCB1) of
        State when State == ?CS_DESTROY; State == ?CS_RESET ->
            if ControllerMonitor /= undefined ->
                    erlang:demonitor(ControllerMonitor, [flush]);
               true -> ok
            end,
            NewData = Data#{
                pcb := PCB1,
                controller := undefined,
                controller_monitor := undefined,
                active := false
            },
            {next_state, closing, NewData, [{reply, From, ok}]};
        _ ->
            NewData = Data#{
                pcb := PCB1,
                blocker => From
            },
            {next_state, closing, NewData}
    end;

connected({call, From}, {close, _NotOwner}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_owner}}]};

connected({call, From}, {recv, _Len}, _Data) ->
    %% TODO: Implement blocking recv
    {keep_state_and_data, [{reply, From, {error, not_implemented}}]};

connected(cast, {packet, Packet}, #{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(Packet, PCB),
    case aiutp_pcb:state(PCB1) of
        State when State == ?CS_DESTROY; State == ?CS_RESET ->
            {next_state, closing, Data#{pcb := PCB1}};
        _ ->
            NewData = active_read(Data#{pcb := PCB1}),
            {keep_state, NewData}
    end;

connected(info, {timeout, TRef, tick}, #{tick_timer := TRef, pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:check_timeouts(PCB),
    case aiutp_pcb:closed(PCB1) of
        {closed, _Reason} ->
            {next_state, closing, Data#{pcb := PCB1, tick_timer := undefined}};
        not_closed ->
            Timer = start_tick_timer(?TIMEOUT_CHECK_INTERVAL),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end;

connected(info, {'DOWN', MRef, process, Parent, _Reason},
          #{parent := Parent, parent_monitor := MRef,
            controller := Controller, active := Active} = Data) ->
    cleanup_monitors(Data),
    if Controller /= undefined andalso Active == true ->
            Controller ! {utp_closed, make_utp_socket(Data), crash};
       true -> ok
    end,
    {stop, normal};

connected(info, {'DOWN', MRef, process, Controller, _Reason},
          #{controller := Controller, controller_monitor := MRef,
            pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:close(PCB),
    NewData = Data#{
        pcb := PCB1,
        controller := undefined,
        controller_monitor := undefined,
        blocker := undefined
    },
    {next_state, closing, NewData};

connected(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% State: closing
%% FIN sent or received, graceful shutdown in progress
%%--------------------------------------------------------------------
closing(enter, _OldState, #{parent := Parent,
                            controller := Controller,
                            active := Active} = Data) ->
    cancel_tick_timer(maps:get(tick_timer, Data, undefined)),
    Remote = maps:get(remote, Data, undefined),
    ConnId = maps:get(conn_id, Data, undefined),
    Blocker = maps:get(blocker, Data, undefined),
    %% Notify blocker if any
    Actions = case Blocker of
        undefined -> [];
        _ -> [{reply, Blocker, ok}]
    end,
    %% Notify controller if in active mode
    if Controller /= undefined andalso Active == true ->
            Controller ! {utp_closed, make_utp_socket(Data), normal};
       true -> ok
    end,
    %% Free connection from socket
    if ConnId /= undefined ->
            aiutp_socket:free_conn(Parent, Remote, ConnId);
       true -> ok
    end,
    {stop, normal, Data, Actions};

closing({call, From}, _Msg, _Data) ->
    {keep_state_and_data, [{reply, From, {error, closed}}]};

closing(cast, _Msg, _Data) ->
    keep_state_and_data;

closing(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% terminate callback
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, Data) ->
    cleanup_monitors(Data),
    ok.

%%--------------------------------------------------------------------
%% code_change callback
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down, term()},
        State :: term(), Data :: term(), Extra :: term()) ->
        {ok, NewState :: term(), NewData :: term()} | (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Try to add connection ID, retry up to 3 times
%% @end
%%--------------------------------------------------------------------
-spec add_conn(pid(), {inet:ip_address(), inet:port_number()}) ->
    {ok, non_neg_integer()} | {error, eagain}.
add_conn(Parent, Remote) ->
    add_conn(Parent, Remote, 3).

-spec add_conn(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, eagain}.
add_conn(_, _, 0) ->
    {error, eagain};
add_conn(Parent, Remote, N) ->
    ConnID = aiutp_util:bit16_random(),
    case aiutp_socket:add_conn(Parent, Remote, ConnID) of
        ok -> {ok, ConnID};
        _ -> add_conn(Parent, Remote, N - 1)
    end.

%%--------------------------------------------------------------------
%% @doc Start tick timer for periodic timeout checks
%% @end
%%--------------------------------------------------------------------
-spec start_tick_timer(pos_integer()) -> reference().
start_tick_timer(Interval) ->
    erlang:start_timer(Interval, self(), tick).

%%--------------------------------------------------------------------
%% @doc Cancel tick timer
%% @end
%%--------------------------------------------------------------------
-spec cancel_tick_timer(reference() | undefined) -> ok.
cancel_tick_timer(undefined) ->
    ok;
cancel_tick_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup all monitors
%% @end
%%--------------------------------------------------------------------
-spec cleanup_monitors(data()) -> ok.
cleanup_monitors(Data) ->
    ParentMon = maps:get(parent_monitor, Data, undefined),
    ControllerMon = maps:get(controller_monitor, Data, undefined),
    Timer = maps:get(tick_timer, Data, undefined),
    if ParentMon /= undefined -> erlang:demonitor(ParentMon, [flush]);
       true -> ok
    end,
    if ControllerMon /= undefined -> erlang:demonitor(ControllerMon, [flush]);
       true -> ok
    end,
    cancel_tick_timer(Timer),
    ok.

%%--------------------------------------------------------------------
%% @doc Handle active mode - read and deliver data
%% @end
%%--------------------------------------------------------------------
-spec active_read(data()) -> data().
active_read(#{controller := undefined} = Data) ->
    Data;
active_read(#{pcb := PCB, controller := Controller, active := true} = Data) ->
    case aiutp_pcb:read(PCB) of
        {undefined, PCB1} ->
            Data#{pcb := PCB1};
        {Payload, PCB1} ->
            UTPSocket = make_utp_socket(Data),
            Controller ! {utp_data, UTPSocket, Payload},
            %% After sending data, set active to false (once mode behavior)
            Data#{pcb := PCB1, active := false}
    end;
active_read(#{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:flush(PCB),
    Data#{pcb := PCB1};
active_read(Data) ->
    %% No PCB yet, just return data unchanged
    Data.

%%--------------------------------------------------------------------
%% @doc Create UTP socket tuple for messages
%% @end
%%--------------------------------------------------------------------
-spec make_utp_socket(data()) -> {utp, pid(), pid()}.
make_utp_socket(#{parent := Parent}) ->
    {utp, Parent, self()}.

%%--------------------------------------------------------------------
%% @doc Sync input messages to new owner during controlling_process
%% @end
%%--------------------------------------------------------------------
-spec sync_input(term(), pid(), boolean()) -> boolean().
sync_input(Socket, NewOwner, Flag) ->
    receive
        {utp_data, Socket, _} = M ->
            NewOwner ! M,
            sync_input(Socket, NewOwner, Flag);
        {utp_closed, Socket, _} = M ->
            NewOwner ! M,
            sync_input(Socket, NewOwner, true)
    after 0 ->
        Flag
    end.
