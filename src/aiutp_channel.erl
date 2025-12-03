%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2025, David Gao
%%% @doc
%%% uTP 通道 - 使用 gen_statem 的连接状态机
%%%
%%% 本模块使用 gen_statem 行为管理单个 uTP 连接。
%%% 它取代了之前基于 gen_server 的 aiutp_worker，提供更清晰的状态管理。
%%%
%%% 状态机：
%%%   idle -> connecting -> connected -> closing -> closed
%%%        -> accepting  -> connected -> closing -> closed
%%%
%%% 状态说明：
%%%   - idle: 初始状态，等待连接或接受命令
%%%   - connecting: 客户端发起连接，已发送 SYN，等待 SYN-ACK
%%%   - accepting: 服务端收到 SYN，已发送 ACK，等待数据
%%%   - connected: 连接已建立，可双向传输数据
%%%   - closing: 已发送或收到 FIN，正在优雅关闭
%%%   - closed: 终止状态，进程将停止
%%%
%%% @end
%%% Created : 20 Jun 2025 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_channel).

-behaviour(gen_statem).

-include("aiutp.hrl").

%% API 导出
-export([start_link/2]).
-export([connect/4, accept/4, incoming/2]).
-export([send/2, recv/2, active/2, close/2]).
-export([controlling_process/2]).

%% gen_statem 回调
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% 状态函数
-export([idle/3, connecting/3, accepting/3, connected/3, closing/3]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% 通道状态数据 (map)
%%
%% 键说明：
%% - parent: pid() - 父套接字进程
%% - parent_monitor: reference() - 父进程监视器引用
%% - socket: gen_udp:socket() - UDP 套接字
%% - remote: {inet:ip_address(), inet:port_number()} | undefined - 远程地址
%% - conn_id: non_neg_integer() | undefined - 连接 ID
%% - controller: pid() | undefined - 控制进程
%% - controller_monitor: reference() | undefined - 控制进程监视器引用
%% - pcb: #aiutp_pcb{} | undefined - 协议控制块
%% - tick_timer: reference() | undefined - 定时器引用
%% - blocker: gen_statem:from() | undefined - 阻塞的调用者
%% - active: boolean() - 主动模式标志
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
%% @doc 启动新的通道进程
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), port()) -> {ok, pid()} | {error, term()}.
start_link(Parent, Socket) ->
    gen_statem:start_link(?MODULE, [Parent, Socket], []).

%%--------------------------------------------------------------------
%% @doc 发起出站连接
%% @end
%%--------------------------------------------------------------------
-spec connect(pid(), pid(), inet:ip_address() | string(), inet:port_number()) ->
    ok | {error, term()}.
connect(Pid, Caller, Address, Port) ->
    gen_statem:call(Pid, {connect, Caller, {Address, Port}}, infinity).

%%--------------------------------------------------------------------
%% @doc 接受入站连接
%% @end
%%--------------------------------------------------------------------
-spec accept(pid(), pid(), tuple(), term()) -> ok | {error, term()}.
accept(Pid, Caller, Remote, Packet) ->
    gen_statem:call(Pid, {accept, Caller, Remote, Packet}, infinity).

%%--------------------------------------------------------------------
%% @doc 处理传入的数据包
%% @end
%%--------------------------------------------------------------------
-spec incoming(pid(), term()) -> ok.
incoming(Pid, Packet) ->
    gen_statem:cast(Pid, {packet, Packet}).

%%--------------------------------------------------------------------
%% @doc 发送数据
%% @end
%%--------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) ->
    gen_statem:call(Pid, {send, Data}, infinity).

%%--------------------------------------------------------------------
%% @doc 接收数据（占位符 - 实际接收逻辑在 connected 状态中）
%% @end
%%--------------------------------------------------------------------
-spec recv(pid(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv(Pid, Len) ->
    gen_statem:call(Pid, {recv, Len}, infinity).

%%--------------------------------------------------------------------
%% @doc 设置主动模式
%% @end
%%--------------------------------------------------------------------
-spec active(pid(), boolean()) -> ok.
active(Pid, V) ->
    gen_statem:call(Pid, {active, V}, infinity).

%%--------------------------------------------------------------------
%% @doc 关闭连接
%% @end
%%--------------------------------------------------------------------
-spec close(pid(), pid()) -> ok | {error, term()}.
close(Pid, Caller) ->
    gen_statem:call(Pid, {close, Caller}, infinity).

%%--------------------------------------------------------------------
%% @doc 将控制权转移给另一个进程
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
%%% gen_statem 回调函数
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
%% 状态: idle
%% 初始状态，等待连接或接受命令
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
%% 状态: connecting
%% 客户端发起连接，已发送 SYN，等待 SYN-ACK
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
           #{controller := Controller, controller_monitor := MRef,
             pcb := PCB} = Data) ->
    %% 控制进程在连接过程中崩溃，发送 RESET 通知对端并关闭连接
    PCB1 = aiutp_pcb:close(PCB),
    {next_state, closing, Data#{controller := undefined,
                                controller_monitor := undefined,
                                blocker := undefined,
                                pcb := PCB1}};

connecting({call, From}, _Msg, _Data) ->
    {keep_state_and_data, [{reply, From, {error, connecting}}]};

connecting(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% 状态: accepting
%% 服务端收到 SYN，已发送 ACK，正在转换到已连接状态
%%--------------------------------------------------------------------
accepting(enter, idle, #{pcb := PCB} = Data) ->
    %% 检查是否已连接（SYN-ACK 交换完成）
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
          #{controller := Controller, controller_monitor := MRef,
            pcb := PCB} = Data) ->
    %% 控制进程在接受连接过程中崩溃，发送 RESET 通知对端并关闭连接
    PCB1 = aiutp_pcb:close(PCB),
    {next_state, closing, Data#{controller := undefined,
                                controller_monitor := undefined,
                                pcb := PCB1}};

accepting({call, From}, _Msg, _Data) ->
    {keep_state_and_data, [{reply, From, {error, accepting}}]};

accepting(info, _Msg, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% 状态: connected
%% 连接已建立，可双向传输数据
%%--------------------------------------------------------------------
connected(enter, _OldState, Data) ->
    {keep_state, active_read(Data)};

connected({call, From}, {send, SendData}, #{pcb := PCB} = Data) ->
    case aiutp_pcb:write(SendData, PCB) of
        {{error, _} = Error, PCB1} ->
            {keep_state, Data#{pcb := PCB1}, [{reply, From, Error}]};
        {ok, PCB1} ->
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
    %% TODO: 实现阻塞式接收
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
%% 状态: closing
%% 已发送或收到 FIN，正在优雅关闭
%%--------------------------------------------------------------------
closing(enter, _OldState, #{parent := Parent,
                            controller := Controller,
                            active := Active} = Data) ->
    cancel_tick_timer(maps:get(tick_timer, Data, undefined)),
    Remote = maps:get(remote, Data, undefined),
    ConnId = maps:get(conn_id, Data, undefined),
    Blocker = maps:get(blocker, Data, undefined),
    %% 通知阻塞的调用者（如有）
    Actions = case Blocker of
        undefined -> [];
        _ -> [{reply, Blocker, ok}]
    end,
    %% 如果处于主动模式，通知控制进程
    if Controller /= undefined andalso Active == true ->
            Controller ! {utp_closed, make_utp_socket(Data), normal};
       true -> ok
    end,
    %% 从套接字释放连接
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
%% terminate 回调
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, Data) ->
    cleanup_monitors(Data),
    ok.

%%--------------------------------------------------------------------
%% code_change 回调
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down, term()},
        State :: term(), Data :: term(), Extra :: term()) ->
        {ok, NewState :: term(), NewData :: term()} | (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 尝试添加连接 ID，最多重试 3 次
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
%% @doc 启动定期超时检查的定时器
%% @end
%%--------------------------------------------------------------------
-spec start_tick_timer(pos_integer()) -> reference().
start_tick_timer(Interval) ->
    erlang:start_timer(Interval, self(), tick).

%%--------------------------------------------------------------------
%% @doc 取消定时器
%% @end
%%--------------------------------------------------------------------
-spec cancel_tick_timer(reference() | undefined) -> ok.
cancel_tick_timer(undefined) ->
    ok;
cancel_tick_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.

%%--------------------------------------------------------------------
%% @doc 清理所有监视器
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
%% @doc 处理主动模式 - 读取并投递数据
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
            %% 发送数据后将 active 设为 false（once 模式行为）
            Data#{pcb := PCB1, active := false}
    end;
active_read(#{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:flush(PCB),
    Data#{pcb := PCB1};
active_read(Data) ->
    %% 尚无 PCB，直接返回原数据
    Data.

%%--------------------------------------------------------------------
%% @doc 创建用于消息的 UTP 套接字元组
%% @end
%%--------------------------------------------------------------------
-spec make_utp_socket(data()) -> {utp, pid(), pid()}.
make_utp_socket(#{parent := Parent}) ->
    {utp, Parent, self()}.

%%--------------------------------------------------------------------
%% @doc 在 controlling_process 期间同步输入消息给新所有者
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
