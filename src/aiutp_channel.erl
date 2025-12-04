%%------------------------------------------------------------------------------
%% @doc uTP 通道 - 连接状态机
%%
%% 本模块使用 gen_statem 行为管理单个 uTP 连接的生命周期。
%% 每个 uTP 连接由一个独立的 channel 进程管理。
%%
%% == 架构图 ==
%% ```
%%                    aiutp_socket (UDP 端口管理)
%%                           │
%%                    aiutp_channel_sup
%%                    /      │       \
%%            channel_1  channel_2  channel_N
%%                │
%%          +-----+-----+
%%          │           │
%%        PCB      Controller
%%   (协议状态)    (用户进程)
%% '''
%%
%% == 状态机 ==
%% ```
%%                     ┌──────────┐
%%                     │   idle   │ 初始状态
%%                     └────┬─────┘
%%                ┌─────────┴─────────┐
%%          connect()              accept()
%%                │                    │
%%         ┌──────▼──────┐      ┌──────▼──────┐
%%         │ connecting  │      │  accepting  │
%%         │ (发送SYN)   │      │ (收到SYN)   │
%%         └──────┬──────┘      └──────┬──────┘
%%                │                    │
%%          收到SYN-ACK           发送ACK
%%                │                    │
%%                └─────────┬──────────┘
%%                   ┌──────▼──────┐
%%                   │  connected  │ 数据传输
%%                   └──────┬──────┘
%%                          │
%%                   close()/超时/RESET
%%                          │
%%                   ┌──────▼──────┐
%%                   │   closing   │ 清理资源
%%                   └──────┬──────┘
%%                          │
%%                       停止进程
%% '''
%%
%% == 状态说明 ==
%% - idle: 初始状态，等待 connect 或 accept 命令
%% - connecting: 客户端发起连接，已发送 SYN，等待 SYN-ACK
%% - accepting: 服务端收到 SYN，已发送 STATE，等待数据确认连接
%% - connected: 连接已建立，可双向传输数据
%% - closing: 连接正在关闭，清理资源后停止进程
%%
%% == 主动模式 (Active Mode) ==
%% - active=true: 数据自动发送给 controller 进程 (once 语义)
%% - active=false: 数据缓存在 PCB 中，需调用 recv 获取
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @copyright (C) 2025, David Gao
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_channel).
-behaviour(gen_statem).
-include("aiutp.hrl").

%%==============================================================================
%% API 导出
%%==============================================================================
-export([
    start_link/2,           %% 启动通道进程
    connect/4,              %% 发起出站连接
    accept/4,               %% 接受入站连接
    incoming/2,             %% 处理传入数据包
    send/2,                 %% 发送数据
    recv/2,                 %% 接收数据
    active/2,               %% 设置主动模式
    close/2,                %% 关闭连接
    controlling_process/2   %% 转移控制权
]).

%%==============================================================================
%% gen_statem 回调导出
%%==============================================================================
-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4
]).

%%==============================================================================
%% 状态函数导出
%%==============================================================================
-export([
    idle/3,
    connecting/3,
    accepting/3,
    connected/3,
    closing/3
]).

%%==============================================================================
%% 类型定义
%%==============================================================================

%% 通道状态数据
%% 使用 map 而非 record，提高可读性和灵活性
-type state_data() :: #{
    %% 必需字段
    parent := pid(),                    %% 父套接字进程 (aiutp_socket)
    parent_monitor := reference(),       %% 父进程监视器
    socket := gen_udp:socket(),          %% UDP 套接字
    active := boolean(),                 %% 主动模式标志

    %% 可选字段 (连接建立后填充)
    remote => {inet:ip_address(), inet:port_number()},  %% 远端地址
    conn_id => non_neg_integer(),                       %% 连接 ID
    controller => pid(),                                %% 控制进程
    controller_monitor => reference(),                  %% 控制进程监视器
    pcb => #aiutp_pcb{},                               %% 协议控制块
    tick_timer => reference(),                         %% 超时检查定时器
    blocker => gen_statem:from()                       %% 阻塞的调用者
}.

%%==============================================================================
%% 常量
%%==============================================================================

%% 连接 ID 生成重试次数
-define(CONN_ID_RETRY_COUNT, 3).

%%==============================================================================
%% API 函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 启动新的通道进程
%%
%% 由 aiutp_channel_sup 调用，创建一个新的 channel 进程。
%% 进程启动后处于 idle 状态，等待 connect 或 accept 命令。
%%
%% @param Parent 父套接字进程 (aiutp_socket)
%% @param Socket UDP 套接字
%% @returns {ok, Pid} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), gen_udp:socket()) -> gen_statem:start_ret().
start_link(Parent, Socket) ->
    gen_statem:start_link(?MODULE, {Parent, Socket}, []).

%%------------------------------------------------------------------------------
%% @doc 发起出站连接
%%
%% 发送 SYN 包到远端，进入 connecting 状态等待响应。
%% 调用会阻塞直到连接建立或失败。
%%
%% @param Pid 通道进程
%% @param Caller 控制进程 (接收数据和通知的进程)
%% @param Address 远端 IP 地址或主机名
%% @param Port 远端端口
%% @returns ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec connect(pid(), pid(), inet:ip_address() | string(), inet:port_number()) ->
    ok | {error, term()}.
connect(Pid, Caller, Address, Port) ->
    gen_statem:call(Pid, {connect, Caller, {Address, Port}}, infinity).

%%------------------------------------------------------------------------------
%% @doc 接受入站连接
%%
%% 处理收到的 SYN 包，发送 STATE 响应，进入 accepting 状态。
%%
%% @param Pid 通道进程
%% @param Caller 控制进程
%% @param Remote 远端地址 {IP, Port}
%% @param PacketInfo 收到的 SYN 数据包及接收时间 {Packet, RecvTime}
%% @returns ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec accept(pid(), pid(), {inet:ip_address(), inet:port_number()},
             {#aiutp_packet{}, non_neg_integer()}) -> ok | {error, term()}.
accept(Pid, Caller, Remote, PacketInfo) ->
    gen_statem:call(Pid, {accept, Caller, Remote, PacketInfo}, infinity).

%%------------------------------------------------------------------------------
%% @doc 处理传入的数据包
%%
%% 由 aiutp_socket 调用，将收到的 UDP 数据包分发给对应的 channel。
%% 使用 cast 异步处理，不阻塞 socket 进程。
%%
%% @param Pid 通道进程
%% @param PacketInfo 数据包和接收时间元组 {Packet, RecvTime}
%% @end
%%------------------------------------------------------------------------------
-spec incoming(pid(), {#aiutp_packet{}, non_neg_integer()}) -> ok.
incoming(Pid, PacketInfo) ->
    gen_statem:cast(Pid, {packet, PacketInfo}).

%%------------------------------------------------------------------------------
%% @doc 发送数据
%%
%% 将数据写入 PCB 发送缓冲区，由协议层负责分片和发送。
%%
%% @param Pid 通道进程
%% @param Data 要发送的数据
%% @returns ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) ->
    gen_statem:call(Pid, {send, Data}, infinity).

%%------------------------------------------------------------------------------
%% @doc 接收数据
%%
%% 从 PCB 接收缓冲区读取数据。
%% 注意: 当前实现仅支持主动模式，此函数返回 not_implemented。
%%
%% @param Pid 通道进程
%% @param Len 期望接收的字节数 (当前未使用)
%% @returns {ok, Data} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec recv(pid(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv(Pid, Len) ->
    gen_statem:call(Pid, {recv, Len}, infinity).

%%------------------------------------------------------------------------------
%% @doc 设置主动模式
%%
%% - active=true: 收到数据时自动发送 {utp_data, Socket, Data} 给 controller
%% - active=false: 数据缓存在 PCB 中
%%
%% 注意: 当前实现使用 "once" 语义，即发送一次数据后自动切换为 false。
%%
%% @param Pid 通道进程
%% @param Active 是否启用主动模式
%% @returns ok
%% @end
%%------------------------------------------------------------------------------
-spec active(pid(), boolean()) -> ok.
active(Pid, Active) ->
    gen_statem:call(Pid, {active, Active}, infinity).

%%------------------------------------------------------------------------------
%% @doc 关闭连接
%%
%% 优雅关闭连接，发送 FIN 包通知对端。
%% 只有 controller 进程可以关闭连接。
%%
%% @param Pid 通道进程
%% @param Caller 调用者进程 (必须是 controller)
%% @returns ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec close(pid(), pid()) -> ok | {error, term()}.
close(Pid, Caller) ->
    gen_statem:call(Pid, {close, Caller}, infinity).

%%------------------------------------------------------------------------------
%% @doc 转移控制权
%%
%% 将连接的控制权从当前 controller 转移给新进程。
%% 新进程将接收后续的 utp_data 和 utp_closed 消息。
%%
%% 转移过程:
%% 1. 验证调用者是当前 controller
%% 2. 暂停主动模式
%% 3. 将当前进程邮箱中的 utp 消息转发给新 owner
%% 4. 更新 controller 并恢复主动模式
%%
%% @param Pid 通道进程
%% @param NewOwner 新的控制进程
%% @returns ok | {error, not_owner}
%% @end
%%------------------------------------------------------------------------------
-spec controlling_process(pid(), pid()) -> ok | {error, term()}.
controlling_process(Pid, NewOwner) ->
    Caller = self(),
    case gen_statem:call(Pid, get_controller) of
        {ok, NewOwner} ->
            %% 新 owner 已经是 controller，无需操作
            ok;
        {ok, Controller} when Controller =/= Caller ->
            %% 调用者不是当前 controller
            {error, not_owner};
        {ok, _Controller} ->
            %% 调用者是当前 controller，执行转移
            {ok, Active} = gen_statem:call(Pid, get_active),
            %% 暂停主动模式
            case Active of
                true -> active(Pid, false);
                false -> ok
            end,
            %% 转发当前邮箱中的 utp 消息
            UTPSocket = {utp, element(2, gen_statem:call(Pid, get_socket_info)), Pid},
            Closed = forward_pending_messages(UTPSocket, NewOwner),
            case Closed of
                true ->
                    %% 连接已关闭，无需更新 controller
                    ok;
                false ->
                    %% 更新 controller 并恢复主动模式
                    gen_statem:call(Pid, {set_controller, Caller, NewOwner, Active}, infinity)
            end;
        {error, _Reason} = Error ->
            %% 连接未就绪 (idle/connecting/accepting/closing 状态)
            Error
    end.

%%==============================================================================
%% gen_statem 回调函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 回调模式
%%
%% 使用状态函数模式，并启用状态进入回调。
%% @end
%%------------------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

%%------------------------------------------------------------------------------
%% @doc 初始化回调
%%
%% 创建初始状态数据，监视父进程，进入 idle 状态。
%% @end
%%------------------------------------------------------------------------------
-spec init({pid(), gen_udp:socket()}) -> gen_statem:init_result(atom()).
init({Parent, Socket}) ->
    ParentMonitor = erlang:monitor(process, Parent),
    Data = #{
        parent => Parent,
        parent_monitor => ParentMonitor,
        socket => Socket,
        active => false
    },
    {ok, idle, Data}.

%%------------------------------------------------------------------------------
%% @doc 终止回调
%%
%% 清理所有资源：取消监视器和定时器。
%% @end
%%------------------------------------------------------------------------------
-spec terminate(term(), atom(), state_data()) -> ok.
terminate(_Reason, _State, Data) ->
    cleanup_resources(Data).

%%------------------------------------------------------------------------------
%% @doc 代码热更新回调
%% @end
%%------------------------------------------------------------------------------
-spec code_change(term(), atom(), state_data(), term()) ->
    {ok, atom(), state_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%==============================================================================
%% 状态: idle
%%
%% 初始状态，等待 connect 或 accept 命令。
%%==============================================================================

idle(enter, _OldState, Data) ->
    {keep_state, Data};

%% 处理 connect 命令 - 发起出站连接
idle({call, From}, {connect, Controller, Remote}, Data) ->
    handle_connect(From, Controller, Remote, Data);

%% 处理 accept 命令 - 接受入站连接
idle({call, From}, {accept, Controller, Remote, PacketInfo}, Data) ->
    handle_accept(From, Controller, Remote, PacketInfo, Data);

%% 其他调用在 idle 状态下返回错误
idle({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

%% 忽略 cast 消息
idle(cast, _Msg, _Data) ->
    keep_state_and_data;

%% 父进程崩溃 - 停止
idle(info, {'DOWN', MRef, process, Parent, _Reason},
     #{parent := Parent, parent_monitor := MRef}) ->
    {stop, normal};

%% 忽略其他 info 消息
idle(info, _Msg, _Data) ->
    keep_state_and_data.

%%==============================================================================
%% 状态: connecting
%%
%% 客户端发起连接，已发送 SYN，等待 SYN-ACK。
%%==============================================================================

connecting(enter, idle, _Data) ->
    keep_state_and_data;

%% 处理收到的数据包
connecting(cast, {packet, PacketInfo}, Data) ->
    handle_packet_connecting(PacketInfo, Data);

%% 超时检查
connecting(info, {timeout, TRef, tick}, #{tick_timer := TRef} = Data) ->
    handle_timeout_connecting(Data);

%% 父进程崩溃
connecting(info, {'DOWN', MRef, process, Parent, _Reason},
           #{parent := Parent, parent_monitor := MRef} = Data) ->
    handle_parent_down(Data);

%% 控制进程崩溃
connecting(info, {'DOWN', MRef, process, Controller, _Reason},
           #{controller := Controller, controller_monitor := MRef} = Data) ->
    handle_controller_down(Data);

%% 其他调用返回错误
connecting({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, connecting}}]};

%% 忽略其他 info 消息
connecting(info, _Msg, _Data) ->
    keep_state_and_data.

%%==============================================================================
%% 状态: accepting
%%
%% 服务端收到 SYN，已发送 STATE，等待数据确认连接。
%%==============================================================================

accepting(enter, idle, #{pcb := PCB} = Data) ->
    %% 检查是否已连接
    case aiutp_pcb:state(PCB) of
        ?CS_CONNECTED ->
            {next_state, connected, Data};
        _ ->
            keep_state_and_data
    end;

%% 处理收到的数据包
accepting(cast, {packet, PacketInfo}, Data) ->
    handle_packet_accepting(PacketInfo, Data);

%% 超时检查
accepting(info, {timeout, TRef, tick}, #{tick_timer := TRef} = Data) ->
    handle_timeout_accepting(Data);

%% 父进程崩溃
accepting(info, {'DOWN', MRef, process, Parent, _Reason},
          #{parent := Parent, parent_monitor := MRef} = Data) ->
    cleanup_resources(Data),
    {stop, normal};

%% 控制进程崩溃
accepting(info, {'DOWN', MRef, process, Controller, _Reason},
          #{controller := Controller, controller_monitor := MRef} = Data) ->
    handle_controller_down(Data);

%% 其他调用返回错误
accepting({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, accepting}}]};

%% 忽略其他 info 消息
accepting(info, _Msg, _Data) ->
    keep_state_and_data.

%%==============================================================================
%% 状态: connected
%%
%% 连接已建立，可双向传输数据。
%%==============================================================================

connected(enter, _OldState, Data) ->
    %% 进入 connected 状态时尝试读取并投递数据
    {keep_state, maybe_deliver_data(Data)};

%% 发送数据
connected({call, From}, {send, SendData}, #{pcb := PCB} = Data) ->
    case aiutp_pcb:write(SendData, PCB) of
        {{error, _} = Error, PCB1} ->
            {keep_state, Data#{pcb := PCB1}, [{reply, From, Error}]};
        {ok, PCB1} ->
            {keep_state, Data#{pcb := PCB1}, [{reply, From, ok}]}
    end;

%% 接收数据 (当前未实现阻塞式接收)
connected({call, From}, {recv, _Len}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_implemented}}]};

%% 设置主动模式
connected({call, From}, {active, Active}, Data) ->
    NewData = maybe_deliver_data(Data#{active := Active}),
    {keep_state, NewData, [{reply, From, ok}]};

%% 获取 controller
connected({call, From}, get_controller, #{controller := Controller}) ->
    {keep_state_and_data, [{reply, From, {ok, Controller}}]};

%% 获取 active 状态
connected({call, From}, get_active, #{active := Active}) ->
    {keep_state_and_data, [{reply, From, {ok, Active}}]};

%% 获取 socket 信息
connected({call, From}, get_socket_info, #{parent := Parent}) ->
    {keep_state_and_data, [{reply, From, {ok, Parent}}]};

%% 设置新的 controller
connected({call, From}, {set_controller, OldController, NewController, Active},
          #{controller := OldController, controller_monitor := OldMon} = Data) ->
    erlang:demonitor(OldMon, [flush]),
    NewMon = erlang:monitor(process, NewController),
    NewData = maybe_deliver_data(Data#{
        controller := NewController,
        controller_monitor := NewMon,
        active := Active
    }),
    {keep_state, NewData, [{reply, From, ok}]};

%% 关闭连接 (仅 controller 可以关闭)
connected({call, From}, {close, Controller}, #{controller := Controller} = Data) ->
    handle_close(From, Data);

connected({call, From}, {close, _NotOwner}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_owner}}]};

%% 处理收到的数据包
connected(cast, {packet, PacketInfo}, Data) ->
    handle_packet_connected(PacketInfo, Data);

%% 超时检查
connected(info, {timeout, TRef, tick}, #{tick_timer := TRef} = Data) ->
    handle_timeout_connected(Data);

%% 父进程崩溃
connected(info, {'DOWN', MRef, process, Parent, _Reason},
          #{parent := Parent, parent_monitor := MRef,
            controller := Controller, active := Active} = Data) ->
    cleanup_resources(Data),
    %% 通知 controller
    notify_controller_if_active(Controller, Active, Data, crash),
    {stop, normal};

%% 控制进程崩溃
connected(info, {'DOWN', MRef, process, Controller, _Reason},
          #{controller := Controller, controller_monitor := MRef} = Data) ->
    handle_controller_down(Data);

%% 忽略其他 info 消息
connected(info, _Msg, _Data) ->
    keep_state_and_data.

%%==============================================================================
%% 状态: closing
%%
%% 连接正在关闭。两种情况：
%% - PCB 已是 CS_DESTROY/CS_RESET: 立即清理并停止
%% - 否则: 继续处理数据包和超时，等待 FIN 被确认
%%==============================================================================

closing(enter, _OldState, #{pcb := PCB} = Data) ->
    case aiutp_pcb:state(PCB) of
        State when State =:= ?CS_DESTROY; State =:= ?CS_RESET ->
            %% PCB 已关闭，立即清理
            handle_closing_enter(Data);
        _ ->
            %% 等待 FIN 确认，保持定时器运行
            keep_state_and_data
    end;

%% 处理收到的数据包 (等待 FIN ACK)
closing(cast, {packet, PacketInfo}, Data) ->
    handle_packet_closing(PacketInfo, Data);

%% 超时检查 (等待 FIN 重传)
closing(info, {timeout, TRef, tick}, #{tick_timer := TRef} = Data) ->
    handle_timeout_closing(Data);

%% 所有调用返回 closed 错误
closing({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, closed}}]};

%% 父进程崩溃
closing(info, {'DOWN', MRef, process, Parent, _Reason},
        #{parent := Parent, parent_monitor := MRef} = Data) ->
    do_closing_cleanup(Data, crash);

%% 忽略其他 info 消息 (包括过期的定时器消息)
closing(info, _Msg, _Data) ->
    keep_state_and_data;

%% 忽略其他 cast 消息
closing(cast, _Msg, _Data) ->
    keep_state_and_data.

%%==============================================================================
%% 内部函数 - 连接建立
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 connect 命令
%%------------------------------------------------------------------------------
handle_connect(From, Controller, Remote,
               #{parent := Parent, socket := Socket} = Data) ->
    case allocate_conn_id(Parent, Remote) of
        {ok, ConnId} ->
            %% 创建 PCB 并发送 SYN
            PCB = aiutp_pcb:connect({Socket, Remote}, ConnId),
            ControllerMonitor = erlang:monitor(process, Controller),
            Timer = start_tick_timer(),
            NewData = Data#{
                remote => Remote,
                conn_id => ConnId,
                controller => Controller,
                controller_monitor => ControllerMonitor,
                pcb => PCB,
                tick_timer => Timer,
                blocker => From
            },
            {next_state, connecting, NewData};
        Error ->
            {stop_and_reply, normal, [{reply, From, Error}]}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 accept 命令
%%------------------------------------------------------------------------------
handle_accept(From, Controller, Remote, PacketInfo,
              #{parent := Parent, socket := Socket} = Data) ->
    {ConnId, PCB} = aiutp_pcb:accept({Socket, Remote}, PacketInfo),
    case aiutp_socket:register_channel(Parent, Remote, ConnId) of
        ok ->
            ControllerMonitor = erlang:monitor(process, Controller),
            Timer = start_tick_timer(),
            NewData = Data#{
                remote => Remote,
                conn_id => ConnId,
                controller => Controller,
                controller_monitor => ControllerMonitor,
                pcb => PCB,
                tick_timer => Timer
            },
            {next_state, accepting, NewData, [{reply, From, ok}]};
        Error ->
            {stop_and_reply, normal, [{reply, From, Error}]}
    end.

%%==============================================================================
%% 内部函数 - 数据包处理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 connecting 状态下收到的数据包
%%------------------------------------------------------------------------------
handle_packet_connecting({_Packet, _RecvTime} = PacketWithTS, #{pcb := PCB, blocker := Blocker} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(PacketWithTS, PCB),
    case aiutp_pcb:state(PCB1) of
        ?CS_CONNECTED ->
            %% 连接建立成功
            NewData = Data#{pcb := PCB1, blocker := undefined},
            {next_state, connected, NewData, [{reply, Blocker, ok}]};
        ?CS_RESET ->
            %% 连接被对端拒绝
            NewData = Data#{pcb := PCB1, blocker := undefined},
            {next_state, closing, NewData, [{reply, Blocker, {error, reset}}]};
        _ ->
            %% 继续等待
            {keep_state, Data#{pcb := PCB1}}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 accepting 状态下收到的数据包
%%------------------------------------------------------------------------------
handle_packet_accepting({_Packet, _RecvTime} = PacketWithTS, #{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(PacketWithTS, PCB),
    case aiutp_pcb:state(PCB1) of
        ?CS_CONNECTED ->
            {next_state, connected, Data#{pcb := PCB1}};
        ?CS_RESET ->
            {next_state, closing, Data#{pcb := PCB1}};
        _ ->
            {keep_state, Data#{pcb := PCB1}}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 connected 状态下收到的数据包
%%------------------------------------------------------------------------------
handle_packet_connected({_Packet, _RecvTime} = PacketWithTS, #{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(PacketWithTS, PCB),
    case aiutp_pcb:state(PCB1) of
        State when State =:= ?CS_DESTROY; State =:= ?CS_RESET ->
            {next_state, closing, Data#{pcb := PCB1}};
        _ ->
            NewData = maybe_deliver_data(Data#{pcb := PCB1}),
            {keep_state, NewData}
    end.

%%==============================================================================
%% 内部函数 - 超时处理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 connecting 状态下的超时
%%------------------------------------------------------------------------------
handle_timeout_connecting(#{pcb := PCB} = Data) ->
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
            Timer = start_tick_timer(),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 accepting 状态下的超时
%%------------------------------------------------------------------------------
handle_timeout_accepting(#{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:check_timeouts(PCB),
    case aiutp_pcb:closed(PCB1) of
        {closed, _Reason} ->
            {next_state, closing, Data#{pcb := PCB1, tick_timer := undefined}};
        not_closed ->
            Timer = start_tick_timer(),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 connected 状态下的超时
%%------------------------------------------------------------------------------
handle_timeout_connected(#{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:check_timeouts(PCB),
    case aiutp_pcb:closed(PCB1) of
        {closed, _Reason} ->
            {next_state, closing, Data#{pcb := PCB1, tick_timer := undefined}};
        not_closed ->
            Timer = start_tick_timer(),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 closing 状态下收到的数据包
%%
%% 继续处理数据包以接收 FIN 的 ACK。
%% 当 PCB 进入 CS_DESTROY/CS_RESET 时完成关闭。
%%------------------------------------------------------------------------------
handle_packet_closing({_Packet, _RecvTime} = PacketWithTS, #{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:process_incoming(PacketWithTS, PCB),
    case aiutp_pcb:state(PCB1) of
        State when State =:= ?CS_DESTROY; State =:= ?CS_RESET ->
            do_closing_cleanup(Data#{pcb := PCB1}, normal);
        _ ->
            {keep_state, Data#{pcb := PCB1}}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 closing 状态下的超时
%%
%% 继续处理超时以触发 FIN 重传。
%% 当 PCB 超时关闭时完成清理。
%%------------------------------------------------------------------------------
handle_timeout_closing(#{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:check_timeouts(PCB),
    case aiutp_pcb:closed(PCB1) of
        {closed, Reason} ->
            do_closing_cleanup(Data#{pcb := PCB1}, Reason);
        not_closed ->
            Timer = start_tick_timer(),
            {keep_state, Data#{pcb := PCB1, tick_timer := Timer}}
    end.

%%==============================================================================
%% 内部函数 - 进程崩溃处理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理父进程崩溃 (connecting 状态)
%%------------------------------------------------------------------------------
handle_parent_down(Data) ->
    cleanup_resources(Data),
    Actions = case maps:get(blocker, Data, undefined) of
        undefined -> [];
        Blocker -> [{reply, Blocker, {error, crash}}]
    end,
    {stop_and_reply, normal, Actions}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理控制进程崩溃
%%
%% 发送 RESET 通知对端并关闭连接
%%------------------------------------------------------------------------------
handle_controller_down(#{pcb := PCB} = Data) ->
    PCB1 = aiutp_pcb:close(PCB),
    NewData = Data#{
        pcb := PCB1,
        controller := undefined,
        controller_monitor := undefined,
        blocker => undefined  %% 使用 => 因为 accepting 状态下 blocker 键可能不存在
    },
    {next_state, closing, NewData}.

%%==============================================================================
%% 内部函数 - 连接关闭
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理 close 请求
%%
%% 两种情况：
%% - PCB 立即进入 CS_DESTROY/CS_RESET: 立即回复 ok 并进入 closing
%% - 否则: 等待 FIN 被确认，暂不回复调用者
%%------------------------------------------------------------------------------
handle_close(From, #{pcb := PCB, controller_monitor := ControllerMonitor} = Data) ->
    PCB1 = aiutp_pcb:close(PCB),
    case aiutp_pcb:state(PCB1) of
        State when State =:= ?CS_DESTROY; State =:= ?CS_RESET ->
            %% 立即关闭
            safe_demonitor(ControllerMonitor),
            NewData = Data#{
                pcb := PCB1,
                controller := undefined,
                controller_monitor := undefined,
                active := false
            },
            {next_state, closing, NewData, [{reply, From, ok}]};
        _ ->
            %% 等待 FIN 确认，不立即回复
            NewData = Data#{
                pcb := PCB1,
                blocker := From
            },
            {next_state, closing, NewData}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理进入 closing 状态（立即关闭情况）
%%
%% 仅当 PCB 已经是 CS_DESTROY/CS_RESET 时调用。
%% 执行资源清理并停止进程。
%%------------------------------------------------------------------------------
handle_closing_enter(Data) ->
    do_closing_cleanup(Data, normal).

%%------------------------------------------------------------------------------
%% @private
%% @doc 执行 closing 清理并停止进程
%%
%% 统一的清理逻辑，处理：
%% - 取消定时器
%% - 回复阻塞的调用者
%% - 通知 controller
%% - 释放连接 ID
%% - 停止进程
%%------------------------------------------------------------------------------
do_closing_cleanup(Data, Reason) ->
    %% 取消定时器
    cancel_tick_timer(maps:get(tick_timer, Data, undefined)),

    %% 获取连接信息
    Parent = maps:get(parent, Data),
    Remote = maps:get(remote, Data, undefined),
    ConnId = maps:get(conn_id, Data, undefined),
    Blocker = maps:get(blocker, Data, undefined),
    Controller = maps:get(controller, Data, undefined),
    Active = maps:get(active, Data, false),

    %% 回复阻塞的调用者
    Actions = case Blocker of
        undefined -> [];
        _ -> [{reply, Blocker, ok}]
    end,

    %% 通知 controller
    notify_controller_if_active(Controller, Active, Data, Reason),

    %% 释放连接 ID
    release_conn_id(Parent, Remote, ConnId),

    %% 根据是否有待回复的调用者选择正确的 gen_statem 返回格式
    case Actions of
        [] -> {stop, normal};
        _ -> {stop_and_reply, normal, Actions}
    end.

%%==============================================================================
%% 内部函数 - 数据投递
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 尝试读取并投递数据给 controller
%%
%% 仅在主动模式且有 controller 时投递数据。
%% 投递后自动切换为非主动模式 (once 语义)。
%%------------------------------------------------------------------------------
-spec maybe_deliver_data(state_data()) -> state_data().
maybe_deliver_data(#{controller := undefined} = Data) ->
    Data;
maybe_deliver_data(#{pcb := PCB, controller := Controller, active := true} = Data) ->
    case aiutp_pcb:read(PCB) of
        {undefined, PCB1} ->
            Data#{pcb := PCB1};
        {Payload, PCB1} ->
            UTPSocket = make_utp_socket(Data),
            Controller ! {utp_data, UTPSocket, Payload},
            %% once 语义: 发送后切换为非主动模式
            Data#{pcb := PCB1, active := false}
    end;
maybe_deliver_data(#{pcb := PCB} = Data) ->
    %% 非主动模式，刷新 PCB
    PCB1 = aiutp_pcb:flush(PCB),
    Data#{pcb := PCB1};
maybe_deliver_data(Data) ->
    %% 尚无 PCB
    Data.

%%==============================================================================
%% 内部函数 - 工具函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 分配连接 ID (带重试)
%%------------------------------------------------------------------------------
-spec allocate_conn_id(pid(), {inet:ip_address(), inet:port_number()}) ->
    {ok, non_neg_integer()} | {error, eagain}.
allocate_conn_id(Parent, Remote) ->
    allocate_conn_id(Parent, Remote, ?CONN_ID_RETRY_COUNT).

allocate_conn_id(_, _, 0) ->
    {error, eagain};
allocate_conn_id(Parent, Remote, N) ->
    ConnId = aiutp_util:bit16_random(),
    case aiutp_socket:register_channel(Parent, Remote, ConnId) of
        ok -> {ok, ConnId};
        _ -> allocate_conn_id(Parent, Remote, N - 1)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 释放连接 ID
%%------------------------------------------------------------------------------
-spec release_conn_id(pid(), term(), term()) -> ok.
release_conn_id(Parent, Remote, ConnId) when ConnId =/= undefined ->
    aiutp_socket:unregister_channel(Parent, Remote, ConnId);
release_conn_id(_, _, _) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc 启动 channel tick 定时器
%%
%% 相当于 libutp 应用层定时器，以 CHANNEL_TICK_INTERVAL (50ms) 频率触发。
%% check_timeouts 内部使用 TIMEOUT_CHECK_INTERVAL (500ms) 节流实际检查。
%%
%% 设计与 libutp 一致：
%% - 应用层高频调用（50ms）保证及时性
%% - 内部节流（500ms）控制实际检查频率
%%------------------------------------------------------------------------------
-spec start_tick_timer() -> reference().
start_tick_timer() ->
    erlang:start_timer(?CHANNEL_TICK_INTERVAL, self(), tick).

%%------------------------------------------------------------------------------
%% @private
%% @doc 取消定时器
%%------------------------------------------------------------------------------
-spec cancel_tick_timer(reference() | undefined) -> ok.
cancel_tick_timer(undefined) ->
    ok;
cancel_tick_timer(TRef) ->
    _ = erlang:cancel_timer(TRef),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc 安全取消监视器
%%------------------------------------------------------------------------------
-spec safe_demonitor(reference() | undefined) -> ok.
safe_demonitor(undefined) ->
    ok;
safe_demonitor(MRef) ->
    erlang:demonitor(MRef, [flush]),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc 清理所有资源
%%------------------------------------------------------------------------------
-spec cleanup_resources(state_data()) -> ok.
cleanup_resources(Data) ->
    safe_demonitor(maps:get(parent_monitor, Data, undefined)),
    safe_demonitor(maps:get(controller_monitor, Data, undefined)),
    cancel_tick_timer(maps:get(tick_timer, Data, undefined)),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc 如果主动模式，通知 controller 连接关闭
%%------------------------------------------------------------------------------
-spec notify_controller_if_active(pid() | undefined, boolean(), state_data(), atom()) -> ok.
notify_controller_if_active(Controller, true, Data, Reason) when Controller =/= undefined ->
    Controller ! {utp_closed, make_utp_socket(Data), Reason},
    ok;
notify_controller_if_active(_, _, _, _) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc 创建 UTP 套接字元组
%%------------------------------------------------------------------------------
-spec make_utp_socket(state_data()) -> {utp, pid(), pid()}.
make_utp_socket(#{parent := Parent}) ->
    {utp, Parent, self()}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 转发邮箱中的待处理 utp 消息给新 owner
%%
%% 返回 true 如果收到 utp_closed 消息。
%%------------------------------------------------------------------------------
-spec forward_pending_messages(term(), pid()) -> boolean().
forward_pending_messages(Socket, NewOwner) ->
    receive
        {utp_data, Socket, _} = Msg ->
            NewOwner ! Msg,
            forward_pending_messages(Socket, NewOwner);
        {utp_closed, Socket, _} = Msg ->
            NewOwner ! Msg,
            true
    after 0 ->
        false
    end.
