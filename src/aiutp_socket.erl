%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc uTP 套接字服务器
%%%
%%% 本模块实现 uTP 套接字的核心管理功能，负责：
%%% - UDP 套接字的创建和管理
%%% - 连接（Channel）的注册和生命周期管理
%%% - 入站数据包的接收和分发
%%% - 监听和接受传入连接
%%%
%%% 架构：
%%% ```
%%%   aiutp_socket (gen_server)
%%%       │
%%%       ├── UDP Socket (gen_udp)
%%%       │       └── 接收/发送 UDP 数据包
%%%       │
%%%       ├── 连接注册表 (conns map)
%%%       │       └── {Remote, ConnId} => {Channel, Monitor}
%%%       │
%%%       └── Acceptor (aiutp_acceptor)
%%%               └── 处理传入的 SYN 请求
%%% ```
%%%
%%% @end
%%% Created :  6 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_socket).

-behaviour(gen_server).
-include("aiutp.hrl").

%%==============================================================================
%% API 导出
%%==============================================================================

-export([start_link/2]).
-export([connect/3, listen/2, accept/1]).
-export([register_channel/3, unregister_channel/3]).

%% 向后兼容别名
-export([add_conn/3, free_conn/3]).

%%==============================================================================
%% gen_server 回调导出
%%==============================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

%%==============================================================================
%% 宏定义
%%==============================================================================

-define(SERVER, ?MODULE).

%% 默认最大连接数
-define(DEFAULT_MAX_CONNS, 100).

%% UDP 缓冲区大小（约 6MB）
-define(UDP_SNDBUF_SIZE, 6553600).
-define(UDP_RECBUF_SIZE, 6553500).

%%==============================================================================
%% 类型定义
%%==============================================================================

%% 连接键：{远程地址, 连接ID}
-type conn_key() :: {{inet:ip_address(), inet:port_number()}, non_neg_integer()}.

%% 连接值：{Channel 进程, 监视器引用}
-type conn_value() :: {pid(), reference()}.

%% 服务器状态
%% - socket: UDP 套接字句柄
%% - conns: 连接注册表，按 {Remote, ConnId} 索引
%% - monitors: 监视器反向索引，用于快速查找要清理的连接
%% - conn_count: 当前连接数（缓存值，O(1) 访问）
%% - max_conns: 最大允许连接数
%% - acceptor: 接受器进程或 closed 表示未监听
%% - options: uTP 选项
-type state() :: #{
    socket := gen_udp:socket() | undefined,
    conns := #{conn_key() => conn_value()},
    monitors := #{reference() => conn_key()},
    conn_count := non_neg_integer(),
    max_conns := pos_integer(),
    acceptor := pid() | closed,
    options := list()
}.

%%==============================================================================
%% API 函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 启动 uTP 套接字服务器
%%
%% @param Port UDP 监听端口
%% @param Options 选项列表，支持：
%%   - {udp, UDPOptions}: 传递给 gen_udp:open 的选项
%%   - {utp, UTPOptions}: uTP 协议选项
%% @returns {ok, Pid} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec start_link(inet:port_number(), list()) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(Port, Options) ->
    gen_server:start_link(?MODULE, [Port, Options], []).

%%------------------------------------------------------------------------------
%% @doc 连接到远程 uTP 端点
%%
%% 创建一个新的 Channel 进程并发起连接。
%%
%% @param Socket uTP 套接字进程
%% @param Address 远程地址（IP 或主机名）
%% @param Port 远程端口
%% @returns {ok, {utp, Socket, Channel}} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec connect(pid(), inet:ip_address() | string(), inet:port_number()) ->
    {ok, {utp, pid(), pid()}} | {error, term()}.
connect(Socket, Address, Port) ->
    case gen_server:call(Socket, get_socket_info) of
        {ok, {UDPSocket, _Options}} ->
            case aiutp_channel_sup:new(Socket, UDPSocket) of
                {ok, Channel} ->
                    ResolvedAddr = aiutp_util:getaddr(Address),
                    Caller = self(),
                    case aiutp_channel:connect(Channel, Caller, ResolvedAddr, Port) of
                        ok ->
                            {ok, {utp, Socket, Channel}};
                        {error, _} = Error ->
                            %% 连接失败时 Channel 会自动停止
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc 开始监听传入连接
%%
%% 启动 Acceptor 进程来处理传入的 SYN 请求。
%%
%% @param Socket uTP 套接字进程
%% @param Options 监听选项（如 backlog）
%% @returns ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec listen(pid(), list()) -> ok | {error, term()}.
listen(Socket, Options) ->
    gen_server:call(Socket, {listen, Options}).

%%------------------------------------------------------------------------------
%% @doc 接受传入连接
%%
%% 阻塞等待新的连接请求。
%%
%% @param Socket uTP 套接字进程
%% @returns {ok, {utp, Socket, Channel}} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec accept(pid()) -> {ok, {utp, pid(), pid()}} | {error, term()}.
accept(Socket) ->
    gen_server:call(Socket, accept, infinity).

%%------------------------------------------------------------------------------
%% @doc 注册 Channel 进程
%%
%% 将 Channel 与 {Remote, ConnId} 关联，并监视 Channel 进程。
%% 当 Channel 进程终止时，会自动清理注册信息。
%%
%% @param Socket uTP 套接字进程
%% @param Remote 远程地址
%% @param ConnId 连接 ID
%% @returns ok | exists | overflow
%% @end
%%------------------------------------------------------------------------------
-spec register_channel(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) ->
    ok | exists | overflow.
register_channel(Socket, Remote, ConnId) ->
    Channel = self(),
    gen_server:call(Socket, {register_channel, Remote, ConnId, Channel}, infinity).

%%------------------------------------------------------------------------------
%% @doc 注销 Channel 进程
%%
%% 移除 Channel 的注册信息并取消监视。
%%
%% @param Socket uTP 套接字进程
%% @param Remote 远程地址
%% @param ConnId 连接 ID
%% @returns ok
%% @end
%%------------------------------------------------------------------------------
-spec unregister_channel(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) ->
    ok.
unregister_channel(Socket, Remote, ConnId) ->
    gen_server:call(Socket, {unregister_channel, Remote, ConnId}, infinity).

%% @doc register_channel/3 的向后兼容别名
%% @deprecated 请使用 register_channel/3
-spec add_conn(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) ->
    ok | exists | overflow.
add_conn(Socket, Remote, ConnId) ->
    register_channel(Socket, Remote, ConnId).

%% @doc unregister_channel/3 的向后兼容别名
%% @deprecated 请使用 unregister_channel/3
-spec free_conn(pid(), {inet:ip_address(), inet:port_number()}, non_neg_integer()) ->
    ok.
free_conn(Socket, Remote, ConnId) ->
    unregister_channel(Socket, Remote, ConnId).

%%==============================================================================
%% gen_server 回调实现
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 初始化服务器
%%
%% 打开 UDP 套接字并初始化状态。
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {ok, state()} | {stop, term()}.
init([Port, Options]) ->
    process_flag(trap_exit, true),

    %% 解析 UDP 选项，确保包含 binary 模式
    UDPOptions = proplists:get_value(udp, Options, []),
    UDPOptions1 = ensure_binary_mode(UDPOptions),

    %% 解析 uTP 选项
    UTPOptions = proplists:get_value(utp, Options, []),

    %% 打开 UDP 套接字
    case open_udp_socket(Port, UDPOptions1) of
        {ok, Socket} ->
            {ok, #{
                socket => Socket,
                conns => #{},
                monitors => #{},
                conn_count => 0,
                max_conns => ?DEFAULT_MAX_CONNS,
                acceptor => closed,
                options => UTPOptions
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理同步调用
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.

%% 获取套接字信息
handle_call(get_socket_info, _From, #{socket := Socket, options := Options} = State) ->
    {reply, {ok, {Socket, Options}}, State};

%% 向后兼容：旧的 socket 调用
handle_call(socket, _From, #{socket := Socket, options := Options} = State) ->
    {reply, {ok, {Socket, Options}}, State};

%% 注册 Channel
handle_call({register_channel, Remote, ConnId, Channel}, _From, State) ->
    {Result, NewState} = do_register_channel(Remote, ConnId, Channel, State),
    {reply, Result, NewState};

%% 向后兼容：旧的 add_conn 调用
handle_call({add_conn, Remote, ConnId, Channel}, _From, State) ->
    {Result, NewState} = do_register_channel(Remote, ConnId, Channel, State),
    {reply, Result, NewState};

%% 注销 Channel
handle_call({unregister_channel, Remote, ConnId}, _From, State) ->
    NewState = do_unregister_channel(Remote, ConnId, State),
    {reply, ok, NewState};

%% 向后兼容：旧的 free_conn 调用
handle_call({free_conn, Remote, ConnId}, _From, State) ->
    NewState = do_unregister_channel(Remote, ConnId, State),
    {reply, ok, NewState};

%% 开始监听
handle_call({listen, Options}, _From,
            #{socket := Socket, acceptor := closed, options := UTPOptions} = State) ->
    Parent = self(),
    {ok, Acceptor} = aiutp_acceptor:start_link(Parent, Socket, Options, UTPOptions),
    {reply, ok, State#{acceptor := Acceptor}};

handle_call({listen, _Options}, _From, State) ->
    %% 已经在监听状态
    {reply, {error, already_listening}, State};

%% 接受连接
handle_call(accept, _From, #{acceptor := closed} = State) ->
    {reply, {error, not_listening}, State};

handle_call(accept, From, #{acceptor := Acceptor} = State) ->
    %% 转发给 Acceptor，它会直接回复调用者
    aiutp_acceptor:accept(Acceptor, From),
    {noreply, State};

%% 未知请求
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理异步消息
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理系统消息
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(term(), state()) ->
    {noreply, state()} | {stop, term(), state()}.

%% 处理 UDP 数据包
handle_info({udp, Socket, IP, Port, Payload}, #{socket := Socket} = State) ->
    handle_udp_packet(IP, Port, Payload, State),
    %% 重新激活套接字以接收下一个包
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};

%% 处理 Channel 进程终止
handle_info({'DOWN', MRef, process, _Pid, _Reason},
            #{monitors := Monitors, conns := Conns, conn_count := ConnCount} = State) ->
    case maps:get(MRef, Monitors, undefined) of
        undefined ->
            %% 未知的监视器，忽略
            {noreply, State};
        Key ->
            %% 清理已终止 Channel 的注册信息
            NewState = State#{
                monitors := maps:remove(MRef, Monitors),
                conns := maps:remove(Key, Conns),
                conn_count := ConnCount - 1
            },
            {noreply, NewState}
    end;

%% 处理 Acceptor 进程退出
handle_info({'EXIT', Acceptor, Reason},
            #{acceptor := Acceptor, socket := Socket} = State) ->
    %% Acceptor 退出，关闭 Socket 并停止服务器
    close_socket_if_open(Socket),
    {stop, Reason, State#{socket := undefined, acceptor := closed}};

%% 忽略其他消息
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 服务器终止时清理资源
%% @end
%%------------------------------------------------------------------------------
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{socket := Socket}) ->
    close_socket_if_open(Socket),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc 热代码升级
%% @end
%%------------------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 格式化状态用于调试
%% @end
%%------------------------------------------------------------------------------
-spec format_status(map()) -> map().
format_status(Status) ->
    Status.

%%==============================================================================
%% 内部函数 - UDP 套接字管理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 打开并配置 UDP 套接字
%% @end
%%------------------------------------------------------------------------------
-spec open_udp_socket(inet:port_number(), list()) ->
    {ok, gen_udp:socket()} | {error, term()}.
open_udp_socket(Port, UDPOptions) ->
    FullOptions = [
        {sndbuf, ?UDP_SNDBUF_SIZE},
        {recbuf, ?UDP_RECBUF_SIZE}
        | UDPOptions
    ],
    case gen_udp:open(Port, FullOptions) of
        {ok, Socket} ->
            %% 配置套接字：单次激活模式，防止消息队列溢出
            ok = inet:setopts(Socket, [
                {active, once},
                {high_msgq_watermark, ?UDP_RECBUF_SIZE}
            ]),
            {ok, Socket};
        {error, _} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 确保 UDP 选项包含 binary 模式
%% @end
%%------------------------------------------------------------------------------
-spec ensure_binary_mode(list()) -> list().
ensure_binary_mode(Options) ->
    case proplists:is_defined(binary, Options) of
        true -> Options;
        false -> [binary | Options]
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 安全关闭套接字
%% @end
%%------------------------------------------------------------------------------
-spec close_socket_if_open(gen_udp:socket() | undefined) -> ok.
close_socket_if_open(undefined) -> ok;
close_socket_if_open(Socket) ->
    gen_udp:close(Socket),
    ok.

%%==============================================================================
%% 内部函数 - 连接注册管理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 注册 Channel 到连接表
%%
%% 检查是否存在重复或超过最大连接数限制。
%% @end
%%------------------------------------------------------------------------------
-spec do_register_channel(
    {inet:ip_address(), inet:port_number()},
    non_neg_integer(),
    pid(),
    state()
) -> {ok | exists | overflow, state()}.
do_register_channel(Remote, ConnId, Channel,
                    #{conns := Conns, monitors := Monitors,
                      conn_count := ConnCount, max_conns := MaxConns} = State) ->
    Key = {Remote, ConnId},
    case maps:is_key(Key, Conns) of
        true ->
            %% 连接 ID 已存在
            {exists, State};
        false when ConnCount >= MaxConns ->
            %% 超过最大连接数
            {overflow, State};
        false ->
            %% 注册新连接并监视 Channel 进程
            Monitor = erlang:monitor(process, Channel),
            NewState = State#{
                conns := maps:put(Key, {Channel, Monitor}, Conns),
                monitors := maps:put(Monitor, Key, Monitors),
                conn_count := ConnCount + 1
            },
            {ok, NewState}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 从连接表注销 Channel
%% @end
%%------------------------------------------------------------------------------
-spec do_unregister_channel(
    {inet:ip_address(), inet:port_number()},
    non_neg_integer(),
    state()
) -> state().
do_unregister_channel(Remote, ConnId,
                      #{conns := Conns, monitors := Monitors,
                        conn_count := ConnCount} = State) ->
    Key = {Remote, ConnId},
    case maps:get(Key, Conns, undefined) of
        undefined ->
            %% 连接不存在，无需操作
            State;
        {_Channel, Monitor} ->
            %% 取消监视并移除注册信息
            erlang:demonitor(Monitor, [flush]),
            State#{
                monitors := maps:remove(Monitor, Monitors),
                conns := maps:remove(Key, Conns),
                conn_count := ConnCount - 1
            }
    end.

%%==============================================================================
%% 内部函数 - 数据包处理
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理接收到的 UDP 数据包
%%
%% 解码数据包并分发到相应的 Channel 或 Acceptor。
%% @end
%%------------------------------------------------------------------------------
-spec handle_udp_packet(inet:ip_address(), inet:port_number(), binary(), state()) -> ok.
handle_udp_packet(IP, Port, Payload, State) ->
    case aiutp_packet:decode(Payload) of
        {ok, Packet} ->
            dispatch_packet({IP, Port}, Packet, State);
        {error, Reason} ->
            %% BEP-29：记录解码错误用于调试，但不崩溃
            logger:debug("uTP packet decode failed from ~p:~p, reason: ~p, size: ~p",
                        [IP, Port, Reason, byte_size(Payload)]),
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 分发数据包到对应的处理者
%%
%% 分发逻辑：
%% 1. 查找连接注册表，存在则转发给 Channel
%% 2. 不存在时：
%%    - SYN 包且正在监听：转发给 Acceptor
%%    - RESET 包：忽略（无需响应）
%%    - 其他：发送 RESET 通知对端
%% @end
%%------------------------------------------------------------------------------
-spec dispatch_packet(
    {inet:ip_address(), inet:port_number()},
    #aiutp_packet{},
    state()
) -> ok.
dispatch_packet(Remote, #aiutp_packet{conn_id = ConnId, type = PktType, seq_nr = SeqNR} = Packet,
                #{socket := Socket, conns := Conns, acceptor := Acceptor,
                  conn_count := ConnCount, max_conns := MaxConns}) ->
    Key = {Remote, ConnId},
    RecvTime = aiutp_util:microsecond(),

    case maps:get(Key, Conns, undefined) of
        {Channel, _Monitor} ->
            %% 已知连接，转发给 Channel
            aiutp_channel:incoming(Channel, {Packet, RecvTime});
        undefined ->
            %% 未知连接，根据包类型处理
            handle_unknown_connection(PktType, Remote, ConnId, SeqNR, Packet, RecvTime,
                                     Socket, Acceptor, ConnCount, MaxConns)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc 处理来自未知连接的数据包
%% @end
%%------------------------------------------------------------------------------
-spec handle_unknown_connection(
    non_neg_integer(),
    {inet:ip_address(), inet:port_number()},
    non_neg_integer(),
    non_neg_integer(),
    #aiutp_packet{},
    non_neg_integer(),
    gen_udp:socket(),
    pid() | closed,
    non_neg_integer(),
    pos_integer()
) -> ok.

%% SYN 包且正在监听
handle_unknown_connection(?ST_SYN, Remote, ConnId, SeqNR, Packet, RecvTime,
                          Socket, Acceptor, ConnCount, MaxConns)
  when Acceptor /= closed ->
    if
        ConnCount >= MaxConns ->
            %% 连接数已满，发送 RESET 拒绝
            send_reset(Socket, Remote, ConnId, SeqNR);
        true ->
            %% 转发给 Acceptor 处理
            aiutp_acceptor:incoming(Acceptor, {?ST_SYN, Remote, {Packet, RecvTime}})
    end;

%% RESET 包：静默忽略
handle_unknown_connection(?ST_RESET, _Remote, _ConnId, _SeqNR, _Packet, _RecvTime,
                          _Socket, _Acceptor, _ConnCount, _MaxConns) ->
    ok;

%% 其他包：发送 RESET 通知对端连接不存在
handle_unknown_connection(_PktType, Remote, ConnId, SeqNR, _Packet, _RecvTime,
                          Socket, _Acceptor, _ConnCount, _MaxConns) ->
    send_reset(Socket, Remote, ConnId, SeqNR).

%%------------------------------------------------------------------------------
%% @private
%% @doc 向远程端点发送 RESET 包
%%
%% 用于通知对端连接不存在或被拒绝。
%% @end
%%------------------------------------------------------------------------------
-spec send_reset(
    gen_udp:socket(),
    {inet:ip_address(), inet:port_number()},
    non_neg_integer(),
    non_neg_integer()
) -> ok | {error, term()}.
send_reset(Socket, Remote, ConnId, AckNR) ->
    Packet = aiutp_packet:reset(ConnId, AckNR),
    Bin = aiutp_packet:encode(Packet),
    gen_udp:send(Socket, Remote, Bin).
