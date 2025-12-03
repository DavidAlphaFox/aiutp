%%------------------------------------------------------------------------------
%% @doc uTP 测试客户端
%%
%% 一个简单的客户端示例，用于测试 aiutp 协议实现。
%%
%% == 使用方法 ==
%% ```
%% %% 在另一个 Erlang shell 中
%% 1> c(utp_client).
%% 2> utp_client:start().                        %% 连接本地服务器
%% 3> utp_client:start("192.168.1.100", 8080).  %% 连接指定地址
%% '''
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @end
%%------------------------------------------------------------------------------
-module(utp_client).

-export([start/0, start/2]).
-export([send/2, recv/1]).

%% 默认服务器地址
-define(DEFAULT_HOST, {127,0,0,1}).
-define(DEFAULT_PORT, 9000).

%%==============================================================================
%% 公开 API
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 连接到默认服务器 (127.0.0.1:9000)
%% @end
%%------------------------------------------------------------------------------
-spec start() -> {ok, {utp, pid(), pid()}} | {error, term()}.
start() ->
    start(?DEFAULT_HOST, ?DEFAULT_PORT).

%%------------------------------------------------------------------------------
%% @doc 连接到指定服务器
%%
%% @param Host 服务器地址（IP 元组或字符串）
%% @param Port 服务器端口
%% @returns {ok, Connection} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec start(inet:ip_address() | string(), inet:port_number()) ->
    {ok, {utp, pid(), pid()}} | {error, term()}.
start(Host, Port) ->
    %% 确保应用启动
    application:start(aiutp),

    io:format("[Client] Opening socket...~n"),
    %% 使用随机端口
    case aiutp:open(0) of
        {ok, Socket} ->
            io:format("[Client] Connecting to ~p:~p...~n", [Host, Port]),
            case aiutp:connect(Socket, Host, Port) of
                {ok, Connection} ->
                    io:format("[Client] Connected!~n"),
                    ok = aiutp:active(Connection, true),
                    {ok, Connection};
                {error, Reason} ->
                    io:format("[Client] Connect failed: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("[Client] Open failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc 发送数据到服务器
%%
%% @param Connection 连接句柄
%% @param Data 要发送的数据
%% @returns ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec send({utp, pid(), pid()}, iodata()) -> ok | {error, term()}.
send(Connection, Data) ->
    io:format("[Client] Sending: ~p~n", [Data]),
    aiutp:send(Connection, Data).

%%------------------------------------------------------------------------------
%% @doc 接收数据（阻塞等待）
%%
%% @param Connection 连接句柄
%% @returns {ok, Data} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec recv({utp, pid(), pid()}) -> {ok, binary()} | {error, term()}.
recv(Connection) ->
    receive
        {utp_data, Connection, Data} ->
            io:format("[Client] Received: ~p~n", [Data]),
            %% 重新激活接收
            ok = aiutp:active(Connection, true),
            {ok, Data};

        {utp_closed, Connection, Reason} ->
            io:format("[Client] Connection closed: ~p~n", [Reason]),
            {error, {closed, Reason}}
    after 5000 ->
        io:format("[Client] Receive timeout~n"),
        {error, timeout}
    end.
