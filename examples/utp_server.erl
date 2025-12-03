%%------------------------------------------------------------------------------
%% @doc uTP 测试服务器
%%
%% 一个简单的 Echo 服务器示例，用于测试 aiutp 协议实现。
%%
%% == 使用方法 ==
%% ```
%% %% 在 Erlang shell 中
%% 1> c(utp_server).
%% 2> utp_server:start().     %% 启动服务器（默认端口 9000）
%% 3> utp_server:start(8080). %% 指定端口启动
%% '''
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @end
%%------------------------------------------------------------------------------
-module(utp_server).

-export([start/0, start/1]).

%% 默认监听端口
-define(DEFAULT_PORT, 9000).

%%==============================================================================
%% 公开 API
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 使用默认端口启动服务器
%% @end
%%------------------------------------------------------------------------------
-spec start() -> {ok, {utp, pid(), pid()}} | {error, term()}.
start() ->
    start(?DEFAULT_PORT).

%%------------------------------------------------------------------------------
%% @doc 在指定端口启动服务器
%%
%% @param Port 监听端口号
%% @returns {ok, Connection} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec start(inet:port_number()) -> {ok, {utp, pid(), pid()}} | {error, term()}.
start(Port) ->
    %% 确保应用启动
    application:start(aiutp),

    io:format("[Server] Opening socket on port ~p...~n", [Port]),
    case aiutp:open(Port) of
        {ok, Socket} ->
            io:format("[Server] Listening...~n"),
            ok = aiutp:listen(Socket),

            io:format("[Server] Waiting for connection...~n"),
            case aiutp:accept(Socket) of
                {ok, Connection} ->
                    io:format("[Server] Connection accepted!~n"),
                    ok = aiutp:active(Connection, true),
                    %% 启动 Echo 处理循环
                    spawn(fun() -> echo_loop(Connection) end),
                    {ok, Connection};
                {error, Reason} ->
                    io:format("[Server] Accept failed: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("[Server] Open failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%==============================================================================
%% 内部函数
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Echo 处理循环
%% @end
%%------------------------------------------------------------------------------
echo_loop(Connection) ->
    receive
        {utp_data, Connection, Data} ->
            io:format("[Server] Received: ~p~n", [Data]),
            %% Echo 回去
            ok = aiutp:send(Connection, Data),
            io:format("[Server] Echoed back~n"),
            %% 继续接收
            ok = aiutp:active(Connection, true),
            echo_loop(Connection);

        {utp_closed, Connection, Reason} ->
            io:format("[Server] Connection closed: ~p~n", [Reason]),
            ok;

        Other ->
            io:format("[Server] Unknown message: ~p~n", [Other]),
            echo_loop(Connection)
    end.
