%%------------------------------------------------------------------------------
%% @doc aiutp_channel recv 功能单元测试
%%
%% 测试阻塞式 recv 的各种场景：
%% - 立即返回（有数据）
%% - 阻塞等待（无数据）
%% - 超时处理
%% - EOF 处理
%% - active 模式互斥
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_channel_recv_tests).
-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% 测试夹具
%%==============================================================================

%% 启动测试环境
setup() ->
    application:ensure_all_started(aiutp),
    ok.

%% 清理测试环境
cleanup(_) ->
    application:stop(aiutp),
    ok.

%%==============================================================================
%% 测试用例
%%==============================================================================

%% recv API 基础测试
recv_api_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"recv/3 存在", fun recv_api_exists/0}
     ]}.

recv_api_exists() ->
    %% 验证 recv/3 函数已导出
    Exports = aiutp:module_info(exports),
    ?assert(lists:member({recv, 2}, Exports)),
    ?assert(lists:member({recv, 3}, Exports)).

%% recv 与 active 模式互斥测试
recv_active_mode_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {timeout, 30, [
          {"active 模式下 recv 返回错误", fun recv_in_active_mode_returns_error/0}
         ]}
     end}.

recv_in_active_mode_returns_error() ->
    %% 这个测试需要真实连接，暂时跳过
    %% 在集成测试中验证
    ok.

%% recv 超时测试 - 使用 mock
recv_timeout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {timeout, 10, [
          {"recv 超时机制测试", fun recv_timeout_mechanism/0}
         ]}
     end}.

recv_timeout_mechanism() ->
    %% 验证 start_recv_timer 函数行为
    %% infinity 返回 undefined
    ?assertEqual(undefined, aiutp_channel:start_recv_timer_for_test(infinity)),
    %% 正整数返回 timer ref
    TimerRef = aiutp_channel:start_recv_timer_for_test(1000),
    ?assert(is_reference(TimerRef)),
    %% 清理定时器
    erlang:cancel_timer(TimerRef),
    ok.

%%==============================================================================
%% 集成测试 - 需要真实连接
%%==============================================================================

%% 辅助函数：获取 uTP socket 绑定的端口
get_utp_port({utp, SocketPid}) ->
    {ok, {UDPSocket, _, _}} = gen_server:call(SocketPid, get_socket_info),
    {ok, Port} = inet:port(UDPSocket),
    Port.

%% 完整的 recv 流程测试
recv_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {timeout, 60, [
          {"echo recv 测试", fun test_echo_recv/0},
          {"recv 超时测试", fun test_recv_timeout/0},
          {"recv active 互斥测试", fun test_recv_active_mutual_exclusion/0},
          {"recv busy 测试", fun test_recv_busy/0}
         ]}
     end}.

test_echo_recv() ->
    %% 启动服务端
    {ok, ServerSocket} = aiutp:open(0),
    ServerPort = get_utp_port(ServerSocket),
    ok = aiutp:listen(ServerSocket),

    %% 启动客户端
    {ok, ClientSocket} = aiutp:open(0),

    %% 服务端接受连接的进程
    Parent = self(),
    spawn_link(fun() ->
        {ok, ServerConn} = aiutp:accept(ServerSocket),
        %% 使用 active 模式接收第一个消息，确认连接已建立
        ok = aiutp:active(ServerConn, true),
        receive
            {utp_data, ServerConn, Data} ->
                %% 切换到非 active 模式
                ok = aiutp:active(ServerConn, false),
                %% 回显数据
                ok = aiutp:send(ServerConn, Data),
                Parent ! {server_done, Data}
        after 5000 ->
            Parent ! {server_error, timeout}
        end
    end),

    %% 等待服务端准备
    timer:sleep(100),

    %% 客户端连接
    {ok, ClientConn} = aiutp:connect(ClientSocket, "127.0.0.1", ServerPort),

    %% 发送数据
    TestData = <<"Hello, recv!">>,
    ok = aiutp:send(ClientConn, TestData),

    %% 设置为非 active 模式并接收回显
    ok = aiutp:active(ClientConn, false),
    RecvResult = aiutp:recv(ClientConn, 0, 5000),

    %% 验证
    ?assertMatch({ok, _}, RecvResult),
    {ok, RecvData} = RecvResult,
    ?assertEqual(TestData, RecvData),

    %% 等待服务端完成
    receive
        {server_done, ServerRecvData} ->
            ?assertEqual(TestData, ServerRecvData);
        {server_error, Reason} ->
            ?assert({server_error, Reason})
    after 5000 ->
        ?assert(false)
    end,

    %% 不调用 close，让进程自动清理
    ok.

test_recv_timeout() ->
    %% 启动服务端（不发送任何数据）
    {ok, ServerSocket} = aiutp:open(0),
    ServerPort = get_utp_port(ServerSocket),
    ok = aiutp:listen(ServerSocket),

    %% 服务端接受连接但不发送数据
    Parent = self(),
    spawn_link(fun() ->
        {ok, ServerConn} = aiutp:accept(ServerSocket),
        ok = aiutp:active(ServerConn, false),
        %% 等待一段时间后关闭
        timer:sleep(2000),
        aiutp:close(ServerConn),
        Parent ! server_done
    end),

    timer:sleep(100),

    %% 客户端连接
    {ok, ClientSocket} = aiutp:open(0),
    {ok, ClientConn} = aiutp:connect(ClientSocket, "127.0.0.1", ServerPort),

    %% 设置短超时
    ok = aiutp:active(ClientConn, false),
    Result = aiutp:recv(ClientConn, 0, 500),

    %% 验证超时
    ?assertEqual({error, timeout}, Result),

    %% 清理
    aiutp:close(ClientConn),
    receive server_done -> ok after 3000 -> ok end,
    ok.

test_recv_active_mutual_exclusion() ->
    %% 启动服务端
    {ok, ServerSocket} = aiutp:open(0),
    ServerPort = get_utp_port(ServerSocket),
    ok = aiutp:listen(ServerSocket),

    %% 服务端
    Parent = self(),
    spawn_link(fun() ->
        {ok, ServerConn} = aiutp:accept(ServerSocket),
        timer:sleep(1000),
        aiutp:close(ServerConn),
        Parent ! server_done
    end),

    timer:sleep(100),

    %% 客户端连接
    {ok, ClientSocket} = aiutp:open(0),
    {ok, ClientConn} = aiutp:connect(ClientSocket, "127.0.0.1", ServerPort),

    %% 设置为 active 模式
    ok = aiutp:active(ClientConn, true),

    %% 在 active 模式下调用 recv 应该返回错误
    Result = aiutp:recv(ClientConn, 0, 1000),
    ?assertEqual({error, active}, Result),

    %% 切换到非 active 模式后应该可以 recv
    ok = aiutp:active(ClientConn, false),
    %% 这里会超时，因为没有数据
    TimeoutResult = aiutp:recv(ClientConn, 0, 100),
    ?assertEqual({error, timeout}, TimeoutResult),

    %% 清理
    aiutp:close(ClientConn),
    receive server_done -> ok after 2000 -> ok end,
    ok.

test_recv_busy() ->
    %% 启动服务端
    {ok, ServerSocket} = aiutp:open(0),
    ServerPort = get_utp_port(ServerSocket),
    ok = aiutp:listen(ServerSocket),

    %% 服务端 - 保持连接打开更长时间
    Parent = self(),
    spawn_link(fun() ->
        {ok, ServerConn} = aiutp:accept(ServerSocket),
        %% 等待足够长时间让测试完成
        timer:sleep(10000),
        aiutp:close(ServerConn),
        Parent ! server_done
    end),

    timer:sleep(100),

    %% 客户端连接
    {ok, ClientSocket} = aiutp:open(0),
    {ok, ClientConn} = aiutp:connect(ClientSocket, "127.0.0.1", ServerPort),
    ok = aiutp:active(ClientConn, false),

    %% 在一个进程中启动短时间 recv
    Caller = self(),
    RecvPid = spawn_link(fun() ->
        Caller ! started,
        Result = aiutp:recv(ClientConn, 0, 2000),
        Caller ! {recv_result, Result}
    end),

    %% 等待 recv 开始
    receive started -> ok after 1000 -> ?assert(false) end,
    timer:sleep(50),

    %% 第二个 recv 应该返回 busy
    BusyResult = aiutp:recv(ClientConn, 0, 100),
    ?assertEqual({error, busy}, BusyResult),

    %% 等待第一个 recv 超时
    receive
        {recv_result, {error, timeout}} -> ok;
        {recv_result, {error, closed}} -> ok;  %% 如果连接关闭也是可接受的
        {recv_result, Other} -> ?assertMatch({error, _}, Other)
    after 6000 ->
        exit(RecvPid, kill),
        ?assert(false)
    end,

    %% 不调用 close，让进程自动清理
    ok.
