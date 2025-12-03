-module(aiutp).

-export([open/1,open/2]).
-export([connect/3,accept/1]).
-export([listen/1,listen/2,send/2,recv/2,close/1]).
-export([active/2,controlling_process/2]).

%% 类型定义
-type utp_socket() :: {utp, pid()}.
-type utp_connection() :: {utp, pid(), pid()}.
-type utp_option() :: {udp, [gen_udp:option()]} | {utp, list()}.
-type listen_option() :: {backlog, pos_integer()}.

-export_type([utp_socket/0, utp_connection/0]).

%% @doc 在指定端口打开一个 uTP 套接字
-spec open(inet:port_number()) -> {ok, utp_socket()} | {error, term()}.
open(Port) -> open(Port, []).

%% @doc 在指定端口打开一个带选项的 uTP 套接字
-spec open(inet:port_number(), [utp_option()]) -> {ok, utp_socket()} | {error, term()}.
open(Port, Options) ->
    case aiutp_sup:new(Port, Options) of
        {ok, SocketPid} -> {ok, {utp, SocketPid}};
        {error, _} = Error -> Error
    end.

%% @doc 连接到远程 uTP 端点
-spec connect(utp_socket(), inet:ip_address() | string(), inet:port_number()) ->
    {ok, utp_connection()} | {error, term()}.
connect({utp,UTPSocket},Address,Port)->
  aiutp_socket:connect(UTPSocket, Address, Port).

%% @doc 开始监听传入连接
-spec listen(utp_socket()) -> ok | {error, term()}.
listen({utp,UTPSocket})->
  aiutp_socket:listen(UTPSocket, []).

%% @doc 开始监听传入连接（带选项）
-spec listen(utp_socket(), [listen_option()]) -> ok | {error, term()}.
listen({utp,UTPSocket},Options)->
  aiutp_socket:listen(UTPSocket, Options).

%% @doc 接受传入连接
-spec accept(utp_socket()) -> {ok, utp_connection()} | {error, term()}.
accept({utp,UTPSocket})->
  aiutp_socket:accept(UTPSocket).

%% @doc 通过 uTP 连接发送数据
-spec send(utp_connection(), iodata()) -> ok | {error, term()}.
send({utp,_,Channel},Data)->
  aiutp_channel:send(Channel,Data).

%% @doc 从 uTP 连接接收数据
-spec recv(utp_connection(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv({utp,_,Channel},Len)->
  aiutp_channel:recv(Channel,Len).

%% @doc 设置连接的主动模式
-spec active(utp_connection(), boolean()) -> ok.
active({utp,_,Channel},V)->
  aiutp_channel:active(Channel, V).

%% @doc 将连接控制权转移给另一个进程
-spec controlling_process(utp_connection(), pid()) -> ok | {error, term()}.
controlling_process({utp,_,Channel},NewOwner)->
  aiutp_channel:controlling_process(Channel, NewOwner).

%% @doc 关闭 uTP 连接
-spec close(utp_connection()) -> ok | {error, term()}.
close({utp,_,Channel})->
  Caller = self(),
  aiutp_channel:close(Channel, Caller).
