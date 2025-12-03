-module(aiutp).

-export([open/1,open/2]).
-export([connect/3,accept/1]).
-export([listen/1,listen/2,send/2,recv/2,close/1]).
-export([active/2,controlling_process/2]).

%% Type definitions
-type utp_socket() :: {utp, pid()}.
-type utp_connection() :: {utp, pid(), pid()}.
-type utp_option() :: {udp, [gen_udp:option()]} | {utp, list()}.
-type listen_option() :: {backlog, pos_integer()}.

-export_type([utp_socket/0, utp_connection/0]).

%% @doc Open a uTP socket on the specified port
-spec open(inet:port_number()) -> {ok, utp_socket()} | {error, term()}.
open(Port) -> aiutp_socket_sup:open(Port,[]).

%% @doc Open a uTP socket on the specified port with options
-spec open(inet:port_number(), [utp_option()]) -> {ok, utp_socket()} | {error, term()}.
open(Port,Options) -> aiutp_socket_sup:open(Port,Options).

%% @doc Connect to a remote uTP endpoint
-spec connect(utp_socket(), inet:ip_address() | string(), inet:port_number()) ->
    {ok, utp_connection()} | {error, term()}.
connect({utp,UTPSocket},Address,Port)->
  aiutp_socket:connect(UTPSocket, Address, Port).

%% @doc Start listening for incoming connections
-spec listen(utp_socket()) -> ok | {error, term()}.
listen({utp,UTPSocket})->
  aiutp_socket:listen(UTPSocket, []).

%% @doc Start listening for incoming connections with options
-spec listen(utp_socket(), [listen_option()]) -> ok | {error, term()}.
listen({utp,UTPSocket},Options)->
  aiutp_socket:listen(UTPSocket, Options).

%% @doc Accept an incoming connection
-spec accept(utp_socket()) -> {ok, utp_connection()} | {error, term()}.
accept({utp,UTPSocket})->
  aiutp_socket:accept(UTPSocket).

%% @doc Send data over a uTP connection
-spec send(utp_connection(), iodata()) -> ok | {error, term()}.
send({utp,_,Channel},Data)->
  aiutp_channel:send(Channel,Data).

%% @doc Receive data from a uTP connection
-spec recv(utp_connection(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv({utp,_,Channel},Len)->
  aiutp_channel:recv(Channel,Len).

%% @doc Set active mode for a connection
-spec active(utp_connection(), boolean()) -> ok.
active({utp,_,Channel},V)->
  aiutp_channel:active(Channel, V).

%% @doc Transfer control of a connection to another process
-spec controlling_process(utp_connection(), pid()) -> ok | {error, term()}.
controlling_process({utp,_,Channel},NewOwner)->
  aiutp_channel:controlling_process(Channel, NewOwner).

%% @doc Close a uTP connection
-spec close(utp_connection()) -> ok | {error, term()}.
close({utp,_,Channel})->
  Caller = self(),
  aiutp_channel:close(Channel, Caller).
