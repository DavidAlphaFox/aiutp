%%%-----------------------------------------------------------------------------
%%% @doc aiutp - Erlang uTP (Micro Transport Protocol) implementation
%%%
%%% This module provides the public API for the aiutp library, an implementation
%%% of the uTP protocol as specified in BEP-29.
%%%
%%% == Overview ==
%%%
%%% uTP is a transport protocol layered on top of UDP that provides reliable,
%%% ordered data delivery with congestion control using the LEDBAT algorithm.
%%% It is designed to minimize impact on network latency while maintaining
%%% high throughput.
%%%
%%% == Usage ==
%%%
%%% === Server Example ===
%%% ```
%%% %% Start listening on port 9000
%%% {ok, Socket} = aiutp:open(9000),
%%% ok = aiutp:listen(Socket),
%%%
%%% %% Accept incoming connection
%%% {ok, Conn} = aiutp:accept(Socket),
%%%
%%% %% Receive and send data
%%% {ok, Data} = aiutp:recv(Conn, 0),
%%% ok = aiutp:send(Conn, <<"Response">>),
%%%
%%% %% Close connection
%%% aiutp:close(Conn).
%%% '''
%%%
%%% === Client Example ===
%%% ```
%%% %% Open socket and connect
%%% {ok, Socket} = aiutp:open(0),
%%% {ok, Conn} = aiutp:connect(Socket, "127.0.0.1", 9000),
%%%
%%% %% Send and receive data
%%% ok = aiutp:send(Conn, <<"Hello">>),
%%% {ok, Response} = aiutp:recv(Conn, 0),
%%%
%%% %% Close connection
%%% aiutp:close(Conn).
%%% '''
%%%
%%% == Active Mode ==
%%%
%%% Connections can operate in active or passive mode:
%%% <ul>
%%%   <li>`active(Conn, true)' - Data is delivered as messages:
%%%       `{utp_data, Conn, Data}'</li>
%%%   <li>`active(Conn, false)' - Data must be retrieved with `recv/2'</li>
%%% </ul>
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(aiutp).

-export([open/1,open/2]).
-export([connect/3,accept/1]).
-export([listen/1,listen/2,send/2,recv/2,close/1]).
-export([active/2,controlling_process/2]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type utp_socket() :: {utp, pid()}.
%% A uTP socket handle. Created by {@link open/1} or {@link open/2}.

-type utp_connection() :: {utp, pid(), pid()}.
%% A uTP connection handle. Created by {@link connect/3} or {@link accept/1}.

-type utp_option() :: {udp, [gen_udp:option()]} | {utp, list()}.
%% Socket options. `{udp, Options}' passes options to the underlying UDP socket.

-type listen_option() :: {backlog, pos_integer()}.
%% Listen options. `{backlog, N}' sets the maximum pending connections queue size.

-export_type([utp_socket/0, utp_connection/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Opens a uTP socket on the specified port.
%%
%% Creates a new uTP socket bound to the given port. Use port 0 to let the
%% system choose an available port.
%%
%% @param Port The port number to bind to (0 for automatic assignment)
%% @returns `{ok, Socket}' on success, `{error, Reason}' on failure
%% @equiv open(Port, [])
%% @see open/2
-spec open(inet:port_number()) -> {ok, utp_socket()} | {error, term()}.
open(Port) -> open(Port, []).

%% @doc Opens a uTP socket on the specified port with options.
%%
%% Creates a new uTP socket bound to the given port with additional options.
%%
%% Options:
%% <ul>
%%   <li>`{udp, UdpOpts}' - Options passed to the underlying UDP socket</li>
%%   <li>`{utp, UtpOpts}' - uTP-specific options (reserved for future use)</li>
%% </ul>
%%
%% @param Port The port number to bind to
%% @param Options List of socket options
%% @returns `{ok, Socket}' on success, `{error, Reason}' on failure
%% @see open/1
-spec open(inet:port_number(), [utp_option()]) -> {ok, utp_socket()} | {error, term()}.
open(Port, Options) ->
    case aiutp_sup:new(Port, Options) of
        {ok, SocketPid} -> {ok, {utp, SocketPid}};
        {error, _} = Error -> Error
    end.

%% @doc Connects to a remote uTP endpoint.
%%
%% Initiates a connection to the specified address and port. This function
%% blocks until the connection is established or times out.
%%
%% @param Socket The local uTP socket
%% @param Address The remote address (IP tuple or hostname string)
%% @param Port The remote port number
%% @returns `{ok, Connection}' on success, `{error, Reason}' on failure
%% @see accept/1
-spec connect(utp_socket(), inet:ip_address() | string(), inet:port_number()) ->
    {ok, utp_connection()} | {error, term()}.
connect({utp,UTPSocket},Address,Port)->
  aiutp_socket:connect(UTPSocket, Address, Port).

%% @doc Starts listening for incoming connections.
%%
%% Puts the socket into listening mode so it can accept incoming connections.
%%
%% @param Socket The uTP socket to listen on
%% @returns `ok' on success, `{error, Reason}' on failure
%% @equiv listen(Socket, [])
%% @see listen/2
%% @see accept/1
-spec listen(utp_socket()) -> ok | {error, term()}.
listen({utp,UTPSocket})->
  aiutp_socket:listen(UTPSocket, []).

%% @doc Starts listening for incoming connections with options.
%%
%% Puts the socket into listening mode with additional options.
%%
%% Options:
%% <ul>
%%   <li>`{backlog, N}' - Maximum number of pending connections (default: 5)</li>
%% </ul>
%%
%% @param Socket The uTP socket to listen on
%% @param Options List of listen options
%% @returns `ok' on success, `{error, Reason}' on failure
%% @see listen/1
%% @see accept/1
-spec listen(utp_socket(), [listen_option()]) -> ok | {error, term()}.
listen({utp,UTPSocket},Options)->
  aiutp_socket:listen(UTPSocket, Options).

%% @doc Accepts an incoming connection.
%%
%% Blocks until an incoming connection is available on the listening socket.
%% The socket must be in listening mode (see {@link listen/1}).
%%
%% @param Socket The listening uTP socket
%% @returns `{ok, Connection}' on success, `{error, Reason}' on failure
%% @see listen/1
%% @see connect/3
-spec accept(utp_socket()) -> {ok, utp_connection()} | {error, term()}.
accept({utp,UTPSocket})->
  aiutp_socket:accept(UTPSocket).

%% @doc Sends data over a uTP connection.
%%
%% Queues data for transmission. Returns immediately after queuing; actual
%% transmission is handled asynchronously with flow control and retransmission.
%%
%% @param Connection The uTP connection
%% @param Data The data to send (binary or iolist)
%% @returns `ok' on success, `{error, Reason}' on failure
%% @see recv/2
-spec send(utp_connection(), iodata()) -> ok | {error, term()}.
send({utp,_,Channel},Data)->
  aiutp_channel:send(Channel,Data).

%% @doc Receives data from a uTP connection.
%%
%% Blocks until data is available. In active mode, data is delivered as
%% messages instead.
%%
%% @param Connection The uTP connection
%% @param Length Number of bytes to receive (0 = any available data)
%% @returns `{ok, Data}' on success, `{error, Reason}' on failure
%% @see send/2
%% @see active/2
-spec recv(utp_connection(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv({utp,_,Channel},Len)->
  aiutp_channel:recv(Channel,Len).

%% @doc Sets the connection's active mode.
%%
%% In active mode (`true'), incoming data is delivered as messages to the
%% controlling process:
%% <ul>
%%   <li>`{utp_data, Connection, Data}' - Received data</li>
%%   <li>`{utp_closed, Connection, Reason}' - Connection closed</li>
%% </ul>
%%
%% In passive mode (`false'), data must be retrieved using {@link recv/2}.
%%
%% @param Connection The uTP connection
%% @param Active `true' for active mode, `false' for passive mode
%% @returns `ok'
%% @see recv/2
-spec active(utp_connection(), boolean()) -> ok.
active({utp,_,Channel},V)->
  aiutp_channel:active(Channel, V).

%% @doc Transfers control of a connection to another process.
%%
%% The new owner will receive active mode messages and must handle
%% connection lifecycle events.
%%
%% @param Connection The uTP connection
%% @param NewOwner The PID of the new controlling process
%% @returns `ok' on success, `{error, Reason}' on failure
-spec controlling_process(utp_connection(), pid()) -> ok | {error, term()}.
controlling_process({utp,_,Channel},NewOwner)->
  aiutp_channel:controlling_process(Channel, NewOwner).

%% @doc Closes a uTP connection.
%%
%% Initiates a graceful connection teardown. Pending data is flushed before
%% the connection is closed.
%%
%% @param Connection The uTP connection to close
%% @returns `ok' on success, `{error, Reason}' on failure
-spec close(utp_connection()) -> ok | {error, term()}.
close({utp,_,Channel})->
  Caller = self(),
  aiutp_channel:close(Channel, Caller).
