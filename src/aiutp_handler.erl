%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2023, David Gao
%%% @doc
%%% 负责管理connection的进程
%%% 负责解析包，负责聚合包
%%% 对于reset和ack的包可以进行整合性发送
%%% @end
%%% Created :  29 April 2023 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_handler).

-behaviour(gen_server).
-include("aiutp.hrl").
%% API
-export([start_link/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2
]).
-export([
    accept/2
]).
-define(SERVER, ?MODULE).

-record(state, {
    socket :: pid(),
    session :: pid(),
    acceptors,
    monitors,
    syns,
    syn_len,
    max_syn_len,
    options
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec accept(pid(),Acceptor :: {pid(), term()}) -> ignore.
%% acceptor是一个gen_server:call的FROM
accept(Pid,Acceptor)->
  gen_server:cast(Pid,{accept,Acceptor}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),pid()) ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.
start_link(Socket,Session) ->
    gen_server:start_link(?MODULE, [Socket,Session], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: term()}
    | {ok, State :: term(), Timeout :: timeout()}
    | {ok, State :: term(), hibernate}
    | {stop, Reason :: term()}
    | ignore.
init([Socket, Session]) ->
    {ok, #state{socket = Socket,session = Session}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
    {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()}
    | {reply, Reply :: term(), NewState :: term(), hibernate}
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: term(), NewState :: term()}.

handle_cast({accept,Acceptor},
            #state{acceptors = Acceptors, syn_len = SynLen} = State)->
  if
    SynLen > 0 -> accept_incoming(Acceptor,State);
    true -> {noreply,State#state{ acceptors = queue:in(Acceptor, Acceptors) }}
  end;
handle_cast({?ST_SYN,Remote,Packet},State)->
  pair_incoming(Remote,Packet,State);
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: term()
) -> any().
terminate(_Reason, _State) -> ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: term(),
    Extra :: term()
) ->
    {ok, NewState :: term()}
    | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(
    Opt :: normal | terminate,
    Status :: list()
) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%% worker只要知道handler就可以了
accept_incoming(Acceptor,#state{acceptors = Acceptors, syn_len = 0 } = State)->
  {noreply,State#state{acceptors = queue:in(Acceptor,Acceptors) }};
accept_incoming({Caller,_} = Acceptor,
                #state{ syns = Syns,syn_len = SynLen,socket = Socket} = State)->
  Parent = self(),
  {ok,Worker} = aiutp_worker_sup:new(Parent,Socket),
  {{value,Req},Syns0} = queue:out(Syns),
  {Remote,{SYN,_} = P} = Req,
  case aiutp_worker:accept(Worker, Caller,Remote, P) of
    ok ->
      gen_server:reply(Acceptor, {ok,{utp,Parent,Worker}}),
      {noreply,State#state{syns = Syns0, syn_len = SynLen - 1}};
    _ ->
      Packet = aiutp_packet:reset(SYN#aiutp_packet.conn_id,SYN#aiutp_packet.seq_nr),
      Bin = aiutp_packet:encode(Packet),
      gen_udp:send(Socket,Remote,Bin),
      accept_incoming(Acceptor,State#state{syns = Syns0,syn_len = SynLen -1 })
  end.
pair_incoming(Remote,{SYN,_},
              #state{socket = Socket,
                     syn_len = SynLen,
                     max_syn_len = MaxSynLen} = State) when SynLen >= MaxSynLen ->
  Packet = aiutp_packet:reset(SYN#aiutp_packet.conn_id,SYN#aiutp_packet.seq_nr),
  Bin = aiutp_packet:encode(Packet),
  gen_udp:send(Socket,Remote,Bin),
  {noreply,State};

pair_incoming(Remote,{Packet,_} = P,
              #state{acceptors = Acceptors,syns = Syns} = State) ->
  Syns0 =
    queue:filter(
      fun({Remote0,{SYN,_}})->
          if (Remote ==  Remote0) andalso
             (SYN#aiutp_packet.conn_id == Packet#aiutp_packet.conn_id) -> false;
            true -> true
          end
      end, Syns),
  Syns1 = queue:in({Remote,P},Syns0),
  Empty = queue:is_empty(Acceptors),
  if
    Empty == true -> {noreply,State#state{syns = Syns1,syn_len = queue:len(Syns1)}};
    true ->
      {{value,Acceptor},Acceptors0} = queue:out(Acceptors),
      accept_incoming(Acceptor,State#state{syns = Syns1,
                                           syn_len = queue:len(Syns1),
                                           acceptors = Acceptors0})
  end.
