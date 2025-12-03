%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_acceptor).

-behaviour(gen_server).

-include("aiutp.hrl").
%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-export([accept/2,incoming/2]).

-define(SERVER, ?MODULE).
-define(BACKLOG, 128).

%% 状态是一个 map，包含以下键：
%% - parent: pid() - 父套接字进程
%% - socket: gen_udp:socket() - UDP 套接字
%% - channel_sup: pid() - channel 监督者进程
%% - acceptors: queue:queue({pid(), term()}) - 等待的接受器队列
%% - syns: queue:queue(syn_request()) - 待处理的 SYN 请求队列
%% - syn_len: non_neg_integer() - 当前 SYN 队列长度
%% - max_syn_len: pos_integer() - 最大 SYN 队列长度（backlog）
%% - options: list() - UTP 选项
-type syn_request() :: {{inet:ip_address(), inet:port_number()},
                        {#aiutp_packet{}, non_neg_integer()}}.
-type state() :: #{
    parent := pid(),
    socket := gen_udp:socket(),
    channel_sup := pid(),
    acceptors := queue:queue({pid(), term()}),
    syns := queue:queue(syn_request()),
    syn_len := non_neg_integer(),
    max_syn_len := pos_integer(),
    options := list()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 注册等待传入连接的接受器
-spec accept(pid(), {pid(), term()}) -> ok.
accept(Pid,Acceptor)->
  gen_server:cast(Pid,{accept,Acceptor}).

%% @doc 处理传入的 SYN 请求
-spec incoming(pid(), term()) -> ok.
incoming(Pid,Req)->
  gen_server:cast(Pid,Req).
%%--------------------------------------------------------------------
%% @doc 启动服务器
%%
%% @param Parent 父套接字进程
%% @param Socket UDP 套接字
%% @param ChannelSup channel 监督者 pid
%% @param Options 监听选项
%% @param UTPOptions UTP 协议选项
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), port(), pid(), list(), list()) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(Parent, Socket, ChannelSup, Options, UTPOptions) ->
  gen_server:start_link(?MODULE, [Parent, Socket, ChannelSup, Options, UTPOptions], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 初始化服务器
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
        {ok, State :: term(), Timeout :: timeout()} |
        {ok, State :: term(), hibernate} |
        {stop, Reason :: term()} |
        ignore.
init([Parent, Socket, ChannelSup, Options, UTPOptions]) ->
  Backlog =
    case Options of
      undefined -> ?BACKLOG;
      _ -> proplists:get_value(backlog, Options, ?BACKLOG)
    end,
  {ok, #{parent => Parent,
         socket => Socket,
         channel_sup => ChannelSup,
         acceptors => queue:new(),
         syns => queue:new(),
         syn_len => 0,
         max_syn_len => Backlog,
         options => UTPOptions}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理 call 消息
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
        {reply, Reply :: term(), NewState :: term()} |
        {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
        {reply, Reply :: term(), NewState :: term(), hibernate} |
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
        {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理 cast 消息
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast({accept,Acceptor},
            #{acceptors := Acceptors, syn_len := SynLen} = State)->
  if
    SynLen > 0 -> accept_incoming(Acceptor, State);
    true -> {noreply, State#{acceptors := queue:in(Acceptor, Acceptors)}}
  end;
handle_cast({?ST_SYN,Remote,Packet},State)->
  pair_incoming(Remote,Packet,State);
handle_cast(_, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理所有非 call/cast 消息
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(timeout, #{acceptors := Acceptors} = State)->
  {{value,Acceptor}, Acceptors0} = queue:out(Acceptors),
  accept_incoming(Acceptor, State#{acceptors := Acceptors0});
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 当 gen_server 即将终止时调用此函数。它应该与 Module:init/1 相反，
%% 执行任何必要的清理工作。当它返回时，gen_server 以 Reason 终止。
%% 返回值被忽略。
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 代码变更时转换进程状态
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
        {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 当从 sys:get_status/1,2 返回或出现在终止错误日志中时，
%% 调用此函数来更改 gen_server 状态的形式和外观。
%% @end
%%--------------------------------------------------------------------
-spec format_status(Status :: map()) -> Status :: map().
format_status(Status) ->
  Status.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private 从待处理的 SYN 请求中接受传入连接
-spec accept_incoming({pid(), term()}, state()) ->
    {noreply, state()} | {noreply, state(), timeout()}.
accept_incoming(Acceptor, #{acceptors := Acceptors, syn_len := 0} = State)->
  {noreply, State#{acceptors := queue:in(Acceptor, Acceptors)}};
accept_incoming({Caller,_} = Acceptor,
                #{syns := Syns, syn_len := SynLen,
                  parent := Parent, socket := Socket,
                  channel_sup := ChannelSup} = State)->
  {ok, Channel} = aiutp_channel_sup:new(ChannelSup, Parent, Socket),
  {{value, Req}, Syns0} = queue:out(Syns),
  {Remote, {SYN, _} = P} = Req,
  case aiutp_channel:accept(Channel, Caller, Remote, P) of
    ok ->
      gen_server:reply(Acceptor, {ok, {utp, Parent, Channel}}),
      {noreply, State#{syns := Syns0, syn_len := SynLen - 1}};
    _ ->
      Packet = aiutp_packet:reset(SYN#aiutp_packet.conn_id, SYN#aiutp_packet.seq_nr),
      Bin = aiutp_packet:encode(Packet),
      gen_udp:send(Socket, Remote, Bin),
      accept_incoming(Acceptor, State#{syns := Syns0, syn_len := SynLen - 1})
  end.

%% @private 将传入的 SYN 与等待的接受器配对或加入队列
-spec pair_incoming({inet:ip_address(), inet:port_number()},
                    {#aiutp_packet{}, integer()}, state()) ->
    {noreply, state()} | {noreply, state(), timeout()}.
pair_incoming(Remote, {SYN, _},
              #{socket := Socket,
                syn_len := SynLen,
                max_syn_len := MaxSynLen} = State) when SynLen >= MaxSynLen ->
  Packet = aiutp_packet:reset(SYN#aiutp_packet.conn_id, SYN#aiutp_packet.seq_nr),
  Bin = aiutp_packet:encode(Packet),
  gen_udp:send(Socket, Remote, Bin),
  {noreply, State};

pair_incoming(Remote, {Packet, _} = P,
              #{acceptors := Acceptors, syns := Syns} = State) ->
  Syns0 =
    queue:filter(
      fun({Remote0, {SYN, _}})->
          if (Remote == Remote0) andalso
             (SYN#aiutp_packet.conn_id == Packet#aiutp_packet.conn_id) -> false;
            true -> true
          end
      end, Syns),
  Syns1 = queue:in({Remote, P}, Syns0),
  Empty = queue:is_empty(Acceptors),
  if
    Empty == true -> {noreply, State#{syns := Syns1, syn_len := queue:len(Syns1)}};
    true ->
      {{value, Acceptor}, Acceptors0} = queue:out(Acceptors),
      accept_incoming(Acceptor, State#{syns := Syns1,
                                       syn_len := queue:len(Syns1),
                                       acceptors := Acceptors0})
  end.
