%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(ai_utp_acceptor).

-behaviour(gen_server).

-include("ai_utp.hrl").
%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([accept/2,incoming/2]).

-define(SERVER, ?MODULE).
-define(BACKLOG, 128).
-record(state, {
                parent,
                socket,
                acceptors,
                monitors,
                syns,
                syn_len,
                max_syn_len
               }).

%%%===================================================================
%%% API
%%%===================================================================
accept(Pid,Acceptor)->
  gen_server:cast(Pid,{accept,Acceptor}).
incoming(Pid,Req)->
  gen_server:cast(Pid,Req).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),port(),list()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Parent,Socket,Options) ->
  gen_server:start_link(?MODULE, [Parent,Socket,Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
        {ok, State :: term(), Timeout :: timeout()} |
        {ok, State :: term(), hibernate} |
        {stop, Reason :: term()} |
        ignore.
init([Parent,Socket,Options]) ->
  Backlog =
    case Options of
      undefined -> ?BACKLOG;
      _ ->proplists:get_value(backlog, Options,?BACKLOG)
    end,
  {ok, #state{
          parent = Parent,
          socket = Socket,
          acceptors = queue:new(),
          syns = queue:new(),
          syn_len = 0,
          max_syn_len = Backlog
         }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
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
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast({accept,Acceptor},
            #state{acceptors = Acceptors, syn_len = SynLen} = State)->
  if
    SynLen > 0 -> accept_incoming(Acceptor,State);
    true -> {noreply,State#state{ acceptors = queue:in(Acceptor, Acceptors) }}
  end;
handle_cast({syn,Remote,Packet,Timing},State)->
  pair_incoming(Remote,Packet,Timing,State);
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(timeout,#state{acceptors = Acceptors} = State)->
  {{value,Acceptor},Acceptors0} = queue:out(Acceptors),
  accept_incoming(Acceptor, State#state{acceptors = Acceptors0});
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
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
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
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept_incoming(Acceptor,#state{acceptors = Acceptors, syn_len = 0 } = State)->
  {noreply,State#state{acceptors = queue:in(Acceptor,Acceptors) }};
accept_incoming({Caller,_} = Acceptor,
                #state{acceptors = Acceptors, syns = Syns,syn_len = SynLen,
                      parent = Parent,socket = Socket } = State)->
  {ok,Worker} = ai_utp_worker_sup:new(Parent, Socket),
  {{value,Req},Syns0} = queue:out(Syns),
  {Remote,Packet,Timing} = Req,
  case ai_utp_worker:accept(Worker, Caller,Remote, Packet,Timing) of
    ok ->
      gen_server:reply(Acceptor, {ok,{utp,Parent,Worker}}),
      {noreply,State#state{syns = Syns0, syn_len = SynLen - 1}};
    {error,exist}->
      Exts = proplists:get_value(ext_bits, Packet#utp_packet.extension),
      ResetPacket = ai_utp_protocol:make_reset_packet(
                      Packet#utp_packet.conn_id,Packet#utp_packet.seq_no),
      ai_utp_util:send(Socket, Remote,
                       ResetPacket#utp_packet{extension = [{ext_bits,Exts}]}, 0),
      accept_incoming(Acceptor,State#state{syns = Syns0,syn_len = SynLen -1 });
    Error ->
      logger:error(Error),
      {noreply,State#state{
                 acceptors = queue:in(Acceptor,Acceptors),
                 syns = queue:in(Req,Syns0)},1000}
  end.
pair_incoming(Remote,Packet,_,
              #state{socket = Socket,
                     syn_len = SynLen,
                     max_syn_len = MaxSynLen} = State) when SynLen >= MaxSynLen ->
  Exts = proplists:get_value(ext_bits, Packet#utp_packet.extension),
  ResetPacket = ai_utp_protocol:make_reset_packet(
                  Packet#utp_packet.conn_id,Packet#utp_packet.seq_no),
  ai_utp_util:send(Socket, Remote,
                   ResetPacket#utp_packet{extension = [{ext_bits,Exts}]}, 0),
  {noreply,State};

pair_incoming(Remote,Packet,Timing,
              #state{acceptors = Acceptors,
                     syns = Syns} = State) ->
  Syns0 =
    queue:filter(
      fun({Remote0,SYN,_})->
          if
            (Remote ==  Remote0) andalso
            (SYN#utp_packet.conn_id == Packet#utp_packet.conn_id) ->
              false;
            true -> true
          end
      end, Syns),
  Syns1 = queue:in({Remote,Packet,Timing},Syns0),
  Empty = queue:is_empty(Acceptors),
  if
    Empty == true ->
      {noreply,State#state{syns = Syns1,syn_len = queue:len(Syns1)}};
    true ->
      {{value,Acceptor},Acceptors0} = queue:out(Acceptors),
      accept_incoming(Acceptor,
                      State#state{syns = Syns1,
                                  syn_len = queue:len(Syns1),
                                  acceptors = Acceptors0
                                 })
  end.
