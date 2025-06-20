%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2025, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2025 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aiutp_channel).

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([state_idle/3,state_syn_sent/3]).

-define(SERVER, ?MODULE).

-record(pcb,{sender_id :: integer(),
             %% 发送时候的id
             receiver_id :: integer(),
             %%接收时候对应的id
             seq_nr :: integer(),
             %% 下一个包将使用的序列号
             ack_nr :: integer(),
             %% 对对端的包进行确认的序列号
             %% 一般收取连续包的时候，对端包的seq_nr - 自己的ack_nr为1
             ibuffer :: array:array(),
             %% 收到但尚未发送ack的包
             send_window :: array:array(),
             %% 发送了但是尚未确认的包
             current_window :: integer(),
             %% 传输中还没有ACK的字节数
             remote_window :: integer(),
             %% 对端的窗口大小，
             duplicate_ack_count :: integer(),
             %%收到了多少次重复的ack包
             last_acked:: integer(),
             %% 对端ack我们的最后序列值
             last_dropped :: integer()
             %% 最后一次从ibuffer中删除的序列号

            }). %utp内部状态

-record(data, {parent :: pid(),
               %%和这个utp关联的业务进程
               parent_monitor :: reference(), 
               %%监控业务进程
               socket :: port(), 
               %%udp socket
               remote :: tuple(),
               %% 远程的地址和端口
               pcb}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
        {ok, Pid :: pid()} |
        ignore |
        {error, Error :: term()}.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
        gen_statem:init_result(atom()).
init([]) ->
  process_flag(trap_exit, true),
  {ok, state_name, #data{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function 
%% with the name of the current state (StateName) 
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec state_idle('enter',
                 OldState :: atom(),
                 Data :: term()) ->
        gen_statem:state_enter_result('state_idle');
                (gen_statem:event_type(),
                 Msg :: term(),
                 Data :: term()) ->
        gen_statem:event_handler_result(atom()).
state_idle({call,Caller}, _Msg, Data) ->
  {next_state, state_idle, Data, [{reply,Caller,ok}]}.
-spec state_syn_sent('enter',
                 OldState :: atom(),
                 Data :: term()) ->
        gen_statem:state_enter_result('state_syn_sent');
                (gen_statem:event_type(),
                 Msg :: term(),
                 Data :: term()) ->
        gen_statem:event_handler_result(atom()).
state_syn_sent({call,Caller}, _Msg, Data) ->
  {next_state, state_syn_sent, Data, [{reply,Caller,ok}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
        any().
terminate(_Reason, _State, _Data) ->
  void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
        {ok, NewState :: term(), NewData :: term()} |
        (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
