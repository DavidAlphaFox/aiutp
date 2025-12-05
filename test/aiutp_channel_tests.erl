%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_channel module
%%
%% Tests the gen_statem based uTP channel implementation.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_channel_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% Test: Callback mode
%%==============================================================================

callback_mode_test() ->
    %% Verify callback mode includes state_functions and state_enter
    Mode = aiutp_channel:callback_mode(),
    ?assert(lists:member(state_functions, Mode)),
    ?assert(lists:member(state_enter, Mode)).

%%==============================================================================
%% Test: Channel supervisor
%%==============================================================================

channel_sup_init_test() ->
    %% Verify supervisor init returns valid child spec
    {ok, {SupFlags, [ChildSpec]}} = aiutp_channel_sup:init([]),

    %% Check supervisor flags
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags)),

    %% Check child spec
    ?assertEqual(aiutp_channel, maps:get(id, ChildSpec)),
    ?assertEqual(temporary, maps:get(restart, ChildSpec)),
    ?assertEqual(worker, maps:get(type, ChildSpec)),
    ?assert(lists:member(aiutp_channel, maps:get(modules, ChildSpec))).

%%==============================================================================
%% Test: State transitions design
%%==============================================================================

state_machine_states_test() ->
    %% Verify all state functions are exported
    Exports = aiutp_channel:module_info(exports),

    %% Check state functions exist (arity 3 for gen_statem state functions)
    ?assert(lists:member({idle, 3}, Exports)),
    ?assert(lists:member({connecting, 3}, Exports)),
    ?assert(lists:member({accepting, 3}, Exports)),
    ?assert(lists:member({connected, 3}, Exports)),
    ?assert(lists:member({closing, 3}, Exports)).

%%==============================================================================
%% Test: API functions exported
%%==============================================================================

api_functions_exported_test() ->
    Exports = aiutp_channel:module_info(exports),

    %% Core API
    ?assert(lists:member({start_link, 2}, Exports)),
    ?assert(lists:member({connect, 4}, Exports)),
    ?assert(lists:member({accept, 4}, Exports)),
    ?assert(lists:member({incoming, 2}, Exports)),

    %% Data transfer
    ?assert(lists:member({send, 2}, Exports)),
    ?assert(lists:member({recv, 3}, Exports)),

    %% Control
    ?assert(lists:member({active, 2}, Exports)),
    ?assert(lists:member({close, 2}, Exports)),
    ?assert(lists:member({controlling_process, 2}, Exports)).

%%==============================================================================
%% Test: gen_statem callbacks exported
%%==============================================================================

gen_statem_callbacks_exported_test() ->
    Exports = aiutp_channel:module_info(exports),

    ?assert(lists:member({callback_mode, 0}, Exports)),
    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({terminate, 3}, Exports)),
    ?assert(lists:member({code_change, 4}, Exports)).

%%==============================================================================
%% Test: Supervisor integration
%%==============================================================================

sup_child_spec_consistency_test() ->
    %% New structure: channel_sup is a child of socket_sup, not aiutp_sup
    %% Verify aiutp_socket_sup includes channel_sup
    {ok, {_SupFlags, ChildSpecs}} = aiutp_socket_sup:init([0, []]),
    ChildIds = [maps:get(id, Spec) || Spec <- ChildSpecs],

    ?assert(lists:member(aiutp_channel_sup, ChildIds)),
    ?assert(lists:member(aiutp_socket, ChildIds)),
    %% Worker sup has been removed
    ?assertNot(lists:member(aiutp_worker_sup, ChildIds)).
