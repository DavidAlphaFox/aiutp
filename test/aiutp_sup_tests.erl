%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_sup module
%%
%% Tests supervisor configuration and child specifications.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Test: Supervisor child IDs are correctly spelled
%%==============================================================================

child_ids_spelling_test() ->
    %% This test ensures the supervisor child IDs match the actual module names
    {ok, {_SupFlags, ChildSpecs}} = aiutp_sup:init([]),

    %% Extract child IDs
    ChildIds = [maps:get(id, Spec) || Spec <- ChildSpecs],

    %% Verify expected child IDs exist
    ?assert(lists:member(aiutp_socket_sup, ChildIds)),
    ?assert(lists:member(aiutp_channel_sup, ChildIds)),

    %% Verify old worker_sup is removed
    ?assertNot(lists:member(aiutp_worker_sup, ChildIds)).

child_specs_modules_match_test() ->
    %% Verify that the 'id' and 'modules' fields are consistent
    {ok, {_SupFlags, ChildSpecs}} = aiutp_sup:init([]),

    lists:foreach(fun(Spec) ->
        Id = maps:get(id, Spec),
        Modules = maps:get(modules, Spec),
        %% The ID should match the module name in modules list
        ?assert(lists:member(Id, Modules))
    end, ChildSpecs).

child_specs_start_mfa_test() ->
    %% Verify that start MFA references correct modules
    {ok, {_SupFlags, ChildSpecs}} = aiutp_sup:init([]),

    lists:foreach(fun(Spec) ->
        {Module, _Func, _Args} = maps:get(start, Spec),
        Modules = maps:get(modules, Spec),
        %% The start module should be in the modules list
        ?assert(lists:member(Module, Modules))
    end, ChildSpecs).

supervisor_flags_test() ->
    %% Verify supervisor flags are reasonable
    {ok, {SupFlags, _ChildSpecs}} = aiutp_sup:init([]),

    %% rest_for_one: if socket_sup crashes, channel_sup restarts too
    %% but if channel_sup crashes, socket_sup continues working
    ?assertEqual(rest_for_one, maps:get(strategy, SupFlags)),
    ?assert(maps:get(intensity, SupFlags) >= 1),
    ?assert(maps:get(period, SupFlags) >= 1).
