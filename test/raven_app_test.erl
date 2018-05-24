%% @doc Tests for raven_app.

-module(raven_app_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

load_configuration_test_() ->
    [
        {"Loads configuration from application env",
        fun() ->
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            Config = raven:get_config(),
            {cfg,"https://app.getsentry.com","PUBLIC_KEY","PRIVATE_KEY","1",inet6} = Config,
            ok = application:stop(raven_erlang)
        end},
        {"Loads a default value (inet) for ipfamily if not specified",
        fun() ->
            ok = application:unset_env(raven_erlang, ipfamily),
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            Config = raven:get_config(),
            {cfg,"https://app.getsentry.com","PUBLIC_KEY","PRIVATE_KEY","1",inet} = Config,
            ok = application:stop(raven_erlang)
        end}
    ].

capture_event_test_() ->
    [
        {"We can start the app and capture a simple event",
        fun() ->
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            ok = raven:capture("Test event", []),
            ok = application:stop(raven_erlang)
        end}
    ].
