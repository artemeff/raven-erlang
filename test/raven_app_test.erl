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
            {cfg,"https://app.getsentry.com","PUBLIC_KEY","PRIVATE_KEY","1",inet6,"2.0"} = Config,
            ok = application:stop(raven_erlang)
        end},
        {"Loads a default value (inet) for ipfamily if not specified",
        fun() ->
            ok = application:unset_env(raven_erlang, ipfamily),
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            Config = raven:get_config(),
            {cfg,"https://app.getsentry.com","PUBLIC_KEY","PRIVATE_KEY","1",inet,"2.0"} = Config,
            ok = application:stop(raven_erlang)
        end},
        {"Loads sentry version 7 configuration",
        fun() ->
            ok = application:set_env(raven_erlang, sentry_ver, 7.0),
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            Config = raven:get_config(),
            {cfg,"https://app.getsentry.com","PUBLIC_KEY","PRIVATE_KEY","1",inet,"7.0"} = Config,
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


request_test_() ->
    [
        {"Sends headers of 2.0 version",
        fun() ->
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            ok = application:set_env(raven_erlang, sentry_ver, 2.0),

            Cfg      = raven:get_config(),
            Document = [],
            UA       = raven:user_agent(),
            Body     = raven:encode_body(Document),

            {
                "https://app.getsentry.com/api/store/",
                [
                    {"X-Sentry-Auth",
                    ["Sentry sentry_version=", "2.0",
                     ",sentry_client=", UA,
                     ",sentry_timestamp=", _TS,
                     ",sentry_key=", "PUBLIC_KEY"]},
                    {"User-Agent", UA}
                ],
                "application/octet-stream",
                Body
            } = raven:request(Document, Cfg),

            ok = application:stop(raven_erlang)
        end},
        {"Sends headers of 7.0 version",
        fun() ->
            {StartAppStatus, _} = application:ensure_all_started(raven_erlang),
            ?assertEqual(ok, StartAppStatus),
            ok = application:set_env(raven_erlang, sentry_ver, 7.0),

            Cfg      = raven:get_config(),
            Document = [],
            UA       = raven:user_agent(),
            Body     = raven:encode_body(Document),

            {
                "https://app.getsentry.com/api/1/store/",
                [
                    {"X-Sentry-Auth",
                    ["Sentry sentry_version=", "7.0",
                     ",sentry_client=", UA,
                     ",sentry_timestamp=", _,
                     ",sentry_key=", "PUBLIC_KEY",
                     ",sentry_secret=", "PRIVATE_KEY"]},
                    {"User-Agent", UA}
                ],
                "application/octet-stream",
                Body
            } = raven:request(Document, Cfg),
            ok = application:stop(raven_erlang)
        end}
    ].
