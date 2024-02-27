-module(raven).
-include("raven.hrl").
-export([
    start/0,
    stop/0,
    capture/2,
    user_agent/0
]).
-ifdef(TEST).
-export([
    get_config/0
]).
-endif.

-define(SENTRY_VERSION, "2.0").
-define(JSONE_OPTS, [native_utf8, {object_key_type, scalar}]).

-record(cfg, {
    uri :: string(),
    public_key :: string(),
    private_key :: string(),
    project :: string(),
    ipfamily :: atom()
}).

-type cfg_rec() :: #cfg{}.

-spec start() -> ok | {error, term()}.
start() ->
    application:ensure_all_started(?APP).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?APP).

-spec capture(string() | binary(), [parameter()]) -> ok.
-type parameter() ::
    {stacktrace, [stackframe()]} |
    {exception, {exit | error | throw, term()}} |
    {atom(), binary() | integer()}.
-type stackframe() ::
    {module(), atom(), non_neg_integer() | [term()]} |
    {module(), atom(), non_neg_integer() | [term()], [{atom(), term()}]}.
capture(Message, Params) when is_list(Message) ->
    capture(unicode:characters_to_binary(Message), Params);
capture(Message, Params0) ->
    Cfg = get_config(),
    Params1 = [{tags, get_tags()} | Params0],
    Params2 = maybe_append_release(Params1),
    Document = {[
        {event_id, event_id_i()},
        {project, unicode:characters_to_binary(Cfg#cfg.project)},
        {platform, erlang},
        {server_name, node()},
        {timestamp, timestamp_i()},
        {message, term_to_json_i(Message)} |
        lists:map(fun
            ({stacktrace, Value}) ->
                {'sentry.interfaces.Stacktrace', {[
                    {frames,lists:reverse([frame_to_json_i(Frame) || Frame <- Value])}
                ]}};
            ({exception, {Type, Value}}) ->
                {'sentry.interfaces.Exception', {[
                    {type, Type},
                    {value, term_to_json_i(Value)}
                ]}};
            ({tags, Tags}) ->
                {tags, {[{Key, term_to_json_i(Value)} || {Key, Value} <- Tags]}};
            ({extra, Tags}) ->
                {extra, {[{Key, term_to_json_i(Value)} || {Key, Value} <- Tags]}};
            ({Key, Value}) ->
                {Key, term_to_json_i(Value)}
        end, Params2)
    ]},
    Timestamp = integer_to_list(unix_timestamp_i()),
    Body = base64:encode(zlib:compress(jsone:encode(Document, ?JSONE_OPTS))),
    UA = user_agent(),
    Headers = [
        {"X-Sentry-Auth",
        ["Sentry sentry_version=", ?SENTRY_VERSION,
         ",sentry_client=", UA,
         ",sentry_timestamp=", Timestamp,
         ",sentry_key=", Cfg#cfg.public_key]},
        {"User-Agent", UA}
    ],
    ok = httpc:set_options([{ipfamily, Cfg#cfg.ipfamily}]),
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    httpc:request(post,
        {Cfg#cfg.uri ++ "/api/store/", Headers, "application/octet-stream", Body},
        HttpOptions,
        [{body_format, binary}, {sync, false}, {receiver, fun(_) -> ok end}]
    ),
    ok.

-spec user_agent() -> iolist().
user_agent() ->
    {ok, Vsn} = application:get_key(?APP, vsn),
    ["raven-erlang/", Vsn].

%% @private
-spec get_config() -> cfg_rec().
get_config() ->
    get_config(?APP).

-spec get_config(App :: atom()) -> cfg_rec().
get_config(App) ->
    IpFamily = application:get_env(App, ipfamily, inet),
    case application:get_env(App, dsn) of
        {ok, Dsn} ->
            {match, [_, Protocol, Keys, Uri, Project]} =
                re:run(Dsn, "^(https?://)(.+)@(.+)/(.+)$", [{capture, all, list}]),
            [PublicKey | MaybePrivateKey] = string:split(Keys, ":"),
            PrivateKey =
                case MaybePrivateKey of
                    [] -> "";
                    [Key] -> Key
                end,
            #cfg{uri = Protocol ++ Uri,
                 public_key = PublicKey,
                 private_key = PrivateKey,
                 project = Project,
                 ipfamily = IpFamily};
        undefined ->
            {ok, Uri} = application:get_env(App, uri),
            {ok, PublicKey} = application:get_env(App, public_key),
            {ok, PrivateKey} = application:get_env(App, private_key),
            {ok, Project} = application:get_env(App, project),
            #cfg{uri = Uri,
                 public_key = PublicKey,
                 private_key = PrivateKey,
                 project = Project,
                 ipfamily = IpFamily}
    end.

get_tags() ->
    application:get_env(?APP, tags, []).

maybe_append_release(Params) ->
    case application:get_env(?APP, release) of
        {ok, Release} -> [{release, Release} | Params];
        undefined -> Params
    end.

event_id_i() ->
    <<U0:32, U1:16, _:4, U2:12, _:2, U3:30, U4:32>> = crypto:strong_rand_bytes(16),
    <<UUID:128>> = <<U0:32, U1:16, 4:4, U2:12, 2#10:2, U3:30, U4:32>>,
    iolist_to_binary(io_lib:format("~32.16.0b", [UUID])).

timestamp_i() ->
    {{Y,Mo,D}, {H,Mn,S}} = calendar:now_to_datetime(os:timestamp()),
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
    iolist_to_binary(io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S])).

unix_timestamp_i() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

frame_to_json_i({Module, Function, Arguments}) ->
    frame_to_json_i({Module, Function, Arguments, []});
frame_to_json_i({Module, Function, Arguments, Location}) ->
    Arity = case is_list(Arguments) of
        true -> length(Arguments);
        false -> Arguments
    end,
    Line = case lists:keyfind(line, 1, Location) of
        false -> -1;
        {line, L} -> L
    end,
    {
        case is_list(Arguments) of
            true -> [{vars, [iolist_to_binary(io_lib:format("~w", [Argument])) || Argument <- Arguments]}];
            false -> []
        end ++ [
            {module, Module},
            {function, <<(atom_to_binary(Function, utf8))/binary, "/", (list_to_binary(integer_to_list(Arity)))/binary>>},
            {lineno, Line},
            {filename, case lists:keyfind(file, 1, Location) of
                false -> <<(atom_to_binary(Module, utf8))/binary, ".erl">>;
                {file, File} -> list_to_binary(File)
            end}
        ]
    }.

term_to_json_i(Term) when is_binary(Term); is_atom(Term) ->
    Term;
term_to_json_i(Term) ->
    iolist_to_binary(io_lib:format("~120p", [Term])).
