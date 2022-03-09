%% @doc raven backend for lager

-module(raven_lager_backend).

-behaviour(gen_event).

-export([init/1, code_change/3, terminate/2, handle_call/2, handle_event/2, handle_info/2]).

-define(DEFAULT_METADATA_KEYS, [pid, file, line, module, function, stacktrace]).

-record(state, {level, metadata_keys = []}).

init([Level]) when is_atom(Level) -> init(Level);

init([Level, Metadata]) when is_atom(Level), is_list(Metadata) ->
  case check_metadata(Metadata) of
    ok -> {ok, #state{level = lager_util:config_to_mask(Level), metadata_keys = Metadata}};
    {error, Reason} -> {error, {fatal, {bad_metadata, Reason}}}
  end;

init(Level) when is_atom(Level) -> init([Level, ?DEFAULT_METADATA_KEYS]);
init(_) -> {error, {fatal, invalid_configuration}}.

%% @private

handle_call(get_loglevel, #state{level = Level} = State) -> {ok, Level, State};

handle_call({set_loglevel, Level}, State) ->
  try lager_util:config_to_mask(Level) of
    Levels -> {ok, ok, State#state{level = Levels}}
  catch
    _:_ -> {ok, {error, bad_log_level}, State}
  end;

handle_call(_, State) -> {ok, ok, State}.

%% @private

handle_event({log, Data}, #state{level = L, metadata_keys = MetadataKeys} = State) ->
  case lager_util:is_loggable(Data, L, ?MODULE) of
    true ->
      {Message, Params} = parse_message(Data, MetadataKeys),
      raven:capture(Message, Params),
      {ok, State};

    false -> {ok, State}
  end;

handle_event(_Event, State) -> {ok, State}.


handle_info(_, State) -> {ok, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.

%% @private

check_metadata(Metadata) ->
  case lists:all(fun (X) -> is_atom(X) end, Metadata) of
    true -> ok;
    false -> {error, metadata_must_be_atom}
  end.


parse_message(LagerMsg, MetadataKeys)
when is_tuple(LagerMsg) andalso element(1, LagerMsg) =:= lager_msg ->
  Extra = parse_meta(lager_msg:metadata(LagerMsg), MetadataKeys),
  {lager_msg:message(LagerMsg), [{level, lager_msg:severity(LagerMsg)}, {extra, Extra}]}.

%% @doc Select metadata as defined in config.
%%
%% == Example ===
%%
%% ```
%% {raven_lager_backend, {warning, [pid, stacktrace, module, line, hostname]}
%% '''

parse_meta(MetaData, MetadataKeys) ->
  [Prop || {K, _} = Prop <- MetaData, lists:member(K, MetadataKeys)].
