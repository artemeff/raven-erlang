-module(raven_app).
-include("raven.hrl").
-behaviour(application).
-export([
	start/2,
	stop/1
]).

%% @hidden
start(_StartType, _StartArgs) ->
		case application:get_env(?APP, error_logger) of
			{ok, true} ->
				error_logger:add_report_handler(raven_error_logger);
			_ ->
				ok
		end,
		raven_sup:start_link().

%% @hidden
stop(_State) ->
	case application:get_env(?APP, error_logger) of
		{ok, true} ->
			error_logger:delete_report_handler(raven_error_logger),
			ok;
		_ ->
			ok
	end.
