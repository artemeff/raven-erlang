-module(raven_error_logger_filter).
-include("raven_error_logger.hrl").

-optional_callbacks([should_send_supervisor_report/3]).

%% @doc OTP supervisors will generate an `error_report` every time
%% a process is restarted, even if the shutdown is normal.
%% To avoid spamming Sentry with issues that are not errors,
%% one can implement this callback to filter supervisor reports,
%% received from error logger.
-callback should_send_supervisor_report(supervisor(),
                                        reason(),
                                        supervisor_report_context()) -> boolean().
