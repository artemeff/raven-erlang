-type report_type() ::
    crash_report |
    supervisor_report |
    progress |
    std_error |
    any(). % error_logger:error_report/2 can be called with any Type.
-type logging_level() :: warning | error.
-type reason() :: any().
-type supervisor() :: atom().
-type supervisor_report_context() ::
    start_error |
    child_terminated |
    shutdown_error.

