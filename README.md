# raven-erlang

raven-erlang is an Erlang client for [Sentry](http://aboutsentry.com/) that integrates with the standard `error_logger` module. It also serves as a [Lager](https://github.com/erlang-lager/lager) backend.

[![Build Status](https://travis-ci.org/artemeff/raven-erlang.svg?branch=master)](https://travis-ci.org/artemeff/raven-erlang)
[![Hex.pm](https://img.shields.io/hexpm/v/raven_erlang.svg)](https://hex.pm/packages/raven_erlang)

## Basic Usage

## Add as dependency

Add raven as a dependency to your project, and include the `raven-erlang` application in your release.

In `rebar.config`:

```erlang
{deps, [
    {raven_erlang, "0.4.0"}
]}.
```

To start `raven_erlang` with your application, add in your `myapp.app.src`:

```erlang
% ...
{applications, [
    % ...
    raven_erlang
]},
% ...
```

## Configure

`raven_erlang` is configured using the application environment. This is generally done in app.config or sys.config:

```erlang
{raven_erlang, [
    % One can point `raven_erlang` to a project like this:
    {uri, "https://app.getsentry.com"},
    {project, "1"},
    {public_key, "PUBLIC_KEY"},
    {private_key, "PRIVATE_KEY"},

    % ...or just use the DSN:
    {dsn, "https://PUBLIC_KEY:PRIVATE_KEY@app.getsentry.com/1"},

    % Set to inet6 to use IPv6.
    % See `ipfamily` in `httpc:set_options/1` for more information.
    % Default value is `inet`.
    {ipfamily, inet},

    % Sentry api version
    % Use 7.0 to send private key (required for cloud version sentry.io)
    % {sentry_ver, 7.0}

    % Environment
    {environment, "local"},

    % Set to true in order to install the standard error logger.
    % Now all events logged using `error_logger` will be sent to Sentry.
    {error_logger, true},

    % Customize error logger:
    % Default value is `[]`.
    {error_logger_config, [
        % `warning` or `error`.
        % If set to `error`, error logger will ignore warning messages and reports.
        % Default value is `warning`.
        {level, warning},

        % Not all messages that error_logger generates are useful.
        % For example, supervisors will always generate an error_report when
        % restarting a child, even if it exits with `reason = normal`.
        % You can provide a module that implements `raven_error_logger_filter` behavior
        % to avoid spamming sentry with issues that are not errors.
        % See http://erlang.org/doc/apps/sasl/error_logging.html for more information.
        % Default value is `undefined`.
        {filter, callback_module}
    ]}
]}.
```

## Lager Backend

At the moment, the raven lager backend shares its configuration with the raven application, and does
not allow per-backend configuration.

### Simple Configuration

This adds the raven backend to lager. By default, it configures the raven lager backend to send most metadata that the lager parse transform creates (see Advanced Configuration).

```erlang
{lager, [
    {handlers, [
        {raven_lager_backend, info}]}]}
```

### Advanced Configuration

This configuration uses a list `[Level :: atom(), MetadataKeys :: [atom()]]`.

`MetadataKeys` is a list of atoms that correspond to the metadata to be sent by Raven, should it be included in the lager log message.

The configuration shown here is equivalent to the Simple Configuration.

```erlang
{lager, [
    {handlers, [
        {raven_lager_backend,
            [info, [pid, file, line, module, function, stacktrace]]}]}]}
```

To exclude all metadata except `pid`:

```erlang
{lager, [
    {handlers, [
        {raven_lager_backend, [info, [pid]]}]}]}
```


## Advanced Usage

You can log directly events to sentry using the `raven:capture/2` function, for example:

```erlang
raven:capture("Test Event", [
    {exception, {error, badarg}},
    {stacktrace, erlang:get_stacktrace()},
    {extra, [
        {pid, self()},
        {process_dictionary, erlang:get()}
    ]}
]).
```
