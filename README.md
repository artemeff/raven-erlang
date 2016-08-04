# raven-erlang

raven-erlang is an Erlang client for [Sentry](http://aboutsentry.com/) that integrates with the standard ```error_logger``` module.

## Basic Usage

Add raven as a dependency to your project, and include the raven application in your release:

The raven application itself needs to be configured using the application's environment, this is generally done in app.config or sys.config.

It will accept either the individual config components:

```erlang
{raven, [
    {uri, "https://app.getsentry.com"},
    {project, "1"},
    {public_key, "PUBLIC_KEY"},
    {private_key, "PRIVATE_KEY"},
    {error_logger, true},  % Set to true in order to install the standard error logger
    {ipfamily, inet}  % Set to inet6 to use IPv6. See `ipfamily` in `httpc:set_options/1` for more information.
]}.
```

or just the DSN:

```erlang
{raven, [
    {dsn, "https://PUBLIC_KEY:PRIVATE_KEY@app.getsentry.com/1"},
    {error_logger, true}  % Set to true in order to install the standard error logger
]}.
```

Now all events logged using error_logger will be sent to the [Sentry](http://aboutsentry.com/) service.

## Lager Backend

At the moment, the raven lager backend shares its configuration with the raven application, and does
not allow per-backend configuration.

To add the raven backend:

```erlang
{lager, [
    {handlers, [
        {raven_lager_backend, info}]}]}
```

## Advanced Usage

You can log directly events to sentry using the `raven:capture/2` function, for example:

```erlang
raven:capture("Test Event", [
    {exception, {error, badarg}},
    {stacktrace, erlang:get_stacktrace()},
    {extra, [
        {pid, self()},
        {process_dictionnary, erlang:get()}
    ]}
]).
```
