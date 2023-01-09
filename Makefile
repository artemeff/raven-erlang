.PHONY: test

test:
	ERL_FLAGS="-config ./test/conf/sample.config" rebar3 eunit
	ERL_FLAGS="-config ./test/conf/deprecated_dsn.config" rebar3 eunit
