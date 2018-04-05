.PHONY: test

test:
	ERL_FLAGS="-config ./test/conf/sample.config" rebar3 eunit
