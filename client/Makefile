.PHONY: all
all: deps compile test

.PHONY: deps
deps:
	rebar get-deps

.PHONY: compile
compile:
	rebar compile

.PHONY: fast
fast:
	rebar skip_deps=true compile

.PHONY: test
test:
	rebar skip_deps=true eunit

.PHONY: clean
clean:
	rebar clean
