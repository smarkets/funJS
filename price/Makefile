.PHONY: rel deps

all: deps compile

compile:
	rebar compile

fast:
	rebar compile skip_deps=true

test:
	rebar eunit skip_deps=true

deps:
	rebar get-deps

clean:
	rm -rf deps
	rebar clean
