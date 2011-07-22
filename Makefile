.PHONY: rel deps test

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

test: compile
	./rebar skip_deps=true eunit
