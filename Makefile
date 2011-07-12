.PHONY: rel deps test

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

test:   
	(cd test; make)

eunit:
	./rebar skip_deps=true eunit
