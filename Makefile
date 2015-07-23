.PHONY: rel deps test
INCLUDE_FILES=$(shell escript tools/get_included_files_h.erl)
CFLAGS =-undefined dynamic_lookup -fPIC $(INCLUDE_FILES)
LDFLAGS =-fPIC -lexpat
EXML_UTILS=c_src/exml_utils.c
EXML_EVENT_IN=c_src/exml_event.c
EXML_EVENT_OUT=priv/exml_event.so
EXML_ESCAPE_IN=c_src/exml_escape.c
EXML_ESCAPE_OUT=priv/exml_escape.so

all: deps compile

compile: rebar
	./rebar compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

test: compile
	./rebar skip_deps=true eunit

rebar:
	wget https://github.com/rebar/rebar/releases/download/2.5.1/rebar && chmod u+x rebar

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib erts; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/exml.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/exml.plt \
	-o dialyzer/exml.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

exml_plt: dialyzer/exml.plt
	@dialyzer --plt dialyzer/exml.plt --check_plt -o dialyzer/exml.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt exml_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin


shared_libs: shared_event shared_escape

shared_event: $(EXML_UTILS) $(EXML_EVENT_IN)
	gcc  -o $(EXML_EVENT_OUT)  $(EXML_UTILS) $(EXML_EVENT_IN) $(CFLAGS) $(LDFLAGS)

shared_escape: $(EXML_UTILS) $(EXML_ESCAPE)
	gcc  -o $(EXML_ESCAPE_OUT) $(EXML_UTILS) $(EXML_ESCAPE_IN) $(CFLAGS) $(LDFLAGS)

