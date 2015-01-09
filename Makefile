.DEFAULT_GOAL =  all
.PHONY: all compile deps clean distclean check test dialyzer doc

all: deps compile

compile:
				./rebar compile

deps:
				./rebar get-deps

clean:
				./rebar clean

distclean: clean
				./rebar delete-deps

check test: compile
				./rebar eunit

dialyzer: compile
				@dialyzer -Wno_return -c ebin

doc :
				@./rebar doc skip_deps=true
