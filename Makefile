CACHEGRIND=qcachegrind
ELVIS=./bin/elvis
REBAR=./bin/rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR) as build compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR) edoc

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR) do eunit -cv, cover -v

profile:
	@echo "Profiling..."
	@$(REBAR) as test compile
	@erl -noshell \
	     -pa _build/test/lib/*/ebin \
		 -eval 'statsderl_profile:fprofx()' \
		 -eval 'init:stop()'
	@_build/test/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

test: compile dialyzer elvis eunit xref

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: clean compile dialyzer edoc elvis eunit profile xref
