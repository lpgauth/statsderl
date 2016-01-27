CACHEGRIND=qcachegrind
REBAR=./rebar3

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

test: compile dialyzer eunit xref

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: clean compile dialyzer edoc eunit profile xref
