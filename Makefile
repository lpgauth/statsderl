REBAR=./rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR) compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR) dialyzer

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR) eunit

test: compile dialyzer eunit xref

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: clean compile dialyzer eunit xref
