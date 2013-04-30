REBAR    = ./rebar

.PHONY: all clean 

all:deps compile

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit