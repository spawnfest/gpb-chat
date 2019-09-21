shell:
	
	rebar3 shell

compile:
	rebar3 compile

eunit:
	rebar3 eunit

test_all: compile eunit