shell:
	rebar3 shell

compile:
	rebar3 compile

eunit:
	rebar3 eunit

ct:
	rebar3 ct

test_all: compile eunit ct

test_cover:
	rebar3 eunit --cover
	rebar3 ct --cover
	rebar3 cover --verbose

proto_python:
	protoc --python_out=python_client/ proto/msg.proto
