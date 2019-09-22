shell:
	rebar3 shell --config priv/sys.config

compile:
	rebar3 compile

eunit:
	rebar3 eunit

ct:
	rebar3 ct --config priv/sys.config

test_all: compile eunit ct

test_cover:
	rebar3 eunit --cover
	rebar3 ct --cover --config priv/sys.config
	rebar3 cover --verbose priv/sys.config
	rebar3 cover --verbose priv/sys.config

proto_python:
	protoc --python_out=python_client/ proto/msg.proto
