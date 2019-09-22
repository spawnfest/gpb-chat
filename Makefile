shell:
	rebar3 shell --config priv/sys.config

compile:
	rebar3 compile

eunit:
	rebar3 eunit --cover

ct:
	rebar3 ct --cover --suite=test/ct/rest_SUITE --sys_config priv/test_offline_off.config
	rebar3 ct --cover --suite=test/ct/offline_SUITE --sys_config priv/test_offline_ets.config

print_cover:
	rebar3 cover --verbose

test_all_with_cover: | eunit ct print_cover

proto_python:
	protoc --python_out=python_client/ proto/msg.proto
