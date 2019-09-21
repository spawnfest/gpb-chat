-module(dummy_test_auth_module).

-behaviour(auth_api).

-export([start/1, is_valid_token/2]).

start(_Opts) ->
    ok.

is_valid_token(_Login, "fail") ->
    false;
is_valid_token(_Login, _Token) ->
    true.
