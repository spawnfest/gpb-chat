-module(dummy_test_auth_module).

-behaviour(auth_api).

-export([start_link/1, start_link/0, is_valid_token/2]).

start_link(_) ->
    ok.

start_link() ->
    ok.

is_valid_token(_Login, "fail") ->
    false;
is_valid_token(_Login, _Token) ->
    true.
