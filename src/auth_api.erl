-module(auth_api).

-include("hrl/config.hrl").

-export([start/1, is_valid_token/2]).

-callback start(Opts :: term()) -> ok | {error, Reason :: term()}.

-callback is_valid_token(Login :: string(), Token :: string()) ->
    true | false | {error, Reason :: term()}.

start(Opts) ->
    ?AUTH_MODULE:start(Opts).

is_valid_token(Login, Token) ->
    ?AUTH_MODULE:is_valid_token(Login, Token).
