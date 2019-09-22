-module(auth_api).

-export([start/1, is_valid_token/2]).

-callback start(Opts :: term()) -> ok | {error, Reason :: term()}.

-callback is_valid_token(Login :: string(), Token :: string()) ->
    true | false | {error, Reason :: term()}.

start(Opts) ->
    Impl = config:impl(?MODULE),
    Impl:start(Opts).

is_valid_token(Login, Token) ->
    Impl = config:impl(?MODULE),
    Impl:is_valid_token(Login, Token).

