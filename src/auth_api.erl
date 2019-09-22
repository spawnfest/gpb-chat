-module(auth_api).

-export([start/1, is_valid_token/2]).

-callback start(Opts :: term()) -> ok | {error, Reason :: term()}.

-callback is_valid_token(Login :: string(), Token :: string()) ->
    true | false | {error, Reason :: term()}.

start(Opts) ->
    Impl = impl(),
    Impl:start_link(Opts).

is_valid_token(Login, Token) ->
    Impl = impl(),
    Impl:is_valid_token(Login, Token).

impl() ->
    [{_, Impl}] = config:get_module(auth_api),
    Impl.