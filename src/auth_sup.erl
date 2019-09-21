-module(auth_sup).

-include("hrl/config.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(Opts) ->
    init(?AUTH_MODULE, Opts).

init(dummy_test_auth_module, Opts) ->
    auth_api:start(Opts),
    {ok, {{one_for_all, 10, 10}, []}};
% add your auth module init finction here :)
init(_, _) ->
    {error, "Not implemented"}.
