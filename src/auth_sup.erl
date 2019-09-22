-module(auth_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(Opts) ->
    auth_api:start(Opts),
    {ok, {{one_for_all, 10, 10}, []}}.
    