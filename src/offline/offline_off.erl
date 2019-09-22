-module(offline_off).

-behaviour(supervisor).

-include("src/proto/msg.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0,
    save_message/1,
    get_users_messages/1,
    remove_users_message/2]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

save_message(_Message) ->
    ok.

get_users_messages(_User) ->
    [].

remove_users_message(_User, _Message) ->
    ok.

init([]) ->
    {ok, {{one_for_all, 10, 10}, []}}.
