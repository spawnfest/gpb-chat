-module(offline_off).

-include("src/proto/msg.hrl").

-export([start_link/0,
    save_message/1,
    get_users_messages/1,
    remove_users_message/2]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
        receive
            _ -> ok
        end.

save_message(_Message) ->
    ok.

get_users_messages(_User) ->
    [].

remove_users_message(_User, _Message) ->
    ok.
