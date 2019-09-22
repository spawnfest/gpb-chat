-module(offline_api).

-export([start_link/0,
    save_message/1,
    get_users_messages/1,
    remove_users_message/2]).

start_link() ->
    Impl = config:impl(offline_api),
    Impl:start_link().

save_message(Message) ->
    Impl = config:impl(offline_api),
    Impl:save_message(Message).

get_users_messages(User) ->
    Impl = config:impl(offline_api),
    Impl:get_users_messages(User).

remove_users_message(User, Message) ->
    Impl = config:impl(offline_api),
    Impl:remove_users_message(User, Message).
