-module(offline_api).

-export([start_link/0,
    save_message/1,
    get_users_messages/1,
    remove_users_message/2]).

start_link() ->
    Impl = impl(),
    Impl:start_link().

save_message(Message) ->
    Impl = impl(),
    Impl:save_message(Message).

get_users_messages(User) ->
    Impl = impl(),
    Impl:get_users_messages(User).

remove_users_message(User, Message) ->
    Impl = impl(),
    Impl:remove_users_message(User, Message).

impl() ->
    [{_, Impl}] = config:get_module(offline_api),
    Impl.
