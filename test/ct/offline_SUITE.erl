-module(offline_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("src/proto/msg.hrl").

-define(URL_USER_CONN, "ws://localhost:8765/user/connect").

 %% Test server callbacks
 -export([suite/0, all/0,
	  init_per_suite/1, end_per_suite/1,
	  init_per_testcase/2, end_per_testcase/2]).

 %% Test cases
 -compile(export_all).

 suite() ->
     [{timetrap,{minutes,10}}].

 init_per_suite(Config) ->
     Apps = [
         inets,
         gun,
         gpb_chat,
         crypto,
         ssl
     ],
     lists:foreach(fun application:ensure_all_started/1, Apps),
     Config.

 end_per_suite(_Config) ->
     ok.

 init_per_testcase(_Case, Config) ->
     Config.

 end_per_testcase(_Case, _Config) ->
     ok.

 all() ->
     [
         message_is_sent_to_disconnected_user_is_saved_by_offline_api,
         many_messages_are_sent_to_disconnected_user_are_saved_by_offline_api,
         message_is_sent_to_disconnected_user_and_delivered_when_user_connects,
         many_messages_are_sent_to_disconnected_user_and_delivered_when_user_connects
     ].


 %%--------------------------------------------------------------------
 %% TEST CASES
 %%--------------------------------------------------------------------

message_is_sent_to_disconnected_user_is_saved_by_offline_api(_Config) ->
    LoginMadzia = msg_helper:fresh_login(),
    LoginJulia = msg_helper:fresh_login(),
    Content = "Hello",
    WS = connect_and_authenticate_succesfully(LoginMadzia, "dummy token"),
    send_msg_assert_result(WS, LoginJulia, Content, "201"),
    [] = offline_api:get_users_messages(LoginMadzia),
    [{LoginJulia, _}] = offline_api:get_users_messages(LoginJulia),
    ok.

many_messages_are_sent_to_disconnected_user_are_saved_by_offline_api(_Config) ->
    LoginMadzia = msg_helper:fresh_login(),
    LJ = LoginJulia = msg_helper:fresh_login(),
    WS = connect_and_authenticate_succesfully(LoginMadzia, "dummy token"),
    lists:foreach(
        fun(C) ->
            send_msg_assert_result(WS, LoginJulia, C, "201")
        end, ["A", "A", "B"]),
    [] = offline_api:get_users_messages(LoginMadzia),
    [
        {LJ, #msg{content = "A"}},
        {LJ, #msg{content = "A"}},
        {LJ, #msg{content = "B"}}
    ] = offline_api:get_users_messages(LoginJulia),
    ok.

message_is_sent_to_disconnected_user_and_delivered_when_user_connects(_Config) ->
    LoginAlek = msg_helper:fresh_login(),
    LoginJulia = msg_helper:fresh_login(),
    Content = "Hello",
    WSAlek = connect_and_authenticate_succesfully(LoginAlek, "dummy token"),
    send_msg_assert_result(WSAlek, LoginJulia, Content, "201"),
    timer:sleep(100),
    {ok, WS} = ws_client:start_link(?URL_USER_CONN, LoginJulia, "dummy token"),
    timer:sleep(100), % time to connect
    WS ! auth,
    timer:sleep(100), % time for request
    Msgs = get_all_msgs(WS),
    ?assertEqual(2, length(Msgs)),
    ExpectedContents = ["200", Content],
    assert_messages_contents(Msgs, ExpectedContents, 2),
    ok.

many_messages_are_sent_to_disconnected_user_and_delivered_when_user_connects(_Config) ->
    LoginAlek = msg_helper:fresh_login(),
    LoginJulia = msg_helper:fresh_login(),
    Contents = ["A", "B", "C"],
    WSAlek = connect_and_authenticate_succesfully(LoginAlek, "dummy token"),
    lists:foreach(
        fun(Content) ->
            send_msg_assert_result(WSAlek, LoginJulia, Content, "201")
        end, Contents),
    timer:sleep(100),
    {ok, WS} = ws_client:start_link(?URL_USER_CONN, LoginJulia, "dummy token"),
    timer:sleep(100), % time to connect
    WS ! auth,
    timer:sleep(100), % time for request
    Msgs = get_all_msgs(WS),
    ExpectedContents = ["200" | Contents],
    assert_messages_contents(Msgs, ExpectedContents, 4),
    ok.

 %%--------------------------------------------------------------------
 %% Helper functions
 %%--------------------------------------------------------------------

connect_and_authenticate_succesfully(Login, Token) ->
    {ok, WS} = ws_client:start_link(?URL_USER_CONN, Login, Token),
    timer:sleep(100), % time to connect
    WS ! auth,
    timer:sleep(100), % time for request
    [AuthMsg] = get_all_msgs(WS),
    ?assertEqual("200", AuthMsg#msg.content),
    WS.

get_all_msgs(WS) ->
    WS ! {get_all_msgs, self()},
    receive
        {msgs, Msgs} -> Msgs
    after
        1000 -> {error, timeout}
    end.

send_msg_assert_result(WS, To, Content, ExpectedResult) ->
    WS ! {send_msg, To, Content},
    timer:sleep(100),
    [MsgResponse] = get_all_msgs(WS),
    ?assertEqual(MsgResponse#msg.content, ExpectedResult).

assert_messages_contents(Msgs, ExpectedContents, ExpectedLen) ->
    ?assertEqual(ExpectedLen, length(Msgs)),
    MContents = lists:map(fun(#msg{content = C}) -> C end, Msgs),
    ?assertEqual(length(MContents), length(ExpectedContents)),
    ?assertEqual([], MContents -- ExpectedContents),
    ?assertEqual([], ExpectedContents -- MContents).
