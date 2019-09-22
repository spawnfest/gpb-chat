-module(rest_SUITE).

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
        live_check_works,
        user_auth_fails,
        send_msg_without_auth,
        when_user_connects_he_appeares_in_session,
        user_sends_session_with_other_users_login,
        message_is_sent_from_user_to_not_connected_user,
        message_is_sent_from_user_and_delivered_to_connected_user,
        many_messages_are_sent_from_user_and_delivered_to_connected_user
    ].


 %%--------------------------------------------------------------------
 %% TEST CASES
 %%--------------------------------------------------------------------

live_check_works(_Config) ->
    Res = live_check(),
    {{<<"200">>, <<"OK">>}, _, Body, _, _} = Res,
    ExpectedResponse = <<"Stayin' Alive">>,
    ?assertEqual(ExpectedResponse, Body),
    ok.

user_auth_fails(_Config) ->
    Login = "auth fail",
    {ok, WS} = ws_client:start_link(?URL_USER_CONN, Login, "fail"),
    timer:sleep(100), % time to connect
    WS ! auth,
    timer:sleep(100), % time for request
    [AuthFailMsg] = get_all_msgs(WS),
    ?assertEqual("server", AuthFailMsg#msg.from),
    ?assertEqual(Login, AuthFailMsg#msg.to),
    ?assertEqual("401", AuthFailMsg#msg.content),
    ?assertEqual('HTTP_RESPONSE', AuthFailMsg#msg.message_type),
    ok.

send_msg_without_auth(_Config) ->
    {ok, WS} = ws_client:start_link(?URL_USER_CONN, "Jan", "xd"),
    timer:sleep(100), % time to connect
    send_msg_assert_result(WS, "Jan", "Hi", "401"),
    ok.

when_user_connects_he_appeares_in_session(_Config) ->
    LoginAlek = "Alek",
    _WSAlek = connect_and_authenticate_succesfully(LoginAlek, "dummy token"),
    [_Session] = session_table:get_user_sesions(LoginAlek),
    ok.

user_sends_session_with_other_users_login(_Config) ->
    WS = connect_and_authenticate_succesfully("Alek", "dummy token"),
    WS ! {send_msg, "Krzys", "Kasia", "Ho Ho Ho"},
    timer:sleep(100), % time for request
    [AuthFailMsg] = get_all_msgs(WS),
    ?assertEqual("server", AuthFailMsg#msg.from),
    ?assertEqual("401", AuthFailMsg#msg.content),
    ?assertEqual('HTTP_RESPONSE', AuthFailMsg#msg.message_type),
    ok.

message_is_sent_from_user_to_not_connected_user(_Config) ->
    LoginAlek = "Alek",
    WSAlek = connect_and_authenticate_succesfully(LoginAlek, "dummy token"),
    send_msg_assert_result(WSAlek, "not existing user", "Hi", "404"),
    ok.

message_is_sent_from_user_and_delivered_to_connected_user(_Config) ->
    LoginAlek = "Alek",
    LoginJulia = "Julia",
    Content = "Hello",
    WSAlek = connect_and_authenticate_succesfully(LoginAlek, "dummy token"),
    WSJulia = connect_and_authenticate_succesfully(LoginJulia, "dummy token"),
    send_msg_assert_result(WSAlek, LoginJulia, Content, "200"),
    timer:sleep(100),
    [MsgFromAlek] = get_all_msgs(WSJulia),
    ?assertEqual(MsgFromAlek#msg.from, LoginAlek),
    ?assertEqual(MsgFromAlek#msg.to, LoginJulia),
    ?assertEqual(MsgFromAlek#msg.content, Content),
    ok.

many_messages_are_sent_from_user_and_delivered_to_connected_user(_Config) ->
    LoginAlek = "Alek",
    LoginJulia = "Julia",
    Contents = ["A", "B", "C"],
    WSAlek = connect_and_authenticate_succesfully(LoginAlek, "dummy token"),
    WSJulia = connect_and_authenticate_succesfully(LoginJulia, "dummy token"),
    lists:map(
        fun(C) ->
            send_msg_assert_result(WSAlek, LoginJulia, C, "200")
        end, Contents),
    timer:sleep(100),
    [MsgC, MsgB, MsgA] = get_all_msgs(WSJulia),
    ?assertEqual(MsgA#msg.content, "A"),
    ?assertEqual(MsgB#msg.content, "B"),
    ?assertEqual(MsgC#msg.content, "C"),
    ok.
 %%--------------------------------------------------------------------
 %% Helper functions
 %%--------------------------------------------------------------------

live_check() ->
    Url = "http://127.0.0.1:8765",
    {ok, Client} = fusco:start_link(Url, []),
    {ok, Result} = fusco:request(Client, <<"/">>, <<"GET">>, [], [], 5000),
    Result.

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
    ?assertEqual(ExpectedResult, MsgResponse#msg.content).
