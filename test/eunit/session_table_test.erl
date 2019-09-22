-module(session_table_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

session_table_is_empty_on_init_test() ->
    {ok, _Pid} = session_table:start_link(),
    Sessions = session_table:get_user_sesions("alek"),
    ?assertEqual([], Sessions),
    gen_server:stop(session_table).

session_table_adding_session_saves_it_test() ->
    {ok, _Pid} = session_table:start_link(),
    User = "alek",
    Session = self(),
    SessionId = session_table:add_session(User, Session),
    Sessions = session_table:get_user_sesions(User),
    ?assertEqual([{User, SessionId, Session}], Sessions),
    gen_server:stop(session_table).

session_table_adding_many_sessions_saves_them_test() ->
    {ok, _Pid} = session_table:start_link(),
    User = "alek",
    Session1 = self(),
    Session2 = dummy_sesion,
    Session3 = <<"asdf">>,
    SessionId1 = session_table:add_session(User, Session1),
    SessionId2 = session_table:add_session(User, Session2),
    SessionId3 = session_table:add_session(User, Session3),
    timer:sleep(100),
    Sessions = session_table:get_user_sesions(User),
    Rec1 = {User, SessionId1, Session1},
    Rec2 = {User, SessionId2, Session2},
    Rec3 = {User, SessionId3, Session3},
    ?assert(in(Sessions, Rec1)),
    ?assert(in(Sessions, Rec2)),
    ?assert(in(Sessions, Rec3)),
    gen_server:stop(session_table).

session_table_removing_sesion_test() ->
    {ok, _Pid} = session_table:start_link(),
    User = "alek",
    Session1 = self(),
    Session2 = dummy_sesion,
    Session3 = <<"asdf">>,
    SessionId1 = session_table:add_session(User, Session1),
    SessionId2 = session_table:add_session(User, Session2),
    SessionId3 = session_table:add_session(User, Session3),
    timer:sleep(100),
    Rec1 = {User, SessionId1, Session1},
    Rec2 = {User, SessionId2, Session2},
    Rec3 = {User, SessionId3, Session3},
    session_table:remove_user_sesions(User, SessionId1),
    timer:sleep(100),
    Sessions = session_table:get_user_sesions(User),
    ?assertNot(in(Sessions, Rec1)),
    ?assert(in(Sessions, Rec2)),
    ?assert(in(Sessions, Rec3)),

    gen_server:stop(session_table).


%%====================================================================
%% Helpers
%%====================================================================

in(List, Elem) ->
    lists:any(
        fun(E) when E =:= Elem -> true;
           (_) -> false end,
        List).
