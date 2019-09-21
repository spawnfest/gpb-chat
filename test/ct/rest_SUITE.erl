-module(rest_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("src/proto/http_response.hrl").
-include_lib("eunit/include/eunit.hrl").

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
         gpb_chat
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
     [live_check_works].


 %%--------------------------------------------------------------------
 %% TEST CASES
 %%--------------------------------------------------------------------

live_check_works(_Config) ->
    Res = live_check(),
    ct:log("Res = ~p\n", [Res]),
    {{<<"200">>, <<"OK">>}, _, Body, _, _} = Res,
    DecodedResponse = http_response:decode_msg(Body, http_response),
    ExpectedResponse = #http_response{code = 200, msg = "Stayin' Alive"},
    ?assertEqual(ExpectedResponse, DecodedResponse),
    ok.

 %%--------------------------------------------------------------------
 %% Helper functions
 %%--------------------------------------------------------------------

live_check() ->
    Url = "http://127.0.0.1:8765",
    {ok, Client} = fusco:start_link(Url, []),
    {ok, Result} = fusco:request(Client, <<"/">>, <<"GET">>, [], [], 5000),
    Result.
