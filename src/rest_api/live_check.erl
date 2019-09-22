-module(live_check).

-export([init/2]).

init(Req0, Opts) ->
    Headers = #{<<"Content-Type">> => <<"text/plain">>},
    Response = "Stayin' Alive",
    logger:debug("~p:~p ~p", [?MODULE, ?FUNCTION_NAME, Response]),
	Req = cowboy_req:reply(200, Headers, Response, Req0),
	{ok, Req, Opts}.
