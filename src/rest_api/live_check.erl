-module(live_check).

-export([init/2]).

-include("src/proto/http_response.hrl").

init(Req0, Opts) ->
    Headers = #{<<"Content-Type">> => <<"application/protobuf">>},
    Response = #http_response{code = 200, msg = "Stayin' Alive"},
    BinResponse = http_response:encode_msg(Response),
	Req = cowboy_req:reply(200, Headers, BinResponse, Req0),
	{ok, Req, Opts}.
