-module(gpb_encode_decode_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/proto/http_response.hrl").

-compile(export_all).

encode_decode_http_response_does_not_change_it_test() ->
    Response = #http_response{code = 200, msg = "Wololo"},
    EncodedResponse = http_response:encode_msg(Response),
    DecodedResponse = http_response:decode_msg(EncodedResponse, http_response),
    ?assertEqual(Response, DecodedResponse).

encode_decode_http_response_has_default_valus_test() ->
    Response = #http_response{},
    ExpectedResponse = #http_response{code = 501, msg = "Not Implemented"},
    EncodedResponse = http_response:encode_msg(Response),
    DecodedResponse = http_response:decode_msg(EncodedResponse, http_response),
    ?assertEqual(ExpectedResponse, DecodedResponse).
