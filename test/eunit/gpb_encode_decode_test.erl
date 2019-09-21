-module(gpb_encode_decode_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/proto/msg.hrl").

-compile(export_all).

encode_decode_msg_does_not_change_it_test() ->
    Msg = #msg{content = "Wololo"},
    EncodedMsg = msg:encode_msg(Msg),
    DecodedMsg = msg:decode_msg(EncodedMsg, msg),
    ?assertEqual(Msg, DecodedMsg).

encode_decode_msg_has_default_valus_test() ->
    Msg = #msg{},
    ExpectedMsg = 
        #msg{from = "server", to = "server", content = "",
             timestamp = "", id = "", message_type = 'HTTP_RESPONSE'},
    EncodedMsg = msg:encode_msg(Msg),
    DecodedMsg = msg:decode_msg(EncodedMsg, msg),
    ?assertEqual(ExpectedMsg, DecodedMsg).
