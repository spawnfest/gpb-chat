-module(ws_client).

-include("src/proto/msg.hrl").

-behaviour(websocket_client).

-export([
         start_link/3,
         init/1,
         onconnect/2,
         ondisconnect/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link(Address, Login, Token) ->
    websocket_client:start_link(Address, ?MODULE, [Login, Token]).

init([Login, Token]) ->
    {once, #{login => Login, token => Token, msgs => []}}.

onconnect(_WSReq, State) ->
    {ok, State}.

ondisconnect({remote, closed}, State) ->
    {reconnect, State};
ondisconnect(Disc, State) ->
    logger:error("Unexpected Disc = ~p, State = ~p", [Disc, State]),
    {reconnect, State}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};
websocket_handle({binary, Msg}, _ConnState, #{msgs := Msgs} = State) ->
    DecodedMsg = msg:decode_msg(Msg, msg),
    NewState = State#{msgs => [DecodedMsg | Msgs]},
    {ok, NewState};
websocket_handle(Req, _ConnState, State) ->
    logger:error("Unexpected Req = ~p, State = ~p", [Req, State]),
    {ok, State}.

websocket_info(auth, _ConnState, State) ->
    #{login := Login, token := Token} = State,
    BinAuthToken = make_auth_token(Login, Token),
    {reply, {binary, BinAuthToken}, State};
websocket_info({send_msg, FromLogin, ToLogin, Content}, _ConnState, State) ->
    BinMsg = make_msg(FromLogin, ToLogin, Content),
    {reply, {binary, BinMsg}, State};
websocket_info({send_msg, ToLogin, Content}, _ConnState, State) ->
    #{login := FromLogin} = State,
    BinMsg = make_msg(FromLogin, ToLogin, Content),
    {reply, {binary, BinMsg}, State};
websocket_info({get_all_msgs, Pid}, _ConnState, #{login := Login, msgs := Msgs} = State) ->
    Pid ! {msgs, Msgs},
    logger:error("~p Msgs = ~p", [Login, Msgs]),
    {ok, State#{msgs => []}};
websocket_info(Msg, _ConnState, State) ->
    logger:error("Unexpected Msg = ~p, State = ~p", [Msg, State]),
    {ok, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ok.

%%====================================================================
%% Helper functions
%%====================================================================

make_auth_token(Login, Token) ->
    Msg = #msg{from = Login, to = "server", message_type = 'AUTH_TOKEN',
         content = Token, id = "Auth " ++ Login},
    ct:log("Module = ~p Msg = ~p", [?MODULE, Msg]),
    msg:encode_msg(Msg).

make_msg(From, To, Content) ->
    Msg = #msg{from = From, to = To, message_type = 'MESSAGE',
         content = Content, id = "Msg from " ++ From ++ " to " ++ To},
    ct:log("Module = ~p Msg = ~p", [?MODULE, Msg]),
    msg:encode_msg(Msg).