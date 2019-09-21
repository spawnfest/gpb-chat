-module(user_ws_connection).

-include("src/proto/msg.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%%====================================================================
%% API functions
%%====================================================================

init(Req, _Opts) ->
	{cowboy_websocket, Req, not_authenticated}.

websocket_init(State) ->
	{ok, State}.

websocket_handle({text, Msg}, State = not_authenticated) ->
	DecodedMsg = msg:decode_msg(Msg, msg),
	{Response, NewState} = case DecodedMsg#msg.message_type of
		'AUTH_TOKEN' ->
			case is_valid_token(DecodedMsg) of
				true -> 
					UserLogin = DecodedMsg#msg.from,
					SessionId = session_table:add_session(UserLogin, self()),
					NewSState = #{session_id => SessionId, user => UserLogin},
					{success_msg(DecodedMsg), NewSState};
				_ -> {auth_fail_msg(DecodedMsg), State}
			end;
		_ ->
			{auth_fail_msg(DecodedMsg), State}
		end,
		{reply, {text, msg:encode_msg(Response)}, NewState};
websocket_handle({text, Msg}, #{user := Login} = State) ->
	DecodedMsg = msg:decode_msg(Msg, msg),
	RespMsg = handle_msg(DecodedMsg, Login),
	{reply, {text, msg:encode_msg(RespMsg)}, State}.
% websocket_handle(_Data, State) ->
% 	logger:error("~p:~p, ~p", [?MODULE, ?FUNCTION_NAME, _Data]),
% 	{ok, State}.

websocket_info(Msg = #msg{to = To}, #{user := To} = State) ->
	% logger:error("Sending ~p ~p:~p, ~p", [self(), ?MODULE, ?FUNCTION_NAME, Msg]),
	{reply, {text, msg:encode_msg(Msg)}, State}.
% websocket_info(Info, State) ->
% 	logger:error("~p:~p, ~p", [?MODULE, ?FUNCTION_NAME, Info]),
% 	{ok, State}.

terminate(_Reason, _PartialReq, State) ->
	#{session_id := SessionId, user := UserLogin} = State,
	session_table:remove_user_sesions(UserLogin, SessionId),
	ok.
%%====================================================================
%% Helper functions
%%====================================================================

is_valid_token(#msg{from = Login, content = Content}) ->
	auth_api:is_valid_token(Login, Content).

auth_fail_msg(DecodedMsg) ->
	#msg{from = "server", to = DecodedMsg#msg.from,
		 message_type = 'HTTP_RESPONSE', content = "401",
		 id = DecodedMsg#msg.id}.

success_msg(DecodedMsg) ->
	#msg{from = "server", to = DecodedMsg#msg.from,
		 message_type = 'HTTP_RESPONSE', content = "200",
		 id = DecodedMsg#msg.id}.

handle_msg(DecodedMsg = #msg{from = Login}, Login) ->
	handle_msg(DecodedMsg);
handle_msg(DecodedMsg, _) ->
	auth_fail_msg(DecodedMsg).

handle_msg(DecodedMsg = #msg{message_type = 'MESSAGE', to = To}) ->
	case session_table:get_user_sesions(To) of
		[] ->
			msg_cannot_be_delivered(DecodedMsg);
		Sessions ->
			lists:foreach(
				fun({_Usr, _SessId, Session}) ->
					Session ! DecodedMsg
				end, Sessions),
			success_msg(DecodedMsg)
	end;
handle_msg(DecodedMsg) ->
	#msg{from = "server", to = DecodedMsg#msg.from,
		 message_type = 'HTTP_RESPONSE', content = "405",
		 id = DecodedMsg#msg.id}.

msg_cannot_be_delivered(DecodedMsg) ->
	#msg{from = "server", to = DecodedMsg#msg.from,
		 message_type = 'HTTP_RESPONSE', content = "404",
		 id = DecodedMsg#msg.id}.
