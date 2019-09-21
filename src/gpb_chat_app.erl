%%%-------------------------------------------------------------------
%% @doc gpb_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(gpb_chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    gpb_chat_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init(_) ->
    SupFlags = #{strategy => one_for_one,
      intensity => 10,
      period => 10},
    Children = [
        cowboy_sup_child()
    ],
    {ok,{SupFlags,Children}}.

cowboy_sup_child() ->
    #{id => cowboy_sup,
	  start => {cowboy_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor}.
