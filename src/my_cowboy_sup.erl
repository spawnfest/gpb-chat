%%%-------------------------------------------------------------------
%% @doc gpb_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(my_cowboy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", live_check, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8765}], #{
		env => #{dispatch => Dispatch}
	}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 10, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
