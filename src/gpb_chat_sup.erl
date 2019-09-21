%%%-------------------------------------------------------------------
%% @doc gpb_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gpb_chat_sup).

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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

init(_) ->
    SupFlags = #{strategy => one_for_one,
      intensity => 10,
      period => 10},
    Children = [
        cowboy_sup_child(),
        auth_sup_child(),
        session_table()
    ],
    {ok, {SupFlags, Children}}.

cowboy_sup_child() ->
    #{id => my_cowboy_sup,
	  start => {my_cowboy_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor}.

auth_sup_child() ->
    #{id => auth_sup,
	  start => {auth_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor}.

session_table() ->
    #{id => session_table,
	  start => {session_table, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.
