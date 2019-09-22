-module(config).

-behaviour(gen_server).

-export([impl/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CONFIG_TABLE_NAME, config_gpb_chat_table).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

impl(Mod) ->
    [{_, Impl}] = get_module(Mod),
    logger:debug("Mod = ~p, Impl = ~p", [Mod, Impl]),
    Impl.

%%====================================================================
%% Genserver callbacks
%%====================================================================

get_module(Module) ->
    gen_server:call(?MODULE, {lookup, Module}).

init(_Args) ->
    ets:new(?CONFIG_TABLE_NAME, [duplicate_bag, named_table]),
    Envs = application:get_all_env(gpb_chat),
    lists:map(
        fun({log_level, Level}) ->
                set_log_level(Level);
           (Mod) ->
            ets:insert(?CONFIG_TABLE_NAME, Mod)
        end, Envs),
    {ok, #{}}.

handle_call({lookup, Module}, _From, State) ->
    Reply = ets:lookup(?CONFIG_TABLE_NAME, Module),
    {reply, Reply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->

   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

set_log_level(Level) ->
    logger:set_primary_config(level, Level).