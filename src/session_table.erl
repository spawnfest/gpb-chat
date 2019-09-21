-module(session_table).

-behaviour(gen_server).

-export([add_session/2,
    get_user_sesions/1,
    remove_user_sesions/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SESSION_TABLE_NAME, session_table).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_session(User, Session) ->
    SessionId = crypto:strong_rand_bytes(30),
    gen_server:cast(?MODULE, {add, User, SessionId, Session}),
    SessionId.

get_user_sesions(User) ->
    gen_server:call(?MODULE, {lookup, User}).

remove_user_sesions(User, SessionId) ->
    gen_server:cast(?MODULE, {remove, User, SessionId}).

%%====================================================================
%% Genserver callbacks
%%====================================================================

init(_Args) ->
    ets:new(?SESSION_TABLE_NAME, [duplicate_bag, named_table]),
   {ok, #{}}.

handle_call({lookup, User}, _From, State) ->
    Reply = ets:lookup(?SESSION_TABLE_NAME, User),
    {reply, Reply, State}.

handle_cast({add, User, SessionId, Session}, State) ->
    ets:insert(?SESSION_TABLE_NAME, {User, SessionId, Session}),
    {noreply, State};

handle_cast({remove, User, SessionId}, State) ->
    Sessions = ets:lookup(?SESSION_TABLE_NAME, User),
    lists:foreach(
        fun(Obj = {Usr, SessId, _}) 
            when User =:= Usr andalso SessionId =:= SessId ->
                ets:delete_object(?SESSION_TABLE_NAME, Obj);
           (_) -> ok
        end, Sessions),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
