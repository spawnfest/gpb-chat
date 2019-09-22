-module(offline_ets).

-behaviour(gen_server).

-include("src/proto/msg.hrl").

-export([save_message/1,
    get_users_messages/1,
    remove_users_message/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(OFFLINE_TABLE_NAME, offline_gpb_chat_table).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save_message(#msg{to = User} = Message) ->
    gen_server:call(?MODULE, {add, User, Message}).

get_users_messages(User) ->
    gen_server:call(?MODULE, {lookup, User}).

remove_users_message(User, Message) ->
    gen_server:cast(?MODULE, {remove, User, Message}).

%%====================================================================
%% Genserver callbacks
%%====================================================================

init(_Args) ->
    ets:new(?OFFLINE_TABLE_NAME, [duplicate_bag, named_table]),
   {ok, #{}}.

handle_call({add, User, Message}, _From, State) ->
    ets:insert(?OFFLINE_TABLE_NAME, {User, Message}),
    {reply, ok, State};
handle_call({lookup, User}, _From, State) ->
    Reply = ets:lookup(?OFFLINE_TABLE_NAME, User),
    {reply, Reply, State}.

handle_cast({remove, User, Message}, State) ->
    Obj = {User, Message},
    ets:delete_object(?OFFLINE_TABLE_NAME, Obj),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
