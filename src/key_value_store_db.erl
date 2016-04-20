%%%-------------------------------------------------------------------
%%% @author danrok
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 12:02 PM
%%%-------------------------------------------------------------------
-module(key_value_store_db).
-author("danrok").

-behaviour(gen_server).

-compile([debug_info]).

%% API
-export([start_link/0, persist_data/2, get_by_id/1, get_all/0]).
-export([init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2]).

start_link() -> gen_server:start_link({local, db}, ?MODULE, [], []).

persist_data(Key,Value) ->
  gen_server:call(db, {store, Key, Value}).

get_by_id(Id) ->
  gen_server:call(db, {get_by_id, Id}).

get_all() ->
  gen_server:call(db, get_all).

init([]) ->
  {ok, dict:new()}.

handle_call({store, Key, Value}, _From, Dict) ->
  {reply, ok, dict:store(Key, Value, Dict)};

handle_call({get_by_id, Id}, _From, Dict) ->
  {reply, dict:find(Id, Dict), Dict};

handle_call(get_all, _From, Dict) ->
  {reply, {dict:to_list(Dict)}, Dict}.

handle_cast(_, Dict) ->
  {noreply, Dict}.

handle_info(Msg, Dict) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, Dict}.

code_change(_OldVsn, Dict, _Extra) ->
  {ok, Dict}.

terminate(normal, _Dict) ->
  ok.
