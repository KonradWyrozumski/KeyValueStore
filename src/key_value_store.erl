%%%-------------------------------------------------------------------
%%% @author danrok
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 12:31 PM
%%%-------------------------------------------------------------------
-module(key_value_store).
-author("danrok").

%% API
-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(key_value_store).
