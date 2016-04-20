%%%-------------------------------------------------------------------
%%% @author danrok
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 12:33 PM
%%%-------------------------------------------------------------------
-module(key_value_store_sup).
-author("danrok").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).
-define(CHILD(Name, I, Args), {Name, {I, start_link, Args}, permanent, 2000, worker, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link([TCP_Port, UDP_Port]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [TCP_Port, UDP_Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([TCP_Port, UDP_Port]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Children =
    [
      ?CHILD(key_value_store_db, key_value_store_db, []),
      ?CHILD(key_value_store_tcp, key_value_store_telnet_handler, [{tcp, TCP_Port}]),
      ?CHILD(key_value_store_udp, key_value_store_telnet_handler, [{udp, UDP_Port}])
    ],

  {ok, {SupFlags, Children}}.