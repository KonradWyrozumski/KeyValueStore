%%%-------------------------------------------------------------------
%%% @author danrok
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 12:38 PM
%%%-------------------------------------------------------------------
-module(key_value_store_app).
-author("danrok").

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Routes = routes(),
  Dispatch = cowboy_router:compile(Routes),
  HttpPort = get_port(http_port),
  TCP_Port = get_port(tcp_port),
  UDP_Port = get_port(udp_port),
  {ok, _} = cowboy:start_http(http, 10, [{port, HttpPort}], [{env, [{dispatch, Dispatch}]}]),
  key_value_store_sup:start_link([TCP_Port, UDP_Port]).

stop(_State) ->
  ok.

routes() ->
  [
    {'_', [
      {"/", key_value_store_rest_handler, []},
      {"/data/:id", key_value_store_rest_handler, []},
      {"/data/", key_value_store_rest_handler, []}
    ]}
  ].

get_port(PortType) ->
  {ok, Port} = application:get_env(PortType),
  Port.

