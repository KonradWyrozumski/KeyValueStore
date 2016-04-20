%%%-------------------------------------------------------------------
%%% @author danrok
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 1:08 PM
%%%-------------------------------------------------------------------
-module(key_value_store_rest_handler).
-author("danrok").

%% API
-export([init/3]).
-export([content_types_provided/2, allowed_methods/2, terminate/3]).
-export([handle/2]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle}], Req, State}.

handle(Req, State) ->
  {Path, Req1} = cowboy_req:path(Req),
  {Method, Req2} = cowboy_req:method(Req1),
  data_handle(Path, Method, Req2, State).

data_handle(<<"/data/">>, <<"GET">>, Req, State) ->
  Msg = jiffy:encode(get_all()),
  {ok, Res1} = cowboy_req:reply(200, [], Msg, Req),
  {ok, Res1, State};

data_handle(<<"/data/", Id/binary>>, <<"GET">>, Req, State) ->
  {Code, Msg} = case get_by_key(Id) of
                  {error, key_not_found} ->
                    {404, io_lib:format("Key ~p not found", [Id])};
                  {ok, Value} ->
                    {200, jiffy:encode(Value)}
                end,
  {ok, Res1} = cowboy_req:reply(Code, [], Msg, Req),
  {ok, Res1, State};

data_handle(<<"/data/">>, <<"POST">>, Req, State) ->
  {ok, Bin, Req1} = cowboy_req:body(Req),
  {Code, Msg} = case decode_json(Bin) of
                  {error, invalid_json} ->
                    {400, io_lib:format("Invalid json ~p", [Bin])};
                  {ok, undefined, _} ->
                    {400, io_lib:format("Missing key in json ~p", [Bin])};
                  {ok, _, undefined} ->
                    {400, io_lib:format("Missing value in json ~p", [Bin])};
                  {ok, Key, Value} ->
                    key_value_store_db:persist_data(Key, Value),
                    {200, "ok"}
                end,
  {ok, Res1} = cowboy_req:reply(Code, [], Msg, Req1),
  {ok, Res1, State};

data_handle(Path, Method, Req, State) ->
  Msg = io_lib:format("Incorrect method:~p, path:~p", [Method, Path]),
  {ok, Req1} = cowboy_req:reply(400, [], Msg, Req),
  {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% Private functions.
get_all() ->
  key_value_store_db:get_all().

get_by_key(Key) ->
  case key_value_store_db:get_by_id(Key) of
    error ->
      {error, key_not_found};
    {ok, Value} ->
      {ok, {[{<<"key">>,Key},{<<"value">>,Value}]}}
  end.

decode_json(BinaryJson) ->
  try jiffy:decode(BinaryJson) of
    {DecodedJson} ->
      {ok,
        proplists:get_value(<<"key">>, DecodedJson),
        proplists:get_value(<<"value">>, DecodedJson)}
    catch
      _Exception -> {error, invalid_json}
  end.
