%%%-------------------------------------------------------------------
%%% @author danrok
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2016 11:47 AM
%%%-------------------------------------------------------------------
-module(key_value_store_telnet_handler).
-author("danrok").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link({tcp, Port}) ->
  {ok, ListenPort} = gen_tcp:listen(Port, [binary, {active,once}, {packet,line}]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, {tcp, ListenPort}, []);

start_link({udp, Port}) -> gen_server:start_link(?MODULE, {udp, Port}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({tcp, ListenSocket}) ->
  gen_server:cast(self(), accept),
  {ok, ListenSocket};
init({udp, Port}) ->
  {ok, UdpSocket} = gen_udp:open(Port, [binary, {active,true}]),
  {ok, UdpSocket}.

handle_cast(accept, ListenSocket) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  {noreply, AcceptSocket}.

%%% TCP Handlers.
handle_info({tcp, _Socket, <<"Add:", KeyValue/binary>>}, Socket) ->
  send(tcp, Socket, store_data(KeyValue)),
  {noreply, Socket};

handle_info({tcp, _Socket, <<"Get:", Id/binary>>}, Socket) ->
  send(tcp, Socket, get_data_by_id(remove_blank_spaces(Id))),
  {noreply, Socket};

handle_info({tcp, _Socket, <<"GetAll:", _Rest/binary>>}, Socket) ->
  io:format("~p", [get_all_data()]),
  send(tcp, Socket, get_all_data()),
  {noreply, Socket};

handle_info({tcp_closed, _Socket}, Socket) ->
  {stop, normal, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
  Msg = io_lib:format(<<"Unknown command~p">>, [Str]),
  send(tcp, Socket, Msg),
  {noreply, Socket};

%%% UDP Handlers.
handle_info({udp, _UdpSocket, Address, Port, <<"Add:", KeyValue/binary>>}, Socket) ->
  Data = store_data(KeyValue),
  send(udp, {Socket, Address, Port}, Data),
  {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"Get:", Id/binary>>}, Socket) ->
  Data = get_data_by_id(remove_blank_spaces(Id)),
  send(udp, {Socket, Address, Port}, Data),
  {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"GetAll:", _Rest/binary>>}, Socket) ->
  Data = get_all_data(),
  send(udp, {Socket, Address, Port}, Data),
  {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<Str/binary>>}, Socket) ->
  Msg = io_lib:format(<<"Unknown command~p">>, [Str]),
  send(udp, {Socket, Address, Port}, Msg),
  {noreply, Socket}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private functions.
store_data(KeyValue) ->
  KeyValueStr = binary_to_list(KeyValue),
  case string:str(KeyValueStr, ":") of
    ColonPosition when ColonPosition > 0 ->
      Key = list_to_binary(string:substr(KeyValueStr, 1, ColonPosition - 1)),
      Value = list_to_binary(string:substr(KeyValueStr, ColonPosition + 1)),
      key_value_store_db:persist_data(Key, Value),
      ok;
    ColonPosition when  ColonPosition =:= 0 -> unknown_pattern
  end.

get_data_by_id(Id) ->
  case key_value_store_db:get_by_id(Id) of
    {ok, Value} ->
      format_key_value(Id, Value);
    _ -> "Not found"
  end.

get_all_data() ->
  {KeyValues} = key_value_store_db:get_all(),
  FormattedKeyValue = [format_key_value(K,V) || {K,V} <- KeyValues],
  string:join(FormattedKeyValue, ", ").

format_key_value(Key, Value) ->
  io_lib:format("Key:~p Value:~p", [Key, Value]).

send(Protocol, Socket, <<BitStr/binary>>) ->
  send(Protocol, Socket, binary_to_list(BitStr));

send(Protocol, Socket, Atom) when is_atom(Atom) ->
  send(Protocol, Socket, atom_to_list(Atom));

send(tcp, Socket, Str) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", [])),
  ok = inet:setopts(Socket, [{active, once}]),
  ok;

send(udp, {Socket, Address, Port}, Str) ->
  gen_udp:send(Socket, Address, Port, Str).

remove_blank_spaces(Str) ->
  re:replace(Str, "(\\s+\\s+)", "", [{return,binary}]).