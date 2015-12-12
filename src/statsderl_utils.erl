-module(statsderl_utils).
-include("statsderl.hrl").

-export([
    error_msg/2,
    inet_getaddrs/1,
    random/1,
    random_element/1,
    random_server/0,
    server_name/1,
    timestamp/0
]).

%% public
-spec error_msg(string(), [term()]) -> ok.

error_msg(Format, Data) ->
    error_logger:error_msg(Format, Data).

-spec inet_getaddrs(inet:ip_address() | inet:hostname()) ->
    {ok, [inet:ip_address()]} | {error, atom()}.

inet_getaddrs(Hostname) ->
    inet:getaddrs(Hostname, inet).

-spec random(pos_integer()) -> pos_integer().

random(N) ->
    erlang:phash2({self(), timestamp()}, N) + 1.

-spec random_element([term()]) -> term().

random_element([Element]) ->
    Element;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    Index = statsderl_utils:random(tuple_size(T)),
    element(Index, T).

-spec random_server() -> atom().

random_server() ->
    server_name(random(?POOL_SIZE)).

-spec server_name(pos_integer()) -> atom().

server_name(1) -> statsderl_1;
server_name(2) -> statsderl_2;
server_name(3) -> statsderl_3;
server_name(4) -> statsderl_4.

-spec timestamp() -> erlang:timestamp().

timestamp() ->
    os:timestamp().
