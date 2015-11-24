-module(statsderl_utils).
-include("statsderl.hrl").

-export([
    now_diff_ms/1,
    random/1,
    random_element/1,
    random_server/0,
    server_name/1
]).

%% public
-spec now_diff_ms(erlang:timestamp()) -> non_neg_integer().

now_diff_ms(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp) div 1000.

-spec random(pos_integer()) -> non_neg_integer().

random(N) ->
    erlang:phash2({self(), os:timestamp()}, N).

-spec random_element([term()]) -> term().

random_element([X]) ->
    X;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    element(random(tuple_size(T)) + 1, T).

-spec random_server() -> atom().

random_server() ->
    server_name(random(?POOL_SIZE) + 1).

-spec server_name(pos_integer()) -> atom().

server_name(1) -> statsderl_1;
server_name(2) -> statsderl_2;
server_name(3) -> statsderl_3;
server_name(4) -> statsderl_4.
