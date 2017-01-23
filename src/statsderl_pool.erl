-module(statsderl_pool).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    sample/2,
    sample_scaled/2,
    server_name/1
]).

%% public
-spec sample(sample_rate(), operation()) ->
    ok.

sample(1, Operation) ->
    operation(Operation);
sample(1.0, Operation) ->
    operation(Operation);
sample(Rate, Operation) ->
    RateInt = trunc(Rate * ?MAX_UNSIGNED_INT_32),
    sample_scaled(RateInt, Operation).

-spec sample_scaled(non_neg_integer(), operation()) ->
    ok.

sample_scaled(RateInt, Operation) ->
    Rand = granderl:uniform(?MAX_UNSIGNED_INT_32),
    case Rand =< RateInt of
        true  ->
            N = Rand rem ?POOL_SIZE + 1,
            operation(Operation, server_name(N));
        false ->
            ok
    end.

-spec server_name(1..4) ->
    atom().

server_name(1) -> statsderl_1;
server_name(2) -> statsderl_2;
server_name(3) -> statsderl_3;
server_name(4) -> statsderl_4.

%% private
cast({cast, _} = Cast, ServerName) ->
    send(ServerName, Cast);
cast(Operation, ServerName) ->
    send(ServerName, {cast, statsderl_protocol:encode(Operation)}).

operation(Operation) ->
    operation(Operation, random_server()).

operation({timing_now, Key, Value}, ServerName) ->
    Value2 = statsderl_utils:timing_now(Value),
    cast({timing, Key, Value2}, ServerName);
operation({timing_now_us, Key, Value}, ServerName) ->
    Value2 = statsderl_utils:timing_now_us(Value),
    cast({timing, Key, Value2}, ServerName);
operation(Operation, ServerName) ->
    cast(Operation, ServerName).

random_server() ->
    server_name(granderl:uniform(?POOL_SIZE)).

send(ServerName, Msg) ->
    try
        ServerName ! Msg,
        ok
    catch
        _:_ ->
            ok
    end.
