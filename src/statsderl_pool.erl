-module(statsderl_pool).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-ignore_xref([
    {statsderl_pool_foil, lookup, 1}
]).

-export([
    init/0,
    sample/2,
    sample_scaled/2,
    server_name/1,
    size/0
]).

%% public
-spec init() ->
    ok.

init() ->
    foil:new(statsderl_pool),
    PoolSize = ?ENV(pool_size, ?DEFAULT_POOL_SIZE),
    foil:insert(statsderl_pool, pool_size, PoolSize),
    [foil:insert(statsderl_pool, N, server_name_gen(N)) ||
        N <- lists:seq(1, PoolSize)],
    foil:load(statsderl_pool).

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
            N = Rand rem size() + 1,
            operation(Operation, server_name(N));
        false ->
            ok
    end.

-spec server_name(pos_integer()) ->
    atom().

server_name(N) ->
    try statsderl_pool_foil:lookup(N) of
        {ok, Value} ->
            Value;
        {error, _Reason} ->
            undefined
    catch
        error:undef ->
            undefined
    end.

-spec size() ->
    pool_size().

size() ->
    try foil:lookup(?MODULE, pool_size) of
        {ok, Value} ->
            Value;
        {error, _Reason} ->
            1
    catch
        error:undef ->
            1
    end.

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
    server_name(granderl:uniform(size())).

send(ServerName, Msg) ->
    try
        ServerName ! Msg,
        ok
    catch
        _:_ ->
            ok
    end.

server_name_gen(N) ->
    list_to_atom("statsderl_" ++ integer_to_list(N)).
