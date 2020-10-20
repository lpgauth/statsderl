-module(statsderl_sample).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    rate/3,
    rate_scaled/3
]).

%% public
-spec rate(sample_rate(), operation(), pool_name()) ->
    ok.

rate(1, Operation, PoolName) ->
    operation(Operation, PoolName);
rate(1.0, Operation, PoolName) ->
    operation(Operation, PoolName);
rate(Rate, Operation, PoolName) ->
    RateInt = trunc(Rate * ?MAX_UNSIGNED_INT_32),
    rate_scaled(RateInt, Operation, PoolName).

-spec rate_scaled(non_neg_integer(), operation(), pool_name()) ->
    ok.

rate_scaled(RateInt, Operation, PoolName) ->
    Rand = granderl:uniform(?MAX_UNSIGNED_INT_32),
    case Rand =< RateInt of
        true  ->
            operation(Operation, PoolName);
        false ->
            ok
    end.

%% private
cast(Request, PoolName) ->
    shackle:cast(PoolName, Request, undefined),
    ok.

operation({timing_now, Key, Value}, PoolName) ->
    Value2 = statsderl_utils:timing_now(Value),
    cast({timing, Key, Value2}, PoolName);
operation({timing_now_us, Key, Value}, PoolName) ->
    Value2 = statsderl_utils:timing_now_us(Value),
    cast({timing, Key, Value2}, PoolName);
operation(Operation, PoolName) ->
    cast(Operation, PoolName).
