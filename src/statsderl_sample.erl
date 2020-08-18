-module(statsderl_sample).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    rate/2,
    rate_scaled/2
]).

%% public
-spec rate(sample_rate(), operation()) ->
    ok.

rate(1, Operation) ->
    operation(Operation);
rate(1.0, Operation) ->
    operation(Operation);
rate(Rate, Operation) ->
    RateInt = trunc(Rate * ?MAX_UNSIGNED_INT_32),
    rate_scaled(RateInt, Operation).

-spec rate_scaled(non_neg_integer(), operation()) ->
    ok.

rate_scaled(RateInt, Operation) ->
    Rand = granderl:uniform(?MAX_UNSIGNED_INT_32),
    case Rand =< RateInt of
        true  ->
            operation(Operation);
        false ->
            ok
    end.

%% private
cast(Request) ->
    shackle:cast(?APP, Request, undefined),
    ok.

operation({timing_now, Key, Value}) ->
    Value2 = statsderl_utils:timing_now(Value),
    cast({timing, Key, Value2});
operation({timing_now_us, Key, Value}) ->
    Value2 = statsderl_utils:timing_now_us(Value),
    cast({timing, Key, Value2});
operation(Operation) ->
    cast(Operation).
