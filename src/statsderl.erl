-module(statsderl).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    counter/3,
    counter/4,
    decrement/3,
    decrement/4,
    gauge/3,
    gauge/4,
    gauge_decrement/3,
    gauge_decrement/4,
    gauge_increment/3,
    gauge_increment/4,
    increment/3,
    increment/4,
    timing/3,
    timing/4,
    timing_fun/3,
    timing_fun/4,
    timing_now/3,
    timing_now/4,
    timing_now_us/3,
    timing_now_us/4
]).

%% public
-spec counter(key(), value(), sample_rate()) ->
    ok.

counter(Key, Value, Rate) ->
    counter(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec counter(key(), value(), sample_rate(), pool_name()) ->
    ok.

counter(Key, Value, Rate, PoolName) ->
    statsderl_sample:rate(Rate, {counter, Key, Value, Rate}, PoolName).

-spec decrement(key(), value(), sample_rate()) ->
    ok.

decrement(Key, Value, Rate) when Value >= 0 ->
    decrement(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec decrement(key(), value(), sample_rate(), pool_name()) ->
    ok.

decrement(Key, Value, Rate, PoolName) when Value >= 0 ->
    statsderl_sample:rate(Rate, {counter, Key, -Value, Rate}, PoolName).

-spec gauge(key(), value(), sample_rate()) ->
    ok.

gauge(Key, Value, Rate) when Value >= 0 ->
    gauge(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec gauge(key(), value(), sample_rate(), pool_name()) ->
    ok.

gauge(Key, Value, Rate, PoolName) when Value >= 0 ->
    statsderl_sample:rate(Rate, {gauge, Key, Value}, PoolName).

-spec gauge_decrement(key(), value(), sample_rate()) ->
    ok.

gauge_decrement(Key, Value, Rate) when Value >= 0 ->
    gauge_decrement(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec gauge_decrement(key(), value(), sample_rate(), pool_name()) ->
    ok.

gauge_decrement(Key, Value, Rate, PoolName) when Value >= 0 ->
    statsderl_sample:rate(Rate, {gauge_decrement, Key, Value}, PoolName).

-spec gauge_increment(key(), value(), sample_rate()) ->
    ok.

gauge_increment(Key, Value, Rate) when Value >= 0 ->
    gauge_increment(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec gauge_increment(key(), value(), sample_rate(), pool_name()) ->
    ok.

gauge_increment(Key, Value, Rate, PoolName) when Value >= 0 ->
    statsderl_sample:rate(Rate, {gauge_increment, Key, Value}, PoolName).

-spec increment(key(), value(), sample_rate()) ->
    ok.

increment(Key, Value, Rate) when Value >= 0 ->
    increment(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec increment(key(), value(), sample_rate(), pool_name()) ->
    ok.

increment(Key, Value, Rate, PoolName) when Value >= 0 ->
    statsderl_sample:rate(Rate, {counter, Key, Value, Rate}, PoolName).

-spec timing(key(), value(), sample_rate()) ->
    ok.

timing(Key, Value, Rate) ->
    timing(Key, Value, Rate, ?DEFAULT_POOL_NAME).

-spec timing(key(), value(), sample_rate(), pool_name()) ->
    ok.

timing(Key, Value, Rate, PoolName) ->
    statsderl_sample:rate(Rate, {timing, Key, Value}, PoolName).

-spec timing_fun(key(), fun(), sample_rate()) ->
    any().

timing_fun(Key, Fun, Rate) ->
    timing_fun(Key, Fun, Rate, ?DEFAULT_POOL_NAME).

-spec timing_fun(key(), fun(), sample_rate(), pool_name()) ->
    any().

timing_fun(Key, Fun, Rate, PoolName) ->
    Timestamp = statsderl_utils:timestamp(),
    Result = Fun(),
    timing_now(Key, Timestamp, Rate, PoolName),
    Result.

-spec timing_now(key(), erlang:timestamp(), sample_rate()) ->
    ok.

timing_now(Key, Timestamp, Rate) ->
    timing_now(Key, Timestamp, Rate, ?DEFAULT_POOL_NAME).

-spec timing_now(key(), erlang:timestamp(), sample_rate(), pool_name()) ->
    ok.

timing_now(Key, Timestamp, Rate, PoolName) ->
    statsderl_sample:rate(Rate, {timing_now, Key, Timestamp}, PoolName).

-spec timing_now_us(key(), erlang:timestamp(), sample_rate()) ->
    ok.

timing_now_us(Key, Timestamp, Rate) ->
    timing_now_us(Key, Timestamp, Rate, ?DEFAULT_POOL_NAME).

-spec timing_now_us(key(), erlang:timestamp(), sample_rate(), pool_name()) ->
    ok.

timing_now_us(Key, Timestamp, Rate, PoolName) ->
    statsderl_sample:rate(Rate, {timing_now_us, Key, Timestamp}, PoolName).
