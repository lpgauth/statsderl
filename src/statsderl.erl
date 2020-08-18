-module(statsderl).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    counter/3,
    decrement/3,
    gauge/3,
    gauge_decrement/3,
    gauge_increment/3,
    increment/3,
    timing/3,
    timing_fun/3,
    timing_now/3,
    timing_now_us/3
]).

%% public
-spec counter(key(), value(), sample_rate()) ->
    ok.

counter(Key, Value, Rate) ->
    statsderl_sample:rate(Rate, {counter, Key, Value, Rate}).

-spec decrement(key(), value(), sample_rate()) ->
    ok.

decrement(Key, Value, Rate) when Value >= 0 ->
    statsderl_sample:rate(Rate, {counter, Key, -Value, Rate}).

-spec gauge(key(), value(), sample_rate()) ->
    ok.

gauge(Key, Value, Rate) when Value >= 0 ->
    statsderl_sample:rate(Rate, {gauge, Key, Value}).

-spec gauge_decrement(key(), value(), sample_rate()) ->
    ok.

gauge_decrement(Key, Value, Rate) when Value >= 0 ->
    statsderl_sample:rate(Rate, {gauge_decrement, Key, Value}).

-spec gauge_increment(key(), value(), sample_rate()) ->
    ok.

gauge_increment(Key, Value, Rate) when Value >= 0 ->
    statsderl_sample:rate(Rate, {gauge_increment, Key, Value}).

-spec increment(key(), value(), sample_rate()) ->
    ok.

increment(Key, Value, Rate) when Value >= 0 ->
    statsderl_sample:rate(Rate, {counter, Key, Value, Rate}).

-spec timing(key(), value(), sample_rate()) ->
    ok.

timing(Key, Value, Rate) ->
    statsderl_sample:rate(Rate, {timing, Key, Value}).

-spec timing_fun(key(), fun(), sample_rate()) ->
    any().

timing_fun(Key, Fun, Rate) ->
    Timestamp = statsderl_utils:timestamp(),
    Result = Fun(),
    timing_now(Key, Timestamp, Rate),
    Result.

-spec timing_now(key(), erlang:timestamp(), sample_rate()) ->
    ok.

timing_now(Key, Timestamp, Rate) ->
    statsderl_sample:rate(Rate, {timing_now, Key, Timestamp}).

-spec timing_now_us(key(), erlang:timestamp(), sample_rate()) ->
    ok.

timing_now_us(Key, Timestamp, Rate) ->
    statsderl_sample:rate(Rate, {timing_now_us, Key, Timestamp}).
