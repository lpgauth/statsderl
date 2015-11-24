-module(statsderl).
-include("statsderl.hrl").

%% public
-export([
    decrement/3,
    gauge/3,
    gauge_decrement/3,
    gauge_increment/3,
    increment/3,
    timing/3,
    timing_fun/3,
    timing_now/3
]).

%% public
-spec decrement(iodata(), integer(), float()) -> ok.

decrement(Key, Value, SampleRate) ->
    maybe_cast(decrement, Key, Value, SampleRate).

-spec gauge(iodata(), integer(), float()) -> ok.

gauge(Key, Value, SampleRate) ->
    maybe_cast(gauge, Key, Value, SampleRate).

-spec gauge_decrement(iodata(), integer(), float()) -> ok.

gauge_decrement(Key, Value, SampleRate) ->
    maybe_cast(gauge_decrement, Key, Value, SampleRate).

-spec gauge_increment(iodata(), integer(), float()) -> ok.

gauge_increment(Key, Value, SampleRate) ->
    maybe_cast(gauge_increment, Key, Value, SampleRate).

-spec increment(iodata(), integer(), float()) -> ok.

increment(Key, Value, SampleRate) ->
    maybe_cast(increment, Key, Value, SampleRate).

-spec timing(iodata(), integer(), float()) -> ok.

timing(Key, Value, SampleRate) ->
    maybe_cast(timing, Key, Value, SampleRate).

-spec timing_fun(iodata(), fun(), float()) -> ok.

timing_fun(Key, Fun, SampleRate) ->
    Timestamp = os:timestamp(),
    Result = Fun(),
    timing_now(Key, Timestamp, SampleRate),
    Result.

-spec timing_now(iodata(), erlang:timestamp(), float()) -> ok.

timing_now(Key, Timestamp, SampleRate) ->
    timing(Key, stastderl_utils:now_diff_ms(Timestamp), SampleRate).

%% private
cast(OpCode, Key, Value, SampleRate) ->
    Packet = statsderl_protocol:encode(OpCode, Key, Value, SampleRate),
    statsderl_utils:random_server() ! {cast, Packet},
    ok.

maybe_cast(OpCode, Key, Value, 1) ->
    cast(OpCode, Key, Value, 1);
maybe_cast(OpCode, Key, Value, 1.0) ->
    cast(OpCode, Key, Value, 1);
maybe_cast(OpCode, Key, Value, SampleRate) ->
    case statsderl_utils:random(100) + 1 =< SampleRate * 100 of
        true  ->
            cast(OpCode, Key, Value, SampleRate);
        false ->
            ok
    end.
