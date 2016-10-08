%% @doc Functions to send data to a StatsD server. In this module, the
%% SampleRate parameter is a float value between 0 and 1 that
%% indicates how frequently the data should actually be sent. For
%% counters, the server takes the sample rate into account.

-module(statsderl).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    counter/3,
    increment/3,
    decrement/3,
    gauge/3,
    gauge_decrement/3,
    gauge_increment/3,
    timing/3,
    timing_fun/3,
    timing_now/3,
    timing_now_us/3
]).

%% public
-spec counter(key(), value(), sample_rate()) -> ok.
%% @doc Increment a counter (identical to increment/3).
counter(Key, Value, SampleRate) ->
    maybe_cast(counter, Key, Value, SampleRate).

-spec increment(key(), value(), sample_rate()) -> ok.
%% @doc Increment a counter.
increment(Key, Value, SampleRate) when Value >= 0 ->
    maybe_cast(counter, Key, Value, SampleRate).

-spec decrement(key(), value(), sample_rate()) -> ok.
%% @doc Decrement a counter.
decrement(Key, Value, SampleRate) when Value >= 0 ->
    maybe_cast(counter, Key, -Value, SampleRate).

-spec gauge(key(), value(), sample_rate()) -> ok.
%% @doc Set a gauge value.
gauge(Key, Value, SampleRate) when Value >= 0 ->
    maybe_cast(gauge, Key, Value, SampleRate).

-spec gauge_decrement(key(), value(), sample_rate()) -> ok.
%% @doc Decrement a gauge value.
gauge_decrement(Key, Value, SampleRate) when Value >= 0 ->
    maybe_cast(gauge_decrement, Key, Value, SampleRate).

-spec gauge_increment(key(), value(), sample_rate()) -> ok.
%% @doc Increment a gauge value.
gauge_increment(Key, Value, SampleRate) when Value >= 0 ->
    maybe_cast(gauge_increment, Key, Value, SampleRate).

-spec timing(key(), value(), sample_rate()) -> ok.
%% @doc Record timer information.
timing(Key, Value, SampleRate) ->
    maybe_cast(timing, Key, Value, SampleRate).

-spec timing_fun(key(), fun(), sample_rate()) -> ok.
%% @doc Run nullary function and record the time spent.
timing_fun(Key, Fun, SampleRate) ->
    Timestamp = statsderl_utils:timestamp(),
    Result = Fun(),
    timing_now(Key, Timestamp, SampleRate),
    Result.

-spec timing_now(key(), erlang:timestamp(), sample_rate()) -> ok.
%% @doc Record time spent between the timestamp passed and now.
timing_now(Key, Timestamp, SampleRate) ->
    maybe_cast(timing_now, Key, Timestamp, SampleRate).

-spec timing_now_us(key(), erlang:timestamp(), sample_rate()) -> ok.

timing_now_us(Key, Timestamp, SampleRate) ->
    maybe_cast(timing_now_us, Key, Timestamp, SampleRate).

%% private
cast(OpCode, Key, Value, SampleRate) ->
    ServerName = statsderl_utils:random_server(),
    cast(OpCode, Key, Value, SampleRate, ServerName).

cast(OpCode, Key, Value, SampleRate, ServerName) ->
    Packet = statsderl_protocol:encode(OpCode, Key, Value, SampleRate),
    send(ServerName, {cast, Packet}).

maybe_cast(timing_now, Key, Value, 1) ->
    cast(timing, Key, timing_now(Value), 1);
maybe_cast(timing_now_us, Key, Value, 1) ->
    cast(timing, Key, timing_now_us(Value), 1);
maybe_cast(OpCode, Key, Value, 1) ->
    cast(OpCode, Key, Value, 1);
maybe_cast(timing_now, Key, Value, 1.0) ->
    cast(timing, Key, timing_now(Value), 1.0);
maybe_cast(timing_now_us, Key, Value, 1.0) ->
    cast(timing, Key, timing_now_us(Value), 1.0);
maybe_cast(OpCode, Key, Value, 1.0) ->
    cast(OpCode, Key, Value, 1);
maybe_cast(OpCode, Key, Value, SampleRate) ->
    Rand = statsderl_utils:random(?MAX_UNSIGNED_INT_32),
    case Rand =< SampleRate * ?MAX_UNSIGNED_INT_32 of
        true  ->
            N = Rand rem ?POOL_SIZE + 1,
            ServerName = statsderl_utils:server_name(N),
            case OpCode of
                timing_now ->
                    cast(timing, Key, timing_now(Value), SampleRate,
                        ServerName);
                timing_now_us ->
                    cast(timing, Key, timing_now_us(Value), SampleRate,
                        ServerName);
                _ ->
                    cast(OpCode, Key, Value, SampleRate, ServerName)
            end;
        false ->
            ok
    end.

send(ServerName, Msg) ->
    try
        ServerName ! Msg,
        ok
    catch
        error:badarg ->
            ok
    end.

timing_now(Timestamp) ->
    timing_now_us(Timestamp) div 1000.

timing_now_us(Timestamp) ->
    Timestamp2 = statsderl_utils:timestamp(),
    timer:now_diff(Timestamp2, Timestamp).
