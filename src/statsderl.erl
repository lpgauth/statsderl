-module(statsderl).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% public
-export([
    decrement/3,
    decrement/4,
    gauge/3,
    gauge/4,
    increment/3,
    increment/4,
    timing/3,
    timing/4,
    timing_fun/3,
    timing_fun/4,
    timing_now/3,
    timing_now/4
]).

-inline([now_diff_ms/1]).

%% public
decrement(Key, Value, SampleRate) ->
    maybe_send(decrement, Key, Value, SampleRate, []).

decrement(Key, Value, SampleRate, Tags) ->
    maybe_send(decrement, Key, Value, SampleRate, Tags).

gauge(Key, Value, SampleRate) ->
    maybe_send(gauge, Key, Value, SampleRate, []).

gauge(Key, Value, SampleRate, Tags) ->
    maybe_send(gauge, Key, Value, SampleRate, Tags).

increment(Key, Value, SampleRate) ->
    maybe_send(increment, Key, Value, SampleRate, []).

increment(Key, Value, SampleRate, Tags) ->
    maybe_send(increment, Key, Value, SampleRate, Tags).

timing(Key, Value, SampleRate) ->
    timing(Key, Value, SampleRate, []).

timing(Key, Value, SampleRate, Tags) ->
    maybe_send(timing, Key, Value, SampleRate, Tags).

timing_fun(Key, Fun, SampleRate) ->
    timing_fun(Key, Fun, SampleRate, []).

timing_fun(Key, Fun, SampleRate, Tags) ->
    Timestamp = os:timestamp(),
    Result = Fun(),
    timing_now(Key, Timestamp, SampleRate, Tags),
    Result.

timing_now(Key, Timestamp, SampleRate) ->
    timing_now(Key, Timestamp, SampleRate, []).

timing_now(Key, Timestamp, SampleRate, Tags) ->
    Duration = now_diff_ms(Timestamp),
    timing(Key, Duration, SampleRate, Tags).

now_diff_ms(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp) div 1000.

%% private
format(value, Value) when is_integer(Value) ->
    integer_to_list(Value);
format(value, Value) when is_float(Value) ->
    io_lib:format("~.2f", [Value]);
format(decrement, Value) ->
    [<<":-">>, format(value, Value), <<"|c">>];
format(increment, Value) ->
    [<<":">>, format(value, Value), <<"|c">>];
format(gauge, Value) ->
    [<<":">>, format(value, Value), <<"|g">>];
format(timing, Value) ->
    [<<":">>, format(value, Value), <<"|ms">>];
format(sample_rate, SampleRate) when SampleRate >= 1 ->
    [];
format(sample_rate, SampleRate) when SampleRate < 1 ->
    [<<"|@">>, io_lib:format("~.3f", [SampleRate * 1.0])];
format(tag, [_|_] = Tags) ->
    [[_, H] | T] = [[$,, X] || X <- Tags],
    [$|, $#, H | T];
format(tag, []) ->
    [].

generate_packet(Type, Key, Value, SampleRate, Tags) ->
    [Key, format(Type, Value), format(sample_rate, SampleRate), format(tag, Tags)].

-ifdef(TEST).
generate_packet_test_() ->
    [?_assertEqual(<<"key:-1|c">>,           iolist_to_binary(generate_packet(decrement,  "key", 1, 1, [])))
    ,?_assertEqual(<<"key:-2|c|#tag1">>,     iolist_to_binary(generate_packet(decrement,  "key", 2, 1, ["tag1"])))
    ,?_assertEqual(<<"key:3|c">>,            iolist_to_binary(generate_packet(increment,  "key", 3, 1, [])))
    ,?_assertEqual(<<"key:4|c|#tag1,tag2">>, iolist_to_binary(generate_packet(increment,  "key", 4, 1, ["tag1", "tag2"])))
    ,?_assertEqual(<<"key:5|g|#tag3">>,      iolist_to_binary(generate_packet(gauge,      "key", 5, 1, ["tag3"])))
    ,?_assertEqual(<<"key:6|ms|#ahhhh">>,    iolist_to_binary(generate_packet(timing,     "key", 6, 1, ["ahhhh"])))
    ,?_assertEqual(<<"key:7|g">>,            iolist_to_binary(generate_packet(gauge,      "key", 7, 1, [])))
    ,?_assertEqual(<<"key:8|ms">>,           iolist_to_binary(generate_packet(timing,     "key", 8, 1, [])))
    ].
-endif.

maybe_seed() ->
    case get(random_seed) of
        undefined ->
            random:seed(os:timestamp());
        {X, X, X} ->
            random:seed(os:timestamp());
        _ ->
            ok
    end.

maybe_send(Method, Key, Value, SampleRate, Tags) when SampleRate >= 1 ->
    send(Method, Key, Value, SampleRate, Tags);
maybe_send(Method, Key, Value, SampleRate, Tags) ->
    maybe_seed(),
    case random:uniform() =< SampleRate of
        true  ->
            send(Method, Key, Value, SampleRate, Tags);
        false ->
            ok
    end.

send(Method, Key, Value, SampleRate, Tags) ->
    Packet = generate_packet(Method, Key, Value, SampleRate, Tags),
    ServerName = statsderl_server:random_server_name(),
    gen_server:cast(ServerName, {send, Packet}).
