-module(statsderl_protocol).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    encode/4
]).

%% public
-spec encode(op_code(), key(), value(), sample_rate()) -> iodata().

encode(counter, Key, Value, SampleRate) ->
    [Key, <<":">>, format_value(Value), <<"|c">>,
        format_sample_rate(SampleRate)];
encode(gauge, Key, Value, _SampleRate) ->
    [Key, <<":">>, format_value(Value), <<"|g">>];
encode(gauge_decrement, Key, Value, _SampleRate) ->
    [Key, <<":-">>, format_value(Value), <<"|g">>];
encode(gauge_increment, Key, Value, _SampleRate) ->
    [Key, <<":+">>, format_value(Value), <<"|g">>];
encode(timing, Key, Value, _SampleRate) ->
    [Key, <<":">>, format_value(Value), <<"|ms">>].

%% private
format_sample_rate(SampleRate) when SampleRate >= 1 ->
    <<>>;
format_sample_rate(SampleRate) ->
    [<<"|@">>, float_to_list(SampleRate, [{decimals, 3}])].

format_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]).
