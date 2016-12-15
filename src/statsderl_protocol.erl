-module(statsderl_protocol).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    encode/1
]).

%% public
-spec encode(operation()) -> iodata().

encode({counter, Key, Value, SampleRate}) ->
    [Key, <<":">>, format_value(Value), <<"|c">>,
        format_sample_rate(SampleRate)];
encode({gauge, Key, Value}) ->
    [Key, <<":">>, format_value(Value), <<"|g">>];
encode({gauge_decrement, Key, Value}) ->
    [Key, <<":-">>, format_value(Value), <<"|g">>];
encode({gauge_increment, Key, Value}) ->
    [Key, <<":+">>, format_value(Value), <<"|g">>];
encode({timing, Key, Value}) ->
    [Key, <<":">>, format_value(Value), <<"|ms">>].

%% private
format_sample_rate(SampleRate) when SampleRate >= 1 ->
    <<>>;
format_sample_rate(SampleRate) ->
    [<<"|@">>, float_to_list(SampleRate, [compact, {decimals, 6}])].

format_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]).
