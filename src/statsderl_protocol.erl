-module(statsderl_protocol).
-include("statsderl.hrl").

-export([
    encode/4
]).

%% public
-spec encode(op_code(), iodata(), iodata(), float()) -> iodata().

encode(decrement, Key, Value, SampleRate) when SampleRate >= 1 ->
    [Key, ":-", Value, "|c"];
encode(decrement, Key, Value, SampleRate) ->
    [Key, ":-", Value, "|c", format_sample_rate(SampleRate)];
encode(gauge, Key, Value, _SampleRate) ->
    [Key, ":", Value, "|g"];
encode(gauge_decrement, Key, Value, _SampleRate) ->
    [Key, ":-", Value, "|g"];
encode(gauge_increment, Key, Value, _SampleRate) ->
    [Key, ":+", Value, "|g"];
encode(increment, Key, Value, SampleRate) when SampleRate >= 1 ->
    [Key, ":", Value, "|c"];
encode(increment, Key, Value, SampleRate) ->
    [Key, ":", Value, "|c", format_sample_rate(SampleRate)];
encode(timing, Key, Value, _SampleRate) ->
    [Key, ":", Value, "|ms"].

%% private
format_sample_rate(SampleRate) ->
    % TODO: optimize me
    ["|@", io_lib:format("~.3f", [SampleRate])].
