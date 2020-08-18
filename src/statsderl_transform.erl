-module(statsderl_transform).
-include("statsderl.hrl").

-export([
    parse_transform/2
]).

-define(ATOM(Atom), {atom, 0, Atom}).
-define(BIN_ELEMENT(String), {bin_element, 0, String, default, default}).
-define(BINARY(String), {bin, 0, [?BIN_ELEMENT(?STRING(String))]}).
-define(INTEGER(Int), {integer, 0, Int}).
-define(OP_NEGATIVE(X), {op, 0, '-', X}).
-define(STRING(String), {string, 0, String}).
-define(TUPLE(List), {tuple, 0, List}).

-type forms() :: [erl_parse:abstract_form() | erl_parse:form_info()].

%% public
-spec parse_transform(forms(), [compile:option()]) ->
    forms().

parse_transform(Forms, _Options) ->
    parse_trans:plain_transform(fun do_transform/1, Forms).

do_transform({call, _, {_, _, {_, _, ?APP}, Function}, _} = F) ->
    replace(safe_normalize(Function), F);
do_transform(_Form) ->
    continue.

%% private
call(Function, Arg1, Arg2) ->
    {call, 0, {remote, 0,
        {atom, 0, statsderl_sample},
        {atom, 0, Function}}, [Arg1, Arg2]
    }.

encode(Operation) ->
    Encoded = statsderl_protocol:encode(Operation),
    binary_to_list(iolist_to_binary(Encoded)).

not_undefined([]) ->
    true;
not_undefined([undefined | _]) ->
    false;
not_undefined([_ | T]) ->
    not_undefined(T).

op_code(increment) ->
    counter;
op_code(decrement) ->
    counter;
op_code(Function) ->
    Function.

packet(counter, Key, Value, Rate) ->
    Key2 = safe_normalize(Key),
    Value2 = safe_normalize(Value),
    Rate2 = safe_normalize(Rate),

    case not_undefined([Key2, Value2, Rate2]) of
        true ->
            encode({counter, Key2, Value2, Rate2});
        false ->
            undefined
    end;
packet(OpCode, Key, Value, _Rate) ->
    Key2 = safe_normalize(Key),
    Value2 = safe_normalize(Value),

    case not_undefined([Key2, Value2]) of
        true ->
            encode({OpCode, Key2, Value2});
        false ->
            undefined
    end.

rate_scaled({float, _, RateValue}) ->
    trunc(RateValue * ?MAX_UNSIGNED_INT_32);
rate_scaled({integer, _, RateValue}) ->
    trunc(RateValue * ?MAX_UNSIGNED_INT_32);
rate_scaled(_) ->
    undefined.

replace(timing_fun, F) ->
    F;
replace(Function, {_, _, _, [Key, Value, Rate]} = F) ->
    RateScaled = rate_scaled(Rate),
    OpCode = op_code(Function),
    Value2 = value(Function, Value),
    Packet = packet(OpCode, Key, Value2, Rate),

    case {RateScaled, Packet} of
        {undefined, undefined} ->
            F;
        {_, undefined} ->
            case OpCode of
                counter ->
                    sample_scaled(RateScaled,
                        ?TUPLE([?ATOM(OpCode), Key, Value2, Rate]));
                _ ->
                    sample_scaled(RateScaled,
                        ?TUPLE([?ATOM(OpCode), Key, Value2]))
            end;
        {undefined, _} ->
            sample(Rate,
                ?TUPLE([?ATOM(cast), ?BINARY(Packet)]));
        _ ->
            sample_scaled(RateScaled,
                ?TUPLE([?ATOM(cast), ?BINARY(Packet)]))
    end.

sample(Rate, Arguments) ->
    call(rate, Rate, Arguments).

sample_scaled(RateScaled, Arguments) ->
    call(rate_scaled, ?INTEGER(RateScaled), Arguments).

safe_normalize(AbsTerm) ->
    try erl_parse:normalise(AbsTerm)
    catch
        _:_ ->
            undefined
    end.

value(decrement, Value) ->
    ?OP_NEGATIVE(Value);
value(_, Value) ->
    Value.
