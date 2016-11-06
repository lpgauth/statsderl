-module(statsderl_utils).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    base_key/1,
    error_msg/2,
    getaddrs/1,
    random_element/1,
    timestamp/0,
    timing_now/1,
    timing_now_us/1
]).

%% public
-spec base_key(base_key()) ->
    iodata().

base_key(hostname) ->
    [hostname(), $.];
base_key(name) ->
    [name(), $.];
base_key(sname) ->
    [sname(), $.];
base_key(undefined) ->
    "";
base_key(Key) when is_binary(Key) ->
    [Key, $.];
base_key([]) ->
    [];
base_key([H | T] = Key) ->
    case io_lib:printable_unicode_list(Key) of
        true ->
            [Key, $.];
        false ->
            [base_key(H) | base_key(T)]
    end.

-spec error_msg(string(), [term()]) ->
    ok.

error_msg(Format, Data) ->
    error_logger:error_msg("[statsderl] " ++ Format, Data).

-spec getaddrs(inet:ip_address() | inet:hostname()) ->
    {ok, inet:ip_address()} | {error, atom()}.

getaddrs({_, _, _, _} = Address) ->
    {ok, Address};
getaddrs(Hostname) when is_binary(Hostname) ->
    getaddrs(binary_to_list(Hostname));
getaddrs(Hostname) ->
    case inet:getaddrs(Hostname, inet) of
        {ok, Addrs} ->
            {ok, random_element(Addrs)};
        {error, Reason} ->
            error_msg("getaddrs error: ~p~n", [Reason]),
            {error, Reason}
    end.

-spec random_element([term()]) ->
    term().

random_element([Element]) ->
    Element;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    Index = granderl:uniform(tuple_size(T)),
    element(Index, T).

-spec timestamp() ->
    erlang:timestamp().

timestamp() ->
    os:timestamp().

-spec timing_now(erlang:timestamp()) ->
    non_neg_integer().

timing_now(Timestamp) ->
    timing_now_us(Timestamp) div 1000.

-spec timing_now_us(erlang:timestamp()) ->
    non_neg_integer().

timing_now_us(Timestamp) ->
    timer:now_diff(statsderl_utils:timestamp(), Timestamp).

%% private
hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

name() ->
    Name = atom_to_list(node()),
    re:replace(Name, "@", ".", [global, {return, list}]).

sname() ->
    string:sub_word(atom_to_list(node()), 1, $@).
