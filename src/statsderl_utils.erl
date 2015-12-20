-module(statsderl_utils).
-include("statsderl.hrl").

-export([
    base_key/1,
    error_msg/2,
    getaddrs/1,
    random/1,
    random_element/1,
    random_server/0,
    server_name/1,
    timestamp/0
]).

%% public
-spec base_key(base_key()) -> iodata().

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

-spec error_msg(string(), [term()]) -> ok.

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
            {ok, statsderl_utils:random_element(Addrs)};
        {error, Reason} ->
            statsderl_utils:error_msg("getaddrs error: ~p~n", [Reason]),
            {error, Reason}
    end.

-spec random(pos_integer()) -> pos_integer().

random(N) ->
    erlang:phash2({self(), timestamp()}, N) + 1.

-spec random_element([term()]) -> term().

random_element([Element]) ->
    Element;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    Index = statsderl_utils:random(tuple_size(T)),
    element(Index, T).

-spec random_server() -> atom().

random_server() ->
    server_name(random(?POOL_SIZE)).

-spec server_name(pos_integer()) -> atom().

server_name(1) -> statsderl_1;
server_name(2) -> statsderl_2;
server_name(3) -> statsderl_3;
server_name(4) -> statsderl_4.

-spec timestamp() -> erlang:timestamp().

timestamp() ->
    os:timestamp().

%% private
hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

name() ->
    Name = atom_to_list(node()),
    re:replace(Name, "@", ".", [global, {return, list}]).

sname() ->
    string:sub_word(atom_to_list(node()), 1, $@).
