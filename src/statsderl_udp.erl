-module(statsderl_udp).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    header/2,
    send/3
]).

-define(INET_AF_INET, 1).
-define(INT16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

%% public
-spec header(inet:ip_address(), inet:port_number()) -> iodata().

-ifdef(UDP_HEADER).

header(IP, Port) ->
    [?INET_AF_INET, ?INT16(Port) | ip4_to_bytes(IP)].

-else.

header(IP, Port) ->
    [?INT16(Port) | ip4_to_bytes(IP)].

-endif.

-spec send(inet:socket(), iodata(), iodata()) ->
    ok | {error, term()}.

send(Socket, Header, Data) ->
    try
        true = erlang:port_command(Socket, [Header, Data]),
        ok
    catch
        Error:Reason ->
            statsderl_utils:error_msg("port_command ~p: ~p~n", [Error, Reason]),
            {error, {Error, Reason}}
    end.

%% private
ip4_to_bytes({A, B, C, D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].
