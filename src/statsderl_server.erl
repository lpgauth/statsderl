-module(statsderl_server).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    init/2,
    start_link/1
]).

-record(state, {
    headers :: tuple(),
    socket :: inet:socket()
}).

%% public
-spec init(pid(), atom()) -> no_return().

init(Parent, Name) ->
    BaseKey = ?ENV(?ENV_BASEKEY, ?DEFAULT_BASEKEY),
    Hostname = ?ENV(?ENV_HOSTNAME, ?DEFAULT_HOSTNAME),
    {ok, Headers} = case is_list(Hostname)
                    andalso Hostname /= []
                    andalso is_tuple(hd(Hostname)) of
                        true ->
                            generate_udp_headers(Hostname, BaseKey, []);
                        false ->
                            Port = ?ENV(?ENV_PORT, ?DEFAULT_PORT),
                            generate_udp_headers([{Hostname, Port}],
                                BaseKey, [])
                    end,

    case gen_udp:open(0, [{active, false}]) of
        {ok, Socket} ->
            register(Name, self()),
            proc_lib:init_ack(Parent, {ok, self()}),

            loop(#state {
                socket = Socket,
                headers = list_to_tuple(Headers)
            });
        {error, Reason} ->
            exit(Reason)
    end.

-spec start_link(atom()) -> {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

%% private
handle_msg({cast, KeyHash, Packet}, #state {
        headers = Headers,
        socket = Socket
    } = State) ->

    Header = element((KeyHash rem tuple_size(Headers)) + 1, Headers),
    statsderl_udp:send(Socket, Header, Packet),
    {ok, State};
handle_msg({inet_reply, _Socket, ok}, State) ->
    {ok, State};
handle_msg({inet_reply, _Socket, {error, Reason}}, State) ->
    statsderl_utils:error_msg("inet_reply error: ~p~n", [Reason]),
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

generate_udp_headers([], _BaseKey, Acc) ->
    {ok, lists:reverse(Acc)};
generate_udp_headers([{Hostname, Port} | Rest], BaseKey, Acc) ->
    case udp_header(Hostname, Port, BaseKey) of
        {ok, Header} ->
            generate_udp_headers(Rest, BaseKey, [Header | Acc]);
        Error ->
            Error
    end.

udp_header(Hostname, Port, BaseKey) ->
    case statsderl_utils:getaddrs(Hostname) of
        {ok, {A, B, C, D}} ->
            Header = statsderl_udp:header({A, B, C, D}, Port),
            BaseKey2 = statsderl_utils:base_key(BaseKey),
            {ok, [Header, BaseKey2]};
        {error, Reason} ->
            {error, Reason}
    end.
