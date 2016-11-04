-module(statsderl_server).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    init/2,
    start_link/1
]).

-record(state, {
    header :: iodata(),
    socket :: inet:socket()
}).

%% public
-spec init(pid(), atom()) ->
    no_return().

init(Parent, Name) ->
    BaseKey = ?ENV(?ENV_BASEKEY, ?DEFAULT_BASEKEY),
    Hostname = ?ENV(?ENV_HOSTNAME, ?DEFAULT_HOSTNAME),
    Port = ?ENV(?ENV_PORT, ?DEFAULT_PORT),

    case udp_header(Hostname, Port, BaseKey) of
        {ok, Header} ->
            case gen_udp:open(0, [{active, false}]) of
                {ok, Socket} ->
                    register(Name, self()),
                    proc_lib:init_ack(Parent, {ok, self()}),

                    loop(#state {
                        socket = Socket,
                        header = Header
                    });
                {error, Reason} ->
                    exit(Reason)
            end;
        {error, Reason} ->
            exit(Reason)
    end.

-spec start_link(atom()) ->
    {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

%% private
loop(#state {
        header = Header,
        socket = Socket
    } = State) ->

    receive
        {cast, Packet} ->
            erlang:port_command(Socket, [Header, Packet]),
            loop(State);
        {inet_reply, _Socket, ok} ->
            loop(State);
        {inet_reply, _Socket, {error, Reason}} ->
            statsderl_utils:error_msg("inet_reply error: ~p~n", [Reason]),
            loop(State)
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
