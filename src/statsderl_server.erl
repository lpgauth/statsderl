-module(statsderl_server).
-include("statsderl.hrl").

-export([
    init/2,
    start_link/1
]).

-record(state, {
    header :: binary(),
    socket :: inet:socket()
}).

%% public
-spec init(pid(), atom()) -> no_return().

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

-spec start_link(atom()) -> {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

%% private
handle_msg({cast, Packet}, #state {
        header = Header,
        socket = Socket
    } = State) ->

    try
        erlang:port_command(Socket, [Header, Packet])
    catch
        Error:Reason ->
            statsderl_utils:error_msg("port_command ~p: ~p~n", [Error, Reason])
    end,
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

udp_header(Hostname, Port, BaseKey) ->
    case statsderl_utils:getaddrs(Hostname) of
        {ok, {A, B, C, D}} ->
            {ok, iolist_to_binary([
                [((Port) bsr 8) band 16#ff, (Port) band 16#ff],
                [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff],
                statsderl_utils:base_key(BaseKey)
            ])};
        {error, Reason} ->
            {error, Reason}
    end.
