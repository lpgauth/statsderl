-module(statsderl_server).
-include("statsderl.hrl").

-export([
    init/2,
    start_link/1
]).

-record(state, {
    header :: iolist(),
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
base_key(hostname) ->
    {ok, Hostname} = inet:gethostname(),
    [Hostname, $.];
base_key(name) ->
    Name = atom_to_list(node()),
    Value = re:replace(Name, "@", ".", [global, {return, list}]),
    [Value, $.];
base_key(sname) ->
    Name = atom_to_list(node()),
    SName = string:sub_word(Name, 1, $@),
    [SName, $.];
base_key(undefined) ->
    "";
base_key(BaseKey) ->
    [BaseKey, $.].

getaddrs({_, _, _, _} = Address) ->
    {ok, Address};
getaddrs(Hostname) when is_binary(Hostname) ->
    getaddrs(binary_to_list(Hostname));
getaddrs(Hostname) ->
    case statsderl_utils:inet_getaddrs(Hostname) of
        {ok, Addrs} ->
            {ok, statsderl_utils:random_element(Addrs)};
        {error, Reason} ->
            statsderl_utils:error_msg("[statsderl] getaddrs error: ~p~n", [Reason]),
            {error, Reason}
    end.

handle_msg({cast, Packet}, #state {
        header = Header,
        socket = Socket
    } = State) ->

    try
        erlang:port_command(Socket, [Header, Packet])
    catch
        Error:Reason ->
            statsderl_utils:error_msg("[statsderl] port_command ~p: ~p~n", [Error, Reason])
    end,
    {ok, State};
handle_msg({inet_reply, _Socket, ok}, State) ->
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

udp_header(Hostname, Port, BaseKey) ->
    case getaddrs(Hostname) of
        {ok, {A, B, C, D}} ->
            {ok, [
                [((Port) bsr 8) band 16#ff, (Port) band 16#ff],
                [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff],
                base_key(BaseKey)
            ]};
        {error, Reason} ->
            {error, Reason}
    end.
