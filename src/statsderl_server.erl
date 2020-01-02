-module(statsderl_server).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    start_link/1
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {
    base_key :: iodata(),
    socket   :: inet:socket()
}).

%% public
-spec start_link(atom()) ->
    {ok, pid()}.

start_link(Name) ->
    metal:start_link(?SERVER, Name, undefined).

%% metal callbacks
-spec init(atom(), pid(), term()) ->
    {ok, term()} | {stop, atom()}.

init(_Name, _Parent, _Opts) ->
    case gen_udp:open(0, [{active, false}]) of
        {ok, Socket} ->
            BaseKey = ?ENV(?ENV_BASEKEY, ?DEFAULT_BASEKEY),
            Hostname = ?ENV(?ENV_HOSTNAME, ?DEFAULT_HOSTNAME),
            Port = ?ENV(?ENV_PORT, ?DEFAULT_PORT),
            gen_udp:connect(Socket, Hostname, Port),

            {ok, #state {
                base_key = statsderl_utils:base_key(BaseKey),
                socket = Socket
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_msg(term(), term()) ->
    {ok, term()}.

handle_msg({cast, Packet}, #state {
        base_key = BaseKey,
        socket = Socket
    } = State) ->

    gen_udp:send(Socket, [BaseKey, Packet]),
    {ok, State}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, #state {
        socket = Socket
    }) ->

    gen_udp:close(Socket),
    ok.
