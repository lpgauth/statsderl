-module(statsderl_server).

%% public
-export([
    pool_size/0,
    random_server_name/0,
    server_name/1,
    start_link/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-inline([pool_size/0]).
-inline([format/2]).

-record(state, {
    hostname,
    port,
    socket,
    basekey
}).

%% public

pool_size() ->
    2.

random_server_name() ->
    Random = erlang:phash2({os:timestamp(), self()}, pool_size()),
    server_name(Random).

server_name(0) -> statsderl_0;
server_name(1) -> statsderl_1;
server_name(N) -> list_to_atom("statsderl_" ++ integer_to_list(N)).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) ->
    {ok, Hostname} = application:get_env(statsderl, hostname),
    {ok, Port} = application:get_env(statsderl, port),
    BaseKey = get_base_key(application:get_env(statsderl, base_key)),
    {ok, Socket} = init_socket(),

    {ok, #state {
        hostname = lookup_hostname(Hostname),
        port = Port,
        basekey = BaseKey,
        socket = Socket
    }}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({send, Packet}, #state {
        hostname = {A,B,C,D},
        port = Port,
        socket = Socket,
        basekey = BaseKey} = State) ->

    Message = [
        [((Port) bsr 8) band 16#ff, (Port) band 16#ff],
        [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff],
        [BaseKey, Packet]
    ],
    SendFun = erlang:get(socket_send_fun),
    catch SendFun(Socket, Message), %% we don't care if it succeeds or not
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
get_base_key({ok, hostname}) ->
    {ok, Hostname} = inet:gethostname(),
    [Hostname, $.];
get_base_key({ok, sname}) ->
    Name = atom_to_list(node()),
    SName = string:sub_word(Name, 1, $@),
    [SName, $.];
get_base_key({ok, name}) ->
    Name = atom_to_list(node()),
    Value = re:replace(Name, "@", ".", [global, {return, list}]),
    [Value, $.];
get_base_key({ok, Key}) ->
    [Key, $.];
get_base_key(undefined) ->
    <<"">>.

lookup_hostname(Address) when is_tuple(Address) ->
    Address;
lookup_hostname(Hostname) ->
    case inet:gethostbyname(Hostname) of
        {ok, {_, _, _, _, _, [Address | _]}} ->
            Address;
        _Else ->
            {127, 0, 0, 1}
    end.

init_socket() ->
    case socket_module() of
        ?MODULE ->
            erlang:put(socket_send_fun, fun erlang:port_command/2),
            {ok, _Socket} = gen_udp:open(0, [{active, false}]);
        Module ->
            erlang:put(socket_send_fun, fun Module:send/2),
            {ok, _Socket} = Module:open()
    end.

socket_module() ->
    case application:get_env(statsderl, socket_module) of
        {ok, Module} ->
            Module;
        undefined ->
            ?MODULE
    end.
