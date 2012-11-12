-module(statsderl).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {
    hostname,
    port,
    socket,
    basekey
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, increment/3, decrement/3, timing/3,
        timing_now/3, gauge/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

increment(Key, Value, SampleRate) ->
    send(increment, Key, Value, SampleRate).

decrement(Key, Value, SampleRate) ->
    send(decrement, Key, Value, SampleRate).

timing(Key, Value, SampleRate) ->
    send(timing, Key, Value, SampleRate).

timing_now(Key, Timestamp, SampleRate) ->
    timing(Key, now_diff_ms(Timestamp), SampleRate).

gauge(Key, Value, SampleRate) ->
    send(gauge, Key, Value, SampleRate).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Hostname} = application:get_env(statsderl, hostname),
    {ok, Port} = application:get_env(statsderl, port),
    BaseKey = case application:get_env(statsderl, base_key) of
        {ok, Key} -> [Key, $.];
        undefined -> <<"">>
    end,
    {ok, Socket} = gen_udp:open(0, [{active, false}]),
    State = #state {
        hostname = Hostname,
        port = Port,
        basekey = BaseKey,
        socket = Socket
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({send, Packet}, State=#state{hostname=Hostname,
        port=Port, socket=Socket, basekey=BaseKey}) ->
    gen_udp:send(Socket, Hostname, Port, [BaseKey, Packet]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send(Method, Key, Value, 1) ->
    send_packet(Method, Key, Value, 1);

send(Method, Key, Value, 1.0) ->
    send_packet(Method, Key, Value, 1.0);

send(Method, Key, Value, SampleRate) ->
    maybe_seed(),
    case random:uniform() =< SampleRate of
        true  -> send_packet(Method, Key, Value, SampleRate);
        false -> ok
    end.

send_packet(Method, Key, Value, SampleRate) ->
    Packet = generate_packet(Method, Key, Value, SampleRate),
    gen_server:cast(?MODULE, {send, Packet}).

now_diff_ms(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp) div 1000.

generate_packet(Method, Key, Value, SampleRate) ->
    BinSampleRate =
        case SampleRate >= 1 of
            true ->
                <<"">>;
            false ->
                [<<"|@">>, io_lib:format("~.3f", [SampleRate])]
        end,
    BinValue = list_to_binary(integer_to_list(Value)),
    case Method of
        increment ->
            [Key, <<":">>, BinValue, <<"|c">>, BinSampleRate];
        decrement ->
            [Key, <<":-">>, BinValue, <<"|c">>, BinSampleRate];
        timing ->
            [Key, <<":">>, BinValue, <<"|ms">>, BinSampleRate];
        gauge ->
            [Key, <<":">>, BinValue, <<"|g">>, BinSampleRate]
    end.

%% this check verifies whether a seed is already placed
%% in the process dictionary for the random module -- if
%% it is, we don't re-seed for any reason, except if the
%% seed is bad (say, {X,X,X} -- usually {0,0,0} and {1,1,1}
%% for the default seed
maybe_seed() ->
    case get(random_seed) of
        undefined ->
            random:seed(erlang:now());
        {X,X,X} ->
            random:seed(erlang:now());
        _ ->
            ok
    end.
