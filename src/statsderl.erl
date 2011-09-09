-module(statsderl).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(LOW_BACKLOG, 5000).
-define(HIGH_BACKLOG, 10000).

-record(state, {
    hostname,
    port,
    socket
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, increment/3, decrement/3, timing/3]).

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
    
increment(Key, Magnitude, SampleRate) ->
    Stats = io_lib:format("~s:~B|c|@~f", [Key, Magnitude, SampleRate]),
    udp_send(Stats, SampleRate).
    
decrement(Key, Magnitude, SampleRate) ->
    Stats = io_lib:format("~s:-~B|c|@~f", [Key, Magnitude, SampleRate]),
    udp_send(Stats, SampleRate).

timing(Key, Timestamp, SampleRate) ->
    Timing = timer:now_diff(erlang:now(), Timestamp) div 1000,
    Stats = io_lib:format("~s:~B|ms|@~f", [Key, Timing, SampleRate]),
    udp_send(Stats, SampleRate).    

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Hostname} = application:get_env(statsderl, hostname),
    {ok, Port} = application:get_env(statsderl, port),
    {ok, Socket} = gen_udp:open(0, [{active, false}]),
    State = #state {
        hostname = Hostname,
        port = Port,
        socket = Socket  
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({udp_send, Stats}, 
    #state{hostname=Hostname, port=Port, socket=Socket}=State) ->
    gen_udp:send(Socket, Hostname, Port, Stats),
    decrease_backlog(),
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

udp_send(Stats, SampleRate) ->
    case sample(SampleRate) of
        true ->
            increase_backlog(),
            gen_server:cast(?MODULE, {udp_send, Stats});
        false ->
            ok
    end.

sample(SampleRate) ->
    case red() of
        true ->
            false;
        false ->
            random:seed(erlang:now()),
            random:uniform() =< SampleRate
    end.
    
%% ------------------------------------------------------------------
%% Random Early Drop
%% http://en.wikipedia.org/wiki/Random_early_detection
%% ------------------------------------------------------------------

red() ->
    Requests = ets:lookup_element(statsderl, backlog, 2),
    case Requests of
        _ when ?LOW_BACKLOG >= Requests->
            false;
        _ when Requests >= ?HIGH_BACKLOG ->
            true;
        _ ->
            random_drop_function(Requests)
    end.

random_drop_function(Requests) ->
    Distribution = ?HIGH_BACKLOG - Requests,
    case erlang:phash2({self(), now()}, Distribution) + 1 of
        Distribution ->
            true;
        _ ->
            false
    end.
    
increase_backlog() ->
    ets:update_counter(statsderl, backlog, 1).

decrease_backlog() ->
    ets:update_counter(statsderl, backlog, -1). 
