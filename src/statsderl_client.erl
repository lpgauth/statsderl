-module(statsderl_client).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    init/1,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    base_key :: iodata()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init(undefined) ->
    {ok, state()}.

init(_Opts) ->
    BaseKey = ?ENV(?ENV_BASEKEY, ?DEFAULT_BASEKEY),

    {ok, #state {
        base_key = statsderl_utils:base_key(BaseKey)
    }}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, undefined, iolist(), state()}.

handle_request({cast, Data}, #state {
        base_key = BaseKey
    } = State) ->

    {ok, undefined, [BaseKey, Data], State};
handle_request(Request, #state {
        base_key = BaseKey
    } = State) ->

    Data = statsderl_protocol:encode(Request),

    {ok, undefined, [BaseKey, Data], State}.

-spec handle_data(binary(), state()) ->
    {ok, [], state()}.

handle_data(_Data, State) ->
    {ok, [], State}.

-spec terminate(state()) -> ok.

terminate(_State) ->
    ok.
