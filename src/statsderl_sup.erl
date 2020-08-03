-module(statsderl_sup).
-include("statsderl.hrl").

%% public
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).


%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

init(_Args) ->
    Hostname = ?ENV(?ENV_HOSTNAME, ?DEFAULT_HOSTNAME),
    PoolSize = ?ENV(pool_size, ?DEFAULT_POOL_SIZE),
    Port = ?ENV(?ENV_PORT, ?DEFAULT_PORT),

    ClientOpts = [
        {address, Hostname},
        {port, Port},
        {protocol, shackle_udp}
    ],
    PoolOtps = [{pool_size, PoolSize}],
    ok = shackle_pool:start(?APP, ?CLIENT, ClientOpts, PoolOtps),

    {ok, {{one_for_one, 5, 10}, []}}.
