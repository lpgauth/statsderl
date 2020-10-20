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
    case ?ENV(pools, undefined) of
        undefined ->
            pool_start({default, [
                {backlog_size, ?ENV(backlog_size, ?DEFAULT_BACKLOG_SIZE)},
                {hostname, ?ENV(?ENV_HOSTNAME, ?DEFAULT_HOSTNAME)},
                {pool_size, ?ENV(pool_size, ?DEFAULT_POOL_SIZE)},
                {port, ?ENV(?ENV_PORT, ?DEFAULT_PORT)}
            ]});
        Pools ->
            [pool_start(Pool) || Pool <- Pools]
    end,

    {ok, {{one_for_one, 5, 10}, []}}.

%% private
lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

name(Name) ->
    list_to_atom("statsderl_" ++ atom_to_list(Name)).

pool_start({Name, Opts}) ->
    BacklogSize = lookup(backlog_size, Opts, ?DEFAULT_BACKLOG_SIZE),
    Hostname = lookup(hostname, Opts, ?DEFAULT_HOSTNAME),
    PoolSize = lookup(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    Port = lookup(port, Opts, ?DEFAULT_PORT),

    ClientOpts = [
        {address, Hostname},
        {port, Port},
        {protocol, shackle_udp}
    ],
    PoolOtps = [
        {backlog_size, BacklogSize},
        {pool_size, PoolSize}
    ],
    ok = shackle_pool:start(name(Name), ?CLIENT, ClientOpts, PoolOtps).
