-module(statsderl_sup).

%% public
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

-define(CHILD(Name, Mod), {Name, {Mod, start_link, [Name]}, permanent, 5000, worker, [Mod]}).

%% public
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init(_Args) ->
    PoolSize = statsderl:pool_size(),
    {ok, {{one_for_one, 5, 10}, child_specs(PoolSize)}}.

%% private
child_specs(0) ->
    [];
child_specs(N) ->
    Name = statsderl:server_name(N),
    [?CHILD(Name, statsderl) | child_specs(N - 1)].
