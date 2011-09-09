
-module(statsderl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    init_red(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(statsderl, worker)]} }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_red() ->
    statsderl = ets:new(statsderl, [set, public, named_table, {read_concurrency, true}]),
    true = ets:insert(statsderl, {backlog, 0}).
