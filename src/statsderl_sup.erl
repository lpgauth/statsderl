
-module(statsderl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    init_red(),
    BaseKey = case application:get_env(statsderl, base_key) of
        {ok, Key} -> Key;
        undefined -> ""
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, BaseKey).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(BaseKey) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(statsderl, worker, BaseKey)]} }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_red() ->
    statsderl = ets:new(statsderl, [set, public, named_table, {read_concurrency, true}]),
    true = ets:insert(statsderl, {backlog, 0}).
