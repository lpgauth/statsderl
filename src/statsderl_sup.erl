-module(statsderl_sup).

%% public
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% public
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init(_Args) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(statsderl, worker)
    ]}}.
