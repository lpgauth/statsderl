-module(statsderl_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% application callbacks
start(_StartType, _StartArgs) ->
    statsderl_sup:start_link().

stop(_State) ->
    ok.
