-module(statsderl_profile).

-compile({parse_transform, statsderl_transform}).

-export([
    fprofx/0
]).

-define(N, 10000).
-define(P, 20).

%% public
-spec fprofx() -> ok.

fprofx() ->
    statsderl_app:start(),
    [counter() || _ <- lists:seq(1, 100)],
    statsderl_app:stop(),

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    {ok, [statsderl]} = statsderl_app:start(),
    timer:sleep(250),

    Self = self(),
    [spawn(fun () ->
        counter(),
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),
    application:stop(statsderl),

    ok.

%% private
counter() ->
    [statsderl:counter(["test", <<".test">>], 5, 0.25) ||
        _ <- lists:seq(1, ?N)].

wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
