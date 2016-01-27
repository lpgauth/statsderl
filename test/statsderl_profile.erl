-module(statsderl_profile).

-export([
    fprofx/0
]).

-define(N, 1000).
-define(P, 100).

%% public
-spec fprofx() -> ok.

fprofx() ->
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") || Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames),

    application:start(statsderl),
    [statsderl:increment(["test", <<".test">>], 1, 0.25) ||
        _ <- lists:seq(1, ?N)],

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    Self = self(),
    [spawn(fun () ->
        [statsderl:increment(["test", <<".test">>], 1, 0.25) ||
            _ <- lists:seq(1, ?N)],
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    application:stop(statsderl),

    ok.

%% private
wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
