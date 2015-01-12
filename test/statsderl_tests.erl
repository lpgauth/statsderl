-module(statsderl_tests).
-compile(debug_info).
-include_lib("eunit/include/eunit.hrl").

-export([
    open/0,
    send/2
]).

%% statsderl_server callbacks
open() ->
    ets:update_counter(?MODULE, open, 1),
    {ok, socket}.

send(Socket, _Message) ->
    ets:update_counter(?MODULE, send, 1),
    ?assertEqual(socket, Socket),
    ok.

%% helpers
this_dir() ->
    Compile = module_info(compile),
    {_, Filename} = lists:keyfind(source, 1, Compile),
    filename:dirname(Filename).

update_code_path() ->
    ThisDir = this_dir(),
    AppDir = filename:dirname(ThisDir),
    EbinDir = filename:join(AppDir, "ebin"),
    ?assert(code:add_pathz(EbinDir)).

-define(check_and_reset(Open, Send),
        begin
            timer:sleep(100), %% race condition :-/
            ?assertEqual([{open, Open}], ets:lookup(?MODULE, open)),
            ?assertEqual([{send, Send}], ets:lookup(?MODULE, send)),
            ets:insert(?MODULE, [{open, 0}, {send, 0}])
        end).

%% tests
app_test() ->
    ets:new(?MODULE, [named_table, public]),
    ets:insert(?MODULE, [{open, 0}, {send, 0}]),
    update_code_path(), %% we need the .app file
    application:start(kernel),
    application:start(stdlib),
    application:load(statsderl),
    application:set_env(statsderl, socket_module, ?MODULE),
    ?assertEqual(ok, application:start(statsderl)),
    ?check_and_reset(statsderl_server:pool_size(), 0),

    SampleRate = 1,
    Tags = ["dummy.tag"],
    Fun = fun erlang:yield/0,
    Timestamp = os:timestamp(),

    statsderl:decrement("statsderl.dummy.counter", 1, SampleRate),
    ?check_and_reset(0, 1),

    statsderl:decrement("statsderl.dummy.counter", 1, SampleRate, Tags),
    ?check_and_reset(0, 1),

    statsderl:gauge("statsderl.dummy.gauge", 1, SampleRate),
    ?check_and_reset(0, 1),

    statsderl:gauge("statsderl.dummy.gauge", 1, SampleRate, Tags),
    ?check_and_reset(0, 1),

    statsderl:increment("statsderl.dummy.counter", 1, SampleRate),
    ?check_and_reset(0, 1),

    statsderl:increment("statsderl.dummy.counter", 1, SampleRate, Tags),
    ?check_and_reset(0, 1),

    statsderl:timing("statsderl.dummy.counter", 1, SampleRate),
    ?check_and_reset(0, 1),

    statsderl:timing("statsderl.dummy.counter", 1, SampleRate, Tags),
    ?check_and_reset(0, 1),

    statsderl:timing_fun("statsderl.dummy.timing", Fun, SampleRate),
    ?check_and_reset(0, 1),

    statsderl:timing_fun("statsderl.dummy.timing", Fun, SampleRate, Tags),
    ?check_and_reset(0, 1),

    statsderl:timing_now("statsderl.dummy.timing", Timestamp, SampleRate),
    ?check_and_reset(0, 1),

    statsderl:timing_now("statsderl.dummy.timing", Timestamp, SampleRate, Tags),
    ?check_and_reset(0, 1),

    ok.
