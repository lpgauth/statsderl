-module(statsderl_transform_tests).
-include_lib("statsderl/include/statsderl.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, statsderl_transform}).

%% tests
statsderl_test_() ->
    {setup,
        fun () -> setup() end,
        fun (Socket) -> cleanup(Socket) end,
        {with, [
            fun counter_subtest/1,
            fun decrement_subtest/1,
            fun gauge_subtest/1,
            fun increment_subtest/1,
            fun timing_fun_subtest/1
        ]}
    }.

%% subtests
counter_subtest(Socket) ->
    A = "test",
    B = 5,
    C = 1,
    statsderl:counter("test", 1, 1),
    assert_packet(Socket, <<"test:1|c">>),
    statsderl:counter("test", 1, 1, statsderl_default),
    assert_packet(Socket, <<"test:1|c">>),
    statsderl:counter(["test", A], 1, 1),
    assert_packet(Socket, <<"testtest:1|c">>),
    statsderl:counter("test", B, 1),
    assert_packet(Socket, <<"test:5|c">>),
    statsderl:counter("test", 1, C),
    assert_packet(Socket, <<"test:1|c">>),
    statsderl:counter(fun () -> "test" end, 1, C),
    assert_packet(Socket, <<"test:1|c">>),
    statsderl:counter(fun () -> ["test.", integer_to_binary(100)] end, 1, C),
    assert_packet(Socket, <<"test.100:1|c">>).


decrement_subtest(Socket) ->
    statsderl:decrement("test", 1, 1.0),
    assert_packet(Socket, <<"test:-1|c">>).

gauge_subtest(Socket) ->
    A = "test",
    B = 5,
    C = 1,
    statsderl:gauge("test", 1, 1),
    assert_packet(Socket, <<"test:1|g">>),
    statsderl:gauge("test", 1, 1, statsderl_default),
    assert_packet(Socket, <<"test:1|g">>),
    statsderl:gauge(["test", A], 1, 1),
    assert_packet(Socket, <<"testtest:1|g">>),
    statsderl:gauge("test", B, 1),
    assert_packet(Socket, <<"test:5|g">>),
    statsderl:gauge("test", 1, C),
    assert_packet(Socket, <<"test:1|g">>).

increment_subtest(Socket) ->
    statsderl:increment("test", 1, 1),
    assert_packet(Socket, <<"test:1|c">>).

timing_fun_subtest(Socket) ->
    meck:new(statsderl_utils, [passthrough, no_history]),
    Seq = meck:loop([{1448, 573975, 400000}, {1448, 573975, 500000}]),
    meck:expect(statsderl_utils, timestamp, [], Seq),
    statsderl:timing_fun("test", fun () -> ok end, 1),
    assert_packet(Socket, <<"test:100|ms">>),
    meck:unload(statsderl_utils).

%% helpers
assert_packet(Socket, Expected) ->
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
    ?assertEqual(Expected, Packet).

cleanup(Socket) ->
    ok = gen_udp:close(Socket),
    statsderl_app:stop().

setup() ->
    error_logger:tty(false),
    application:load(?APP),
    {ok, Socket} = gen_udp:open(?DEFAULT_PORT, [binary, {active, false}]),
    timer:sleep(100),
    statsderl_app:start(),
    timer:sleep(1000),
    Socket.
