%% TODO
% timing_fun/3,
% timing_now/3
% sampling rate

-module(statsderl_tests).
-include_lib("statsderl/include/statsderl.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.

statsderl_test_() ->
    {setup,
        fun () -> setup() end,
        fun (Socket) -> cleanup(Socket) end,
        {with, [
            fun counter_subtest/1,
            fun decrement_subtest/1,
            fun gauge_decrement_subtest/1,
            fun gauge_increment_subtest/1,
            fun gauge_subtest/1,
            fun increment_subtest/1,
            fun sampling_rate_subtest/1,
            fun timing_fun_subtest/1,
            fun timing_subtest/1
        ]}
    }.

counter_subtest(Socket) ->
    statsderl:counter("test", 1.123, 1),
    assert_packet(Socket, <<"test:1.12|c">>).

decrement_subtest(Socket) ->
    statsderl:decrement("test", 1, 1),
    assert_packet(Socket, <<"test:-1|c">>).

gauge_subtest(Socket) ->
    statsderl:gauge("test", 1, 1),
    assert_packet(Socket, <<"test:1|g">>).

gauge_decrement_subtest(Socket) ->
    statsderl:gauge_decrement("test", 1, 1),
    assert_packet(Socket, <<"test:-1|g">>).

gauge_increment_subtest(Socket) ->
    statsderl:gauge_increment("test", 1, 1),
    assert_packet(Socket, <<"test:+1|g">>).

increment_subtest(Socket) ->
    statsderl:increment("test", 1, 1),
    assert_packet(Socket, <<"test:1|c">>).

sampling_rate_subtest(Socket) ->
    meck:new(statsderl_utils, [passthrough, no_history]),
    meck:expect(statsderl_utils, random, fun (?MAX_UNSIGNED_INT_32) -> 0 end),
    statsderl:counter("test", 1, 0.1234),
    assert_packet(Socket, <<"test:1|c|@0.123">>),
    meck:unload(statsderl_utils).

timing_fun_subtest(Socket) ->
    meck:new(statsderl_utils, [passthrough, no_history]),
    Seq = meck:loop([{1448, 573975, 400000}, {1448, 573975, 500000}]),
    meck:expect(statsderl_utils, timestamp, [], Seq),
    statsderl:timing_fun("test", fun () -> ok end, 1),
    assert_packet(Socket, <<"test:100|ms">>),
    meck:unload(statsderl_utils).

timing_subtest(Socket) ->
    statsderl:timing("test", 1, 1),
    assert_packet(Socket, <<"test:1|ms">>).

%% private
assert_packet(Socket, Expected) ->
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
    ?assertEqual(Expected, Packet).

cleanup(Socket) ->
    ok = gen_udp:close(Socket),
    statsderl_app:stop().

setup() ->
    error_logger:tty(false),
    statsderl_app:start(),
    {ok, Socket} = gen_udp:open(?DEFAULT_PORT, [binary, {active, false}]),
    Socket.
