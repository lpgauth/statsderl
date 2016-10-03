-module(statsderl_tests).
-include_lib("statsderl/include/statsderl.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.

statsderl_base_key_test() ->
    assert_base_key("base_key", <<"base_key.">>),
    assert_base_key(<<"base_key">>, <<"base_key.">>).

statsderl_hostname_test() ->
    meck:new(statsderl_utils, [passthrough, no_history]),
    meck:expect(statsderl_utils, getaddrs, fun (_) ->
        {ok, {127, 0, 0, 1}}
    end),
    Socket = setup([{?ENV_HOSTNAME, <<"adgear.com">>}]),
    statsderl:counter("test", 1, 1),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
    ?assertEqual(<<"test:1|c">>, Packet),
    meck:unload(statsderl_utils),
    cleanup(Socket).

statsderl_multiple_hostname_test() ->
    meck:new(statsderl_utils, [passthrough, no_history]),
    meck:expect(statsderl_utils, getaddrs, fun (_) ->
        {ok, {127, 0, 0, 1}}
    end),
    Socket = setup([{?ENV_HOSTNAME,
        [{<<"adgear.com">>, ?DEFAULT_PORT}, {"whatever.com", ?DEFAULT_PORT}]}]),
    statsderl:counter("test", 1, 1),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
    ?assertEqual(<<"test:1|c">>, Packet),
    meck:unload(statsderl_utils),
    cleanup(Socket).

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

%% subtests
counter_subtest(Socket) ->
    statsderl:counter("test", 1.123, 1),
    assert_packet(Socket, <<"test:1.12|c">>).

decrement_subtest(Socket) ->
    statsderl:decrement("test", 1, 1.0),
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
    meck:expect(statsderl_utils, random, fun (?MAX_UNSIGNED_INT_32) -> 1 end),
    statsderl:counter("test", 1, 0.1234),
    assert_packet(Socket, <<"test:1|c|@0.123">>),
    meck:expect(statsderl_utils, random, fun (?MAX_UNSIGNED_INT_32) ->
        ?MAX_UNSIGNED_INT_32
    end),
    statsderl:counter("test", 1, 0.1234),
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

%% helpers
assert_base_key(BaseKey, Expected) ->
    Socket = setup([{?ENV_BASEKEY, BaseKey}]),
    statsderl:counter("test", 1, 1),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
    ?assertEqual(<<Expected/binary, "test:1|c">>, Packet),
    cleanup(Socket).

assert_packet(Socket, Expected) ->
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
    ?assertEqual(Expected, Packet).

cleanup(Socket) ->
    ok = gen_udp:close(Socket),
    statsderl_app:stop().

setup() ->
    setup([]).

setup(EnvVars) ->
    error_logger:tty(false),
    application:load(?APP),
    [application:unset_env(?APP, K) || K <- ?ENV_VARS],
    [application:set_env(?APP, K, V) || {K, V} <- EnvVars],
    statsderl_app:start(),
    {ok, Socket} = gen_udp:open(?DEFAULT_PORT, [binary, {active, false}]),
    Socket.
