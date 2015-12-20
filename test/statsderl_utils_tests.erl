-module(statsderl_utils_tests).
-include_lib("statsderl/include/statsderl.hrl").
-include_lib("eunit/include/eunit.hrl").

base_key_test() ->
    meck:new(inet, [passthrough, no_history, unstick]),
    meck:expect(inet, gethostname, fun () ->
        {ok, "myhost"}
    end),

    assert_base_key(name, <<"nonode.nohost.">>),
    assert_base_key(sname, <<"nonode.">>),
    assert_base_key(undefined, <<"">>),
    assert_base_key(["hello", <<"world">>], <<"hello.world.">>),
    assert_base_key(["rtb", "gateway", hostname], <<"rtb.gateway.myhost.">>),

    meck:unload(inet).

getaddrs_test() ->
    meck:new(inet, [passthrough, no_history, unstick]),
    meck:expect(inet, getaddrs, fun
        ("adgear.com", inet) -> {ok, [
            {127, 0, 0, 1}
        ]};
        ("adgrx.com", inet) -> {ok, [
            {38, 74, 193, 134},
            {38, 74, 193, 40}
        ]}
    end),

    {ok, {127, 0, 0, 1}} = statsderl_utils:getaddrs(<<"adgear.com">>),
    {ok, {38, 74, 193, _}} = statsderl_utils:getaddrs(<<"adgrx.com">>),

    meck:unload(inet).

%% private
assert_base_key(BaseKey, Expected) ->
    BaseKey2 = iolist_to_binary(statsderl_utils:base_key(BaseKey)),
    ?assertEqual(Expected, BaseKey2).
