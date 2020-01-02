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

%% private
assert_base_key(BaseKey, Expected) ->
    BaseKey2 = iolist_to_binary(statsderl_utils:base_key(BaseKey)),
    ?assertEqual(Expected, BaseKey2).
