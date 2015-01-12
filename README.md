# Statsderl
#### Installation:

    $ make

#### Base Key:

For multi-node setups, it might be useful to be able to define a basic key based on the current node name, for example. Statsderl supports doing so by setting the app variable `base_key` to some iolist:

    application:set_env(statsderl, base_key, "my_node_name")

No need to split the keys -- the `.` as a separator is added automatically.

Alternatively, you can use one of the following atoms:

 * `hostname` -- use the hostname returned by `inet:gethostname/0`.
 * `sname` -- use the short node name (passed to `erl -sname`; e.g. the "nonode" in "nonode@nohost").
 * `name` -- use the long node name (passed to `erl -name`; e.g. "nonode@nohost").

#### Usage:
    application:start(statsderl).

#### Methods:

`["tag"]` is used to add [DataDog tags](https://github.com/DataDog/dogapi/blob/master/src/dogapi/stats/statsd.py#L30) to the stat.

##### Decrement
    statsderl:decrement("test.decrement", 1, 0.5).
    statsderl:decrement("test.decrement", 1, 0.5, ["tag"]).
    
##### Increment
    statsderl:increment("test.increment", 1, 0.5).
    statsderl:increment("test.increment", 1, 0.5, ["tag"]).

##### Gauge
    statsderl:gauge("test.gauge", 333, 1.0).
    statsderl:gauge("test.gauge", 333, 1.0, ["tag"]).

##### Timing
    statsderl:timing("test.timing", 5, 0.5).
    statsderl:timing("test.timing", 5, 0.5, ["tag"]).

##### Timing fun
    statsderl:timing_fun("test.timing", fun() -> timer:sleep(100) end, 0.5).
    statsderl:timing_fun("test.timing", fun() -> timer:sleep(100) end, 0.5, ["tag"]).

##### Timing now
    Timestamp = erlang:now().
    statsderl:timing_now("test.timing", Timestamp, 0.5).
    statsderl:timing_now("test.timing", Timestamp, 0.5, ["tag"]).
