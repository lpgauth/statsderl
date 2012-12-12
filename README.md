# Statsderl
### Installation

    $ make
### Usage
    application:start(statsderl).

#### Increment
    statsderl:increment("test.increment", 1, 0.5).

#### Decrement
    statsderl:decrement("test.decrement", 1, 0.5).

#### Timing
    statsderl:timing("test.timing", 5, 0.5).

#### Timing fun
    statsderl:timing_fun("test.timing", fun() -> timer:sleep(100) end, 0.5).

#### Timing now
    Timestamp = erlang:now(),
    statsderl:timing_now("test.timing", Timestamp, 0.5).

#### Gauge
    statsderl:gauge("test.gauge", 333, 1.0).

### Base Key

For multi-node setups, it might be useful to be able to define a basic key based on the current node name, for example. Statsderl supports doing so by setting the app variable `base_key` to some iolist:

    application:set_env(statsderl, base_key, "my_node_name")

No need to split the keys -- the `.` as a separator is added automatically.
