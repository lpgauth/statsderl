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
    
#### Timing now
    Timestamp = erlang:now(),
    statsderl:timing_now("test.timing", Timestamp, 0.5).




