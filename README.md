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
    Timestamp = erlang:now(),
    statsderl:timing("test.timing", Timestamp, 0.5).




