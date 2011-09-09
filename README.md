STATSDERL
=========
Usage
-----
    
    application:start(statsderl),
    statsderl:increment("test.increment", 1, 0.5),
    statsderl:decrement("test.decrement", 1, 0.5),
    Timestamp = erlang:now(),
    statsderl:timing("test.decrement", Timestamp, 0.5).


Installation
------------

    $ make

