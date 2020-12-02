%% macros
-define(APP, statsderl).
-define(CHILD(Name, Mod), {Name, {Mod, start_link, [Name]}, permanent, 5000, worker, [Mod]}).
-define(CLIENT, statsderl_client).
-define(MAX_UNSIGNED_INT_32, 4294967295).

%% env vars
-define(ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(ENV_BASEKEY, base_key).
-define(ENV_HOSTNAME, hostname).
-define(ENV_PORT, port).
-define(ENV_VARS, [?ENV_BASEKEY, ?ENV_HOSTNAME, ?ENV_PORT]).

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 4096).
-define(DEFAULT_BASEKEY, undefined).
-define(DEFAULT_HOSTNAME, "127.0.0.1").
-define(DEFAULT_POOL_SIZE, 4).
-define(DEFAULT_PORT, 8125).

%% types
-type base_key() :: base_key_part() | [base_key_part()].
-type base_key_part() :: hostname | name | sname | undefined | iodata().
-type key() :: iodata() | fun().
-type operation() :: {cast, iodata()} |
                     {counter, key(), value(), sample_rate()} |
                     {gauge, key(), value()} |
                     {gauge_decrement, key(), value()} |
                     {gauge_increment, key(), value()} |
                     {timing, key(), value()} |
                     {timing_now, key(), erlang:timestamp()} |
                     {timing_now_us, key(), erlang:timestamp()}.
-type pool_size() :: pos_integer().
-type sample_rate() :: number().
-type value() :: number().
