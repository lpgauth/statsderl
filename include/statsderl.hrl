%% macros
-define(APP, statsderl).
-define(CHILD(Name, Mod), {Name, {Mod, start_link, [Name]}, permanent, 5000, worker, [Mod]}).
-define(DEFAULT_BASEKEY, undefined).
-define(DEFAULT_HOSTNAME, {127, 0, 0, 1}).
-define(DEFAULT_PORT, 8125).
-define(ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(ENV_BASEKEY, base_key).
-define(ENV_HOSTNAME, hostname).
-define(ENV_PORT, port).
-define(ENV_VARS, [?ENV_BASEKEY, ?ENV_HOSTNAME, ?ENV_PORT]).
-define(MAX_UNSIGNED_INT_32, 4294967296).
-define(POOL_SIZE, 4).
-define(SERVER, statsderl_server).

%% types
-type key() :: iodata().
-type op_code() :: decrement | gauge | gauge_decrement |
    gauge_increment | increment | timing.
-type sample_rate() :: number().
-type value() :: number().
