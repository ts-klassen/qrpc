-module(qrpc_counter_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("qrpc/include/qrpc.hrl").

-export([
        all/0
      , init_per_suite/1
      , end_per_suite/1
      , init_per_testcase/2
      , end_per_testcase/2
    ]).

-export([
        add_new_counter/1
      , add_existing_counter/1
      , set_counter/1
      , get_missing_counter/1
      , expiration_ttl/1
      , expiration_slot/1
    ]).

all() ->
    [
        add_new_counter
      , add_existing_counter
      , set_counter
      , get_missing_counter
      , expiration_ttl
      , expiration_slot
    ].

init_per_suite(Config) ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}]),
    {ok, Port} = inet:port(ListenSocket),
    gen_tcp:close(ListenSocket),
    application:set_env(qrpc, listen_port, Port),
    application:set_env(qrpc, clog_dir, "/root/inaccessible/directory/path"),
    application:set_env(qrpc, dlog_dir, "/root/inaccessible/directory/path"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DetsPath = filename:join(PrivDir, "qrpc_kvs_test"),
    application:set_env(qrpc, kvs_dets_path, DetsPath),
    application:ensure_all_started(qrpc),
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(qrpc),
    ok.

add_new_counter(_Config) ->
    Id = make_ref(),
    1 = qrpc_counter:add(Id, {ttl, 60}, 1),
    1 = qrpc_counter:get(Id),
    ok.

add_existing_counter(_Config) ->
    Id = make_ref(),
    1 = qrpc_counter:add(Id, {ttl, 60}, 1),
    3 = qrpc_counter:add(Id, {ttl, 60}, 2),
    8 = qrpc_counter:add(Id, {ttl, 60}, 5),
    8 = qrpc_counter:get(Id),
    ok.

set_counter(_Config) ->
    Id = make_ref(),
    5 = qrpc_counter:set(Id, {ttl, 60}, 5),
    5 = qrpc_counter:get(Id),
    10 = qrpc_counter:set(Id, {ttl, 60}, 10),
    10 = qrpc_counter:get(Id),
    ok.

get_missing_counter(_Config) ->
    Id = make_ref(),
    0 = qrpc_counter:get(Id),
    ok.

expiration_ttl(_Config) ->
    Id = make_ref(),
    1 = qrpc_counter:add(Id, {ttl, 1}, 1),
    1 = qrpc_counter:get(Id),
    timer:sleep(1100),
    0 = qrpc_counter:get(Id),
    ok.

expiration_slot(_Config) ->
    Id = make_ref(),
    1 = qrpc_counter:add(Id, {slot, secondly}, 1),
    1 = qrpc_counter:get(Id),
    timer:sleep(1100),
    0 = qrpc_counter:get(Id),
    ok.
