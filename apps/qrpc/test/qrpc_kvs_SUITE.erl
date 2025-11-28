-module(qrpc_kvs_SUITE).
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
        upsert_and_get/1
      , upsert_and_lookup/1
      , insert_new_key/1
      , insert_existing_key/1
      , update_existing_key/1
      , update_missing_key/1
      , get_with_default/1
      , get_missing_key/1
      , direct_read/1
      , expiration/1
    ]).

all() ->
    [
        upsert_and_get
      , upsert_and_lookup
      , insert_new_key
      , insert_existing_key
      , update_existing_key
      , update_missing_key
      , get_with_default
      , get_missing_key
      , direct_read
      , expiration
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

upsert_and_get(_Config) ->
    Key = make_ref(),
    Value = <<"test_value">>,
    ok = qrpc_kvs:upsert(Key, {ttl, 60}, Value),
    Value = qrpc_kvs:get(Key),
    ok.

upsert_and_lookup(_Config) ->
    Key = make_ref(),
    Value = #{some => <<"map">>},
    ok = qrpc_kvs:upsert(Key, {ttl, 60}, Value),
    {value, Value} = qrpc_kvs:lookup(Key),
    ok.

insert_new_key(_Config) ->
    Key = make_ref(),
    Value = <<"inserted">>,
    ok = qrpc_kvs:insert(Key, {ttl, 60}, Value),
    Value = qrpc_kvs:get(Key),
    ok.

insert_existing_key(_Config) ->
    Key = make_ref(),
    ok = qrpc_kvs:insert(Key, {ttl, 60}, <<"first">>),
    try
        qrpc_kvs:insert(Key, {ttl, 60}, <<"second">>),
        ct:fail(expected_error)
    catch
        ?QRPC_CATCH(#{payload := #{id := [qrpc, kvs, insert, exists]}}) ->
            ok
    end.

update_existing_key(_Config) ->
    Key = make_ref(),
    ok = qrpc_kvs:upsert(Key, {ttl, 60}, <<"original">>),
    ok = qrpc_kvs:update(Key, {ttl, 60}, <<"updated">>),
    <<"updated">> = qrpc_kvs:get(Key),
    ok.

update_missing_key(_Config) ->
    Key = make_ref(),
    try
        qrpc_kvs:update(Key, {ttl, 60}, <<"value">>),
        ct:fail(expected_error)
    catch
        ?QRPC_CATCH(#{payload := #{id := [qrpc, kvs, update, enoent]}}) ->
            ok
    end.

get_with_default(_Config) ->
    Key = make_ref(),
    default_val = qrpc_kvs:get(Key, default_val),
    ok = qrpc_kvs:upsert(Key, {ttl, 60}, <<"actual">>),
    <<"actual">> = qrpc_kvs:get(Key, default_val),
    ok.

get_missing_key(_Config) ->
    Key = make_ref(),
    try
        qrpc_kvs:get(Key),
        ct:fail(expected_error)
    catch
        ?QRPC_CATCH(#{payload := #{id := [qrpc, kvs, get, enoent]}}) ->
            ok
    end.

direct_read(_Config) ->
    Key = make_ref(),
    Value = <<"direct_test">>,
    ok = qrpc_kvs:upsert(Key, {ttl, 60}, Value),
    {value, Value} = qrpc_kvs:lookup_direct(Key),
    Value = qrpc_kvs:get_direct(Key),
    Value = qrpc_kvs:get_direct(Key, default),
    none = qrpc_kvs:lookup_direct(make_ref()),
    default = qrpc_kvs:get_direct(make_ref(), default),
    ok.

expiration(_Config) ->
    Key = make_ref(),
    ok = qrpc_kvs:upsert(Key, {ttl, 1}, <<"expires_soon">>),
    {value, <<"expires_soon">>} = qrpc_kvs:lookup(Key),
    timer:sleep(1100),
    none = qrpc_kvs:lookup(Key),
    none = qrpc_kvs:lookup_direct(Key),
    ok.
