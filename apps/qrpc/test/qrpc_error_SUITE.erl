-module(qrpc_error_SUITE).
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
        raises/1
      , logs_to_dlog/1
      , logs_to_clog/1
      , get_path/1
      , get_with_default/1
      , lookup_path/1
    ]).

all() ->
    [
        raises
      , logs_to_dlog
      , logs_to_clog
      , get_path
      , get_with_default
      , lookup_path
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ClogDir = filename:join(PrivDir, "clog"),
    DlogDir = filename:join(PrivDir, "dlog"),
    application:set_env(qrpc, clog_dir, ClogDir),
    application:set_env(qrpc, dlog_dir, DlogDir),
    application:set_env(qrpc, dlog_config, #{
        qrpc_error => [
            {type, halt}
          , {size, infinity}
        ]
    }),
    [{clog_dir, ClogDir}|Config].

end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun(Log) ->
        disk_log:close(Log)
    end, disk_log:all()),
    ok.

raises(_Config) ->
    try
        ?QRPC_ERROR(#{
            id => [test, error, raises]
          , fault_source => server
          , message => <<"Test error">>
          , message_ja => <<"テストエラー"/utf8>>
          , is_known => true
          , is_retryable => false
          , version => 1
        }),
        ct:fail(expected_error)
    catch
        ?QRPC_CATCH(#{payload := #{id := [test, error, raises]}}) ->
            ok
    end.

logs_to_dlog(_Config) ->
    try
        ?QRPC_ERROR(#{
            id => [test, error, logs_dlog]
          , fault_source => server
          , message => <<"Test error for dlog">>
          , message_ja => <<"dlog用テストエラー"/utf8>>
          , is_known => true
          , is_retryable => false
          , version => 1
        })
    catch
        ?QRPC_CATCH(_) ->
            ok
    end,
    timer:sleep(100),
    {ok, _} = qrpc_dlog:open(qrpc_error),
    ok = disk_log:sync({qrpc_dlog, qrpc_error}),
    {_, Terms} = disk_log:chunk({qrpc_dlog, qrpc_error}, start),
    true = length(Terms) >= 1,
    true = lists:any(fun
        (#{payload := #{id := [test, error, logs_dlog]}}) -> true;
        (_) -> false
    end, Terms),
    ok.

logs_to_clog(Config) ->
    ClogDir = ?config(clog_dir, Config),
    {ok, _} = qrpc_clog:open(qrpc_error),
    try
        ?QRPC_ERROR(#{
            id => [test, error, logs_clog]
          , fault_source => server
          , message => <<"Test error for clog">>
          , message_ja => <<"clog用テストエラー"/utf8>>
          , is_known => true
          , is_retryable => false
          , version => 1
        })
    catch
        ?QRPC_CATCH(_) ->
            ok
    end,
    timer:sleep(100),
    ok = qrpc_clog:sync(qrpc_error),
    LogDir = filename:join(ClogDir, "qrpc_error"),
    LogFiles = filelib:wildcard(filename:join([LogDir, "**", "*.log"])),
    true = length(LogFiles) >= 1,
    Contents = lists:map(fun(F) ->
        {ok, Bin} = file:read_file(F),
        Bin
    end, LogFiles),
    Content = iolist_to_binary(Contents),
    true = binary:match(Content, <<"test.error.logs_clog">>) =/= nomatch,
    ok.

get_path(_Config) ->
    try
        ?QRPC_ERROR(#{
            id => [test, error, get_test]
          , fault_source => client
          , message => <<"Test">>
          , message_ja => <<"テスト"/utf8>>
          , is_known => true
          , is_retryable => false
          , detail => #{foo => bar}
          , version => 1
        })
    catch
        ?QRPC_CATCH(Error) ->
            [test, error, get_test] = qrpc_error:get(Error, [payload, id]),
            client = qrpc_error:get(Error, [payload, fault_source]),
            bar = qrpc_error:get(Error, [payload, detail, foo]),
            ok
    end.

get_with_default(_Config) ->
    try
        ?QRPC_ERROR(#{
            id => [test, error, get_default]
          , fault_source => server
          , message => <<"Test">>
          , message_ja => <<"テスト"/utf8>>
          , is_known => true
          , is_retryable => false
          , version => 1
        })
    catch
        ?QRPC_CATCH(Error) ->
            default_value = qrpc_error:get(Error, [payload, nonexistent], default_value),
            ok
    end.

lookup_path(_Config) ->
    try
        ?QRPC_ERROR(#{
            id => [test, error, lookup_test]
          , fault_source => external
          , message => <<"Test">>
          , message_ja => <<"テスト"/utf8>>
          , is_known => false
          , is_retryable => true
          , version => 1
        })
    catch
        ?QRPC_CATCH(Error) ->
            {value, [test, error, lookup_test]} = qrpc_error:lookup(Error, [payload, id]),
            {value, external} = qrpc_error:lookup(Error, [payload, fault_source]),
            {value, true} = qrpc_error:lookup(Error, [payload, is_retryable]),
            none = qrpc_error:lookup(Error, [payload, nonexistent]),
            ok
    end.
