-module(qrpc_dlog_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
        all/0
      , init_per_suite/1
      , end_per_suite/1
      , init_per_testcase/2
      , end_per_testcase/2
    ]).

-export([
        open/1
      , log/1
      , alog/1
    ]).

all() ->
    [
        open
      , log
      , alog
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DlogDir = filename:join(PrivDir, "dlog"),
    application:set_env(qrpc, dlog_dir, DlogDir),
    application:set_env(qrpc, dlog_config, #{
        test_open => [
            {type, halt}
          , {size, infinity}
        ]
      , test_log => [
            {type, halt}
          , {size, infinity}
        ]
      , test_alog => [
            {type, halt}
          , {size, infinity}
        ]
    }),
    Config.

end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun(Log) ->
        disk_log:close(Log)
    end, disk_log:all()),
    ok.

open(_Config) ->
    {ok, _Log} = qrpc_dlog:open(test_open),
    ok.

log(_Config) ->
    ok = qrpc_dlog:log(test_log, #{key => value1}),
    ok = qrpc_dlog:log(test_log, #{key => value2}),
    ok = disk_log:sync({qrpc_dlog, test_log}),
    {_, Terms} = disk_log:chunk({qrpc_dlog, test_log}, start),
    2 = length(Terms),
    [#{key := value1}, #{key := value2}] = Terms,
    ok.

alog(_Config) ->
    {ok, _} = qrpc_dlog:open(test_alog),
    ok = qrpc_dlog:alog(test_alog, #{async => true}),
    timer:sleep(100),
    ok = disk_log:sync({qrpc_dlog, test_alog}),
    {_, Terms} = disk_log:chunk({qrpc_dlog, test_alog}, start),
    1 = length(Terms),
    [#{async := true}] = Terms,
    ok.
