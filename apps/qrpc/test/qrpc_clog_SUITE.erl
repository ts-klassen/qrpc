-module(qrpc_clog_SUITE).
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
    ClogDir = filename:join(PrivDir, "clog"),
    application:set_env(qrpc, clog_dir, ClogDir),
    [{clog_dir, ClogDir}|Config].

end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun(Log) ->
        disk_log:close(Log)
    end, disk_log:all()),
    ok.

open(_Config) ->
    {ok, _Log} = qrpc_clog:open(test_open),
    ok.

log(Config) ->
    ClogDir = ?config(clog_dir, Config),
    ok = qrpc_clog:log(test_log, <<"entry1\n">>),
    ok = qrpc_clog:log(test_log, <<"entry2\n">>),
    ok = qrpc_clog:sync(test_log),
    LogDir = filename:join(ClogDir, "test_log"),
    {ok, Content} = read_all_logs(LogDir),
    true = binary:match(Content, <<"entry1">>) =/= nomatch,
    true = binary:match(Content, <<"entry2">>) =/= nomatch,
    ok.

alog(Config) ->
    ClogDir = ?config(clog_dir, Config),
    {ok, _} = qrpc_clog:open(test_alog),
    ok = qrpc_clog:alog(test_alog, <<"async_entry\n">>),
    timer:sleep(100),
    ok = qrpc_clog:sync(test_alog),
    LogDir = filename:join(ClogDir, "test_alog"),
    {ok, Content} = read_all_logs(LogDir),
    true = binary:match(Content, <<"async_entry">>) =/= nomatch,
    ok.

%% helpers

read_all_logs(Dir) ->
    LogFiles = filelib:wildcard(filename:join([Dir, "**", "*.log"])),
    Contents = lists:map(fun(F) ->
        {ok, Bin} = file:read_file(F),
        Bin
    end, LogFiles),
    {ok, iolist_to_binary(Contents)}.
