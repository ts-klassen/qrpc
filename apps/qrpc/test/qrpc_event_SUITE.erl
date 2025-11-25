-module(qrpc_event_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
        all/0
      , init/1
      , simple/1
    ]).

all() ->
    [
        init
      , simple
    ].

init(_Config) ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}]),
    {ok, Port} = inet:port(ListenSocket),
    gen_tcp:close(ListenSocket),
    application:set_env(qrpc, listen_port, Port),
    application:set_env(qrpc, rabbitmq, #{
        host => <<"localhost">>
      , port => 5672
      , username => <<"guest">>
      , password => <<"guest">>
      , vhost => <<"/">>
    }),
    % set bad log dir so it gets dumped to ct log
    application:set_env(qrpc, clog_dir, "/root/inaccessible/directory/path"),
    application:set_env(qrpc, dlog_dir, "/root/inaccessible/directory/path"),
    application:ensure_all_started(qrpc),
    ok.

simple(_Config) ->
    NameSpace1 = klsn_binstr:uuid(),
    NameSpace2 = klsn_binstr:uuid(),
    #{payload := ok} = qrpc_event:send(#{payload => #{
        namespace => NameSpace1
      , id => <<"1">>
      , message => <<"hello">>
    }}),
    #{payload := [
        #{id := <<"1">>,message := <<"hello">>,namespace := NameSpace1}
    ]} = qrpc_event:fetch(#{payload => #{namespace => NameSpace1, timeout => 100}}),
    #{payload := ok} = qrpc_event:send(#{payload => #{
        namespace => NameSpace1
      , id => <<"2">>
      , message => <<"world">>
    }}),
    #{payload := [
        #{id := <<"1">>,message := <<"hello">>,namespace := NameSpace1}
      , #{id := <<"2">>,message := <<"world">>,namespace := NameSpace1}
    ]} = qrpc_event:fetch(#{payload => #{namespace => NameSpace1, timeout => 100}}),
    #{payload := [
    ]} = qrpc_event:fetch(#{payload => #{namespace => NameSpace2, timeout => 100}}),
    ok.

