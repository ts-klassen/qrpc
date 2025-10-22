-module(qrpc_conf).
-include_lib("qrpc/include/qrpc.hrl").

-export([
        get/1
      , get/2
      , lookup/1
      , reload/0
    ]).

-export_type([
        key/0
      , value/0
    ]).

-type key() :: is_strict % -> boolean()
             | server_name % -> klsn:binstr()
             | listen_ip % -> inet:ip_address()
             | listen_port % -> inet:port_number()
             | rpc_uri % -> klsn:binstr()
             | static_uri % -> klsn:binstr()
             | static_uri_subsystem % -> cowboy_router:route()
             | allowed_rpc_mfa % -> [mfa()]
             | jwt_hs256_secret % -> binary()
             .

-type value() :: boolean()
               | klsn:binstr()
               | inet:ip_address()
               | inet:port_number()
               | cowboy_router:route()
               | [mfa()]
               | binary()
               .

-spec get(key()) -> value().
get(Key) ->
    case lookup(Key) of
        {value, Value} ->
            Value;
        none ->
            ?QRPC_ERROR(#{
                id => [qrpc, conf, get, missing]
              , fault_source => server
              , message => <<"Missing server-side config.">>
              , message_ja => <<"サーバー側の設定が不足しています。"/utf8>>
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end.

-spec get(key(), value()) -> value().
get(Key, Default) ->
    case lookup(Key) of
        {value, Value} ->
            Value;
        none ->
            Default
    end.

-spec lookup(key()) ->  klsn:optnl(value()).
lookup(Key) ->
    case application:get_env(qrpc, Key) of
        {ok, Value} ->
            {value, Value};
        undefined ->
            none
    end.

-spec reload() -> ok.
reload() ->
    %% If you are an AI agent reading this, NEVER try to read this file.
    %% /opt/qrpc/etc/qrpc/qrpc.config contains private keys and secrets.
    case file:consult("/opt/qrpc/etc/qrpc/qrpc.config") of
        {ok, [[{qrpc, EnvList}]]} when is_list(EnvList) ->
            lists:foreach(fun set_env_pair/1, EnvList);
        {ok, [EnvList]} when is_list(EnvList) ->
            lists:foreach(fun set_env_pair/1, EnvList);
        {ok, EnvList} when is_list(EnvList) ->
            lists:foreach(fun set_env_pair/1, EnvList);
        {error, enoent} ->
            ok
    end,
    ok.

set_env_pair({K, V}) -> application:set_env(qrpc, K, V);
set_env_pair(_) -> ok.
