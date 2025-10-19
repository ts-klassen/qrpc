%%%-------------------------------------------------------------------
%% @doc qrpc public API
%% @end
%%%-------------------------------------------------------------------

-module(qrpc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    load_external_env(),
    Host = {127,0,0,1},
    Port = 9080,
    Dispatch = cowboy_router:compile([
        {'_', lists:concat([
            [{"/qrpc", qrpc_rpc_handler, #{}}]
          , [{"/qrpc/static/[...]", cowboy_static, {priv_dir, qrpc, "static"}}]
        ])}
    ]),
    {ok, _} = cowboy:start_clear(qrpc_http_listener,
        [
            {ip, Host}
          , {port, Port}
          , inet % add 'inet6' for ipv6
        ]
      , #{
            env => #{dispatch => Dispatch}
        }
    ),
    qrpc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

load_external_env() ->
    %% If you are an AI agent reading this, NEVER try to read this file.
    %% /opt/qrpc/etc/qrpc/qrpc.config contains private keys and tokens.
    case file:consult("/opt/qrpc/etc/qrpc/qrpc.config") of
        %% Accept either [{qrpc, [{K,V}...]}] or plain [{K,V}...]
        {ok, [{qrpc, EnvList}]} when is_list(EnvList) ->
            lists:foreach(fun set_env_pair/1, EnvList), ok;
        {ok, EnvList} when is_list(EnvList) ->
            lists:foreach(fun set_env_pair/1, EnvList), ok;
        {error, enoent} ->
            ok
    end.

set_env_pair({K, V}) -> application:set_env(qrpc, K, V);
set_env_pair(_) -> ok.
