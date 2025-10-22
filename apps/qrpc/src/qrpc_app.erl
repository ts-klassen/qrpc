%%%-------------------------------------------------------------------
%% @doc qrpc public API
%% @end
%%%-------------------------------------------------------------------

-module(qrpc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    qrpc_conf:reload(),
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
