%%%-------------------------------------------------------------------
%% @doc qrpc public API
%% @end
%%%-------------------------------------------------------------------

-module(qrpc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    qrpc_conf:reload(),
    Host = qrpc_conf:get(listen_ip, {127,0,0,1}),
    Port = qrpc_conf:get(listen_port, 9080),
    Dispatch = cowboy_router:compile([
        {'_', lists:concat([
            [{"/qrpc", qrpc_rpc_handler, #{}}]
          , lists:map(fun(SubSystem) ->
                SSBin = klsn_binstr:from_any(SubSystem),
                URI = klsn_binstr:replace(
                    [{<<"/:subsystem/">>, <<"/", SSBin/binary, "/">>}]
                  , qrpc_conf:get(static_uri, <<"/qrpc/:subsystem/static/[...]">>)
                ),
                {URI, cowboy_static, {priv_dir, SubSystem, "static"}}
            end, qrpc_conf:get(static_uri_subsystem, []))
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
    cowboy:stop_listener(qrpc_http_listener),
    ok.
