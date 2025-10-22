-module(qrpc).

-export([
        rpc/1
    ]).


-spec rpc(qrpc_rpc:rpc()) -> qrpc_rpc:rpc().
rpc(Rpc) ->
    qrpc_rpc:rpc(Rpc).

