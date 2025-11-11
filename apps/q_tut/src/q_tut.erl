-module(q_tut).
-include_lib("qrpc/include/qrpc.hrl").

-export([
        hello_world/1
      , hello_error/1
      , hello_admin/1
    ]).

hello_world(_) ->
    #{ payload => <<"hello world">> }.

hello_error(Rpc) ->
    ?QRPC_ERROR(#{
        id => [q_tut, q_tut, hello_error]
      , fault_source => client
      , message => <<"hello error (An sample error)">>
      , message_ja => <<"hello error （エラーのサンプルです）"/utf8>>
      , detail => #{
            rpc => Rpc:strip_rpc_for_log()
        }
      , is_known => true
      , is_retryable => false
      , version => 1
    }).

hello_admin(Rpc) ->
    #{ payload => Rpc:get([metadata, jwt]) }.


