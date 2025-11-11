-module(q_tut).
-include_lib("qrpc/include/qrpc.hrl").

-export([
        hello_world/1
      , hello_error/1
      , hello_admin/1
      , hello_config/1
      , hello_blob/1
    ]).

hello_world(_) ->
    #{ payload => <<"hello world">> }.

hello_error(Rpc) ->
    ?QRPC_ERROR(#{
        id => [q_tut, q_tut, hello_error, example_error]
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

hello_config(_Rpc) ->
    #{ payload => #{
        get => ?QRPC_SUBCONF_GET(hello_config)
      , default => ?QRPC_SUBCONF_GET(non_existing_key, <<"default value">>)
      , lookup => case ?QRPC_SUBCONF_LOOKUP(hello_config) of
            {value, Value} ->
                <<"value: ", Value/binary>>;
            none ->
                <<"This was not suppose to happen!">>
        end
    } }.

hello_blob(Rpc) ->
    BlobCount = max(1, Rpc:get([payload, <<"blob_count">>], 1)),
    #{
       payload => #{ blob_count => BlobCount }
     , blob => lists:map(fun(I) ->
           Ibin = klsn_binstr:from_any(I),
           #{ data => <<"hello blob ", Ibin/binary>> }
       end, lists:seq(1, BlobCount))
    }.

