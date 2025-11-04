-module(qrpc_rpc_handler).
-include_lib("qrpc/include/qrpc.hrl").
-behavior(cowboy_rest).

-export([init/2]).

-define(MAX_LENGTH, 8000000).

init(Req0, State) ->
    {Body, Req10} = case cowboy_req:has_body(Req0) of
        true ->
            case cowboy_req:read_body(Req0, #{length => ?MAX_LENGTH}) of
                {ok, Data, Req5} ->
                    {{ok, Data}, Req5};
                {more, _Data, Req5} ->
                    {{error, payload_too_large}, Req5};
                Other ->
                    {{error, Other}, Req0}
            end;
        false ->
            {{error, empty}, Req0}
    end,
    ResponseData = try main(Body) catch
        ?QRPC_CATCH(QrpcError) ->
            on_qrpc_error(QrpcError);
        Class:Reason:Stack ->
        try ?QRPC_ERROR(#{
            id => [qrpc, rpc_handler, init, catch_all]
          , fault_source => server
          , message => <<"Unknown Erlang error.">>
          , message_ja => <<"未知の Erlang error が発生しました。"/utf8>>
          , detail => #{
                req => Req0
              , state => State
            }
          , is_known => false
          , is_retryable => false
          , class => Class
          , reason => Reason
          , stacktrace => Stack
          , version => 1
        }) catch ?QRPC_CATCH(QrpcError) ->
            on_qrpc_error(QrpcError)
        end
    end,
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, ResponseData, Req10),
    {ok, Req, State}.

main({ok, Data}) ->
    Request0 = try json:decode(Data) catch Class1:Reason1:Stack1 ->
        ?QRPC_ERROR(#{
            id => [qrpc, rpc_handler, main, malformed_json]
          , fault_source => client
          , message => <<"malformed JSON.">>
          , message_ja => <<"不正な JSON です。"/utf8>>
          , detail => #{}
          , is_known => true
          , is_retryable => false
          , class => Class1
          , reason => Reason1
          , stacktrace => Stack1
          , version => 1
        })
    end,
    Request10 = klsn_map:upsert([metadata, protocol], http, Request0),
    Response = qrpc:rpc(Request10),
    try json:encode(Response) catch Class2:Reason2:Stack2 ->
        ?QRPC_ERROR(#{
            id => [qrpc, rpc_handler, main, json_encode_failed]
          , fault_source => server
          , message => <<"failed to encode to JSON.">>
          , message_ja => <<"JSON エンコードに失敗しました。"/utf8>>
          , detail => #{}
          , is_known => true
          , is_retryable => false
          , class => Class2
          , reason => Reason2
          , stacktrace => Stack2
          , version => 1
        })
    end;
main({error, empty}) ->
    ?QRPC_ERROR(#{
        id => [qrpc, rpc_handler, main, missing_http_body]
      , fault_source => client
      , message => <<"HTTP body missing.">>
      , message_ja => <<"HTTP ボディがありません。"/utf8>>
      , detail => #{}
      , is_known => true
      , is_retryable => false
      , version => 1
    });
main({error, payload_too_large}) ->
    ?QRPC_ERROR(#{
        id => [qrpc, rpc_handler, main, payload_too_large]
      , fault_source => client
      , message => <<"HTTP payload too large.">>
      , message_ja => <<"HTTP ペイロードが大きすぎます。"/utf8>>
      , detail => #{}
      , is_known => true
      , is_retryable => false
      , version => 1
    });
main({error, Other}) ->
    ?QRPC_ERROR(#{
        id => [qrpc, rpc_handler, main, failed_to_get_http_body]
      , fault_source => server
      , message => <<"failed to get HTTP body.">>
      , message_ja => <<"HTTP body の取得に失敗しました。"/utf8>>
      , detail => #{
            cowboy_req_read_body => Other
        }
      , is_known => false
      , is_retryable => false
      , version => 1
    }).

on_qrpc_error(QrpcError) ->
    Metadata = klsn_map:filter(#{
        success => {value, false}
      , error_detail => {value, klsn_map:filter(#{
            uuid => klsn_map:lookup([metadata, uuid], QrpcError)
          , id => klsn_map:lookup([payload, id], QrpcError)
          , fault_source => klsn_map:lookup([payload, fault_source], QrpcError)
          , message => klsn_map:lookup([payload, message], QrpcError)
          , message_ja => klsn_map:lookup([payload, message_ja], QrpcError)
          , is_known => klsn_map:lookup([payload, is_known], QrpcError)
          , is_retryable => klsn_map:lookup([payload, is_retryable], QrpcError)
          , should_auto_retry => klsn_map:lookup([payload, should_auto_retry], QrpcError)
          , retry_count => klsn_map:lookup([payload, retry_count], QrpcError)
          , retry_after => klsn_map:lookup([payload, retry_after], QrpcError)
        })}
      , server_name => qrpc_conf:lookup(server_name)
      , type => {value, response}
    }),
    json:encode(#{
        metadata => Metadata
      , payload => null
    }).

