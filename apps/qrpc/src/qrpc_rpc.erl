-module(qrpc_rpc).
-include_lib("qrpc/include/qrpc.hrl").

-export([
        rpc/1
      , get/2
      , get/3
      , lookup/2
      , early_response/1
      , strip_rpc_for_log/1
    ]).

-export_type([
        rpc/0
      , type/0
      , mode/0
      , protocol/0
      , partial_request_metadata/0
      , request_metadata/0
      , response_metadata/0
      , payload/0
      , blob/0
    ]).

-type rpc() :: #{
        qrpc_map_calls := #{module := qrpc_rpc, type := rpc, pos := head}
      , metadata := partial_request_metadata()
                  | request_metadata()
                  | response_metadata()
      , payload := payload()
      , blob => blob() | [blob()]
    }.

-type type() :: request | response.

-type mode() :: normal.

-type protocol() :: native | http.

-type partial_request_metadata() :: #{
        type => request | klsn:binstr()
      , mode => mode() | klsn:binstr()
      , protocol => protocol() | klsn:binstr()
      , client_name => klsn:binstr()
      , request_id => klsn:binstr()
      , jwt => qrpc_jwt:jwt()
      , module := module() | klsn:binstr()
      , function := atom() | klsn:binstr()
      , arity := non_neg_integer()
    }.

-type request_metadata() :: #{
        type := request
      , mode := mode()
      , protocol := protocol()
      , client_name := klsn:binstr()
      , request_id := klsn:binstr()
      , rpc_uuid := klsn:binstr()
      , jwt := qrpc_jwt:payload()
      , module := module()
      , function := atom()
      , arity := non_neg_integer()
    }.

-type response_metadata() :: #{
        success := boolean()
      , error_detail => #{
            uuid := klsn:binstr()
          , id := [atom(), ...]
          , fault_source := client | server | external | unknown
          , message := klsn:binstr()
          , message_ja := klsn:binstr()
          , is_known := boolean()
          , is_retryable := boolean()
          , should_auto_retry := boolean()
          , retry_count => non_neg_integer()
          , retry_after => non_neg_integer()
        }
      , server_name := klsn:binstr()
      , type := response
      , mode := mode()
      , protocol := protocol()
      , request_id := klsn:binstr()
      , rpc_uuid := klsn:binstr()
    }.

-type payload() :: term().

-type blob() :: #{
        content_type => klsn:binstr()
      , content_length => non_neg_integer()
      , data := binary()
    }.


-spec rpc(rpc()) -> rpc().
rpc(PartialReqRpc0) ->
    PartialReqRpc = safe_parse_req(PartialReqRpc0),
    try
        ReqRpc = parse_req(PartialReqRpc),
        ResRpc = process_rpc(ReqRpc),
        Metadata = gen_response_metadata(ReqRpc),
        Blob = normalized_blob(ResRpc),
        #{
            qrpc_map_calls => #{module => qrpc_rpc, type => rpc, pos => head}
          , metadata => Metadata
          , payload => get(ResRpc, [payload], null)
          , blob => Blob
        }
    catch
        ?QRPC_CATCH(QrpcError) ->
            #{
                qrpc_map_calls => #{module => qrpc_rpc, type => rpc, pos => head}
              , metadata => gen_error_detail(PartialReqRpc, QrpcError)
              , payload => null
            }
    end.


-spec gen_response_metadata(rpc()) -> response_metadata().
gen_response_metadata(ReqRpc) ->
    #{
        success => true
      , server_name => qrpc_conf:get(server_name)
      , type => response
      , mode => ReqRpc:get([metadata, mode])
      , protocol => ReqRpc:get([metadata, protocol])
      , request_id => ReqRpc:get([metadata, request_id])
      , rpc_uuid => ReqRpc:get([metadata, rpc_uuid])
    }.


-spec normalized_blob(rpc()) -> [blob()].
normalized_blob(Rpc) ->
    Blob0 = maps:get(blob, Rpc, []),
    Blob10 = case Blob0 of
        BlobInArray when is_list(BlobInArray) ->
            BlobInArray;
        BlobAsElement ->
            [BlobAsElement]
    end,
    lists:map(fun
        (B=#{data := Data}) when is_map(B), is_binary(Data) ->
            Data = maps:get(data, B, <<>>),
            #{
                content_type => klsn_binstr:from_any(
                    maps:get(content_type, B, <<"application/octet-stream">>)
                )
              , content_length => size(Data)
              , data => Data
            };
        (B) ->
            ?QRPC_ERROR(#{
                id => [qrpc, rpc, normalized_blob, invalid_blob]
              , fault_source => server
              , message => <<"server created an invalid blob">>
              , message_ja => <<"サーバーが無効な Blob を生成しました"/utf8>>
              , detail => #{
                    blob => B
                }
              , is_known => false
              , is_retryable => false
              , version => 1
            })
    end, Blob10).

-spec gen_error_detail(rpc(), qrpc_error:error()) -> response_metadata().
gen_error_detail(Rpc, QrpcError) ->
    klsn_map:filter(#{
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
      , mode => klsn_map:lookup([metadata, mode], Rpc)
      , protocol => klsn_map:lookup([metadata, protocol], Rpc)
      , request_id => klsn_map:lookup([metadata, request_id], Rpc)
      , rpc_uuid => klsn_map:lookup([metadata, rpc_uuid], Rpc)
    }).


-spec safe_parse_req(rpc()) -> rpc().
safe_parse_req(Rpc0) ->
    Rpc = case Rpc0 of
        RpcMap when is_map(RpcMap) ->
            RpcMap;
        _ ->
            #{}
    end,
    Lookup = fun
        (Key, Map) when is_map(Map) ->
            case klsn_map:lookup([Key], Map) of
                none ->
                    klsn_map:lookup([klsn_binstr:from_any(Key)], Map);
                Found ->
                    Found
            end;
        (_, _) ->
            none
    end,
    GetAtom = fun(Key, Map, Atoms) ->
        case Lookup(Key, Map) of
            {value, Enum} when is_atom(Enum); is_binary(Enum) ->
                SearchRes = lists:search(fun
                    (Atom) ->
                        klsn_binstr:from_any(Enum) =:= klsn_binstr:from_any(Atom)
                end, Atoms),
                case SearchRes of
                    {value, Atom} ->
                        Atom;
                    false ->
                        hd(Atoms)
                end;
            _ ->
                hd(Atoms)
        end
    end,
    Metadata0 = klsn_maybe:get_value(Lookup(metadata, Rpc), #{}),
    Metadata = case Metadata0 of
        MetadataMap when is_map(MetadataMap) ->
            MetadataMap;
        _ ->
            #{}
    end,
    Rpc#{
        metadata => Metadata#{
            mode => GetAtom(mode, Metadata, [normal])
          , protocol => GetAtom(protocol, Metadata, [native, http])
          , request_id => case Lookup(request_id, Metadata) of
                {value, RequestId} when is_binary(RequestId) ->
                    RequestId;
                {value, RequestId} ->
                    iolist_to_binary(io_lib:format("~p", [RequestId]));
                none ->
                    klsn_binstr:uuid()
            end
          , rpc_uuid => klsn_binstr:uuid()
        }
    }.

-spec parse_req(rpc()) -> rpc().
parse_req(Rpc) ->
    FieldError = fun
        (Path, none) ->
            ?QRPC_ERROR(#{
                id => [qrpc, rpc, parse_req, missing_required_field] ++ Path
              , fault_source => client
              , message => <<"Missing required field.">>
              , message_ja => <<"必須のフィールドが欠損しています。"/utf8>>
              , detail => #{
                    path => Path
                  , rpc => strip_rpc_for_log(Rpc)
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            });
        (Path, {value, BadValue}) ->
            ?QRPC_ERROR(#{
                id => [qrpc, rpc, parse_req, invalid_field_type] ++ Path
              , fault_source => client
              , message => <<"Invalid field type.">>
              , message_ja => <<"フィールドの型が不正です。"/utf8>>
              , detail => #{
                    path => Path
                  , value => BadValue
                  , rpc => strip_rpc_for_log(Rpc)
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    ToAtom = fun
        (Atom, _, []) when is_atom(Atom) ->
            Atom;
        (Binary, Path, []) when is_binary(Binary) ->
            try binary_to_existing_atom(Binary) catch
                error:badarg:Stack ->
                    ?QRPC_ERROR(#{
                        id => [qrpc, rpc, parse_req, unknown_atom] ++ Path
                      , fault_source => client
                      , message => <<"Unknown Erlang atom.">>
                      , message_ja => <<"未知の Erlang atom が含まれます。"/utf8>>
                      , detail => #{
                            path => Path
                          , value => Binary
                          , rpc => strip_rpc_for_log(Rpc)
                        }
                      , is_known => true
                      , is_retryable => false
                      , class => error
                      , reason => badarg
                      , stacktrace => Stack
                      , version => 1
                    })
            end; 
        (Binary, Path, Atoms) when is_binary(Binary); is_atom(Binary) ->
            SearchRes = lists:search(fun
                (Atom) ->
                    klsn_binstr:from_any(Binary) =:= klsn_binstr:from_any(Atom)
            end, Atoms),
            case SearchRes of
                {value, Atom} ->
                    Atom;
                false ->
                    ?QRPC_ERROR(#{
                        id => [qrpc, rpc, parse_req, invalid_enum] ++ Path
                      , fault_source => client
                      , message => <<"Invalid enum.">>
                      , message_ja => <<"不正な enum が含まれます。"/utf8>>
                      , detail => #{
                            path => Path
                          , value => Binary
                          , rpc => strip_rpc_for_log(Rpc)
                        }
                      , is_known => true
                      , is_retryable => false
                      , version => 1
                    })
            end
    end,
    Lookup = fun(Path, Fun) ->
        OptnlValue = case klsn_map:lookup(Path, Rpc) of
            {value, Value0} ->
                {value, Value0};
            none ->
                BinPath = lists:map(fun klsn_binstr:from_any/1, Path),
                klsn_map:lookup(BinPath, Rpc)
        end,
        try Fun(OptnlValue) catch
            error:function_clause ->
                FieldError(Path, OptnlValue)
        end
    end,
    GetAtom = fun(Path, Atoms) ->
        Lookup(Path, fun
            ({value, Atom}) when is_atom(Atom); is_binary(Atom) ->
                ToAtom(Atom, Path, Atoms)
        end)
    end,
    GetAtomOrHead = fun(Path, Atoms) ->
        Lookup(Path, fun
            ({value, Atom}) when is_atom(Atom); is_binary(Atom) ->
                GetAtom(Atom, Path, Atoms);
            (none) ->
                hd(Atoms)
        end)
    end,
    Lookup([metadata], fun({value, #{}}) -> checking_existence end),
    #{
        qrpc_map_calls => #{module => qrpc_rpc, type => rpc, pos => head}
      , metadata => #{
            type => GetAtomOrHead([metadata, type], [request])
          , mode => GetAtomOrHead([metadata, type], [normal])
          , protocol => GetAtomOrHead([metadata, type], [native, http])
          , client_name => Lookup([metadata, client_name], fun
                ({value, Value}) when is_binary(Value) ->
                    Value;
                (none) ->
                    <<>>
            end)
          , request_id => Lookup([metadata, request_id], fun
                ({value, Value}) when is_binary(Value) ->
                    Value
            end)
          , rpc_uuid => Lookup([metadata, rpc_uuid], fun
                ({value, Value}) when is_binary(Value) ->
                    Value
            end)
          , jwt => Lookup([metadata, jwt], fun
                ({value, Value}) when is_binary(Value) ->
                    qrpc_jwt:decode(Value);
                (none) ->
                    qrpc_jwt:issue(#{
                        ttl => 60
                      , tier => default
                      , role => public
                    })
            end)
          , module => GetAtom([metadata, module], [])
          , function => GetAtom([metadata, function], [])
          , arity => Lookup([metadata, arity], fun
                ({value, Value}) when is_integer(Value), Value >= 0 ->
                    Value
            end)
        }
      , payload => Lookup([payload], fun({value, Term}) -> Term end)
      , blob => [] % No client blob allowed for now
    }.

-spec process_rpc(rpc()) -> rpc().
process_rpc(Rpc) ->
    Module = Rpc:get([metadata, module]),
    Function = Rpc:get([metadata, function]),
    Arity = Rpc:get([metadata, arity]),
    Mfa = {Module, Function, Arity},
    IsAllowed = case qrpc_conf:get(allowed_rpc_mfa) of
        #{Mfa := #{role := public}} ->
            true;
        #{Mfa := #{role := Role}} ->
            JWT = Rpc:get([metadata, jwt]),
            JWT:consume(),
            case JWT:lookup(role) of
                {value, JWTRole} ->
                    is_role_allowed(#{
                        at_least => Role
                      , provided => JWTRole
                    });
                none -> false
            end;
        _ ->
            false
    end,
    case IsAllowed of
        true ->
            ok;
        false ->
            ?QRPC_ERROR(#{
                id => [qrpc, rpc, process_rpc, not_allowed]
              , fault_source => client
              , message => <<"Call not allowed.">>
              , message_ja => <<"当該呼び出しは許可されていません。"/utf8>>
              , detail => #{
                    rpc => strip_rpc_for_log(Rpc)
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    try 
        case Arity of
            1 ->
                Module:Function(Rpc);
            _ ->
                ?QRPC_ERROR(#{
                    id => [qrpc, rpc, process_rpc, unimplemented, arity]
                  , fault_source => server
                  , message => <<"Unimplemented arity.">>
                  , message_ja => <<"未実装の arity です。"/utf8>>
                  , detail => #{
                        rpc => strip_rpc_for_log(Rpc)
                      , arity => Arity
                    }
                  , is_known => true
                  , is_retryable => false
                  , version => 1
                })
        end
    catch
        throw:{?MODULE, {early_response, Return}} ->
            Return;
        error:undef:Stack ->
            case Stack of
                [{UModule, UFunction, UArgs, _}|_] when (
                    UModule =:= Module andalso
                    UFunction =:= Function andalso
                    UArgs =:= [Rpc]
                )->
                    ok;
                _ ->
                    erlang:raise(error, undef, Stack)
            end,
            ?QRPC_ERROR(#{
                id => [qrpc, rpc, process_rpc, mfa_undef]
              , fault_source => server
              , message => <<"Undefined function.">>
              , message_ja => <<"存在しない関数です。"/utf8>>
              , detail => #{
                    rpc => strip_rpc_for_log(Rpc)
                  , module => Module
                  , function => Function
                  , arity => Arity
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end.


-spec get(rpc(), klsn_list:path()) -> term().
get(Rpc, Path) ->
    case lookup(Rpc, Path) of
        {value, Value} ->
            Value;
        none ->
            ?QRPC_ERROR(#{
                id => [qrpc, rpc, get, not_found]
              , fault_source => server
              , message => <<"Missing backend specified field.">>
              , message_ja => <<"指定したフィールドが存在しませんでした。"/utf8>>
              , detail => #{
                    path => Path
                  , rpc => strip_rpc_for_log(Rpc)
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end.    

-spec get(rpc(), klsn_list:path(), term()) -> term().
get(Rpc, Path, Default) ->
    case lookup(Rpc, Path) of
        {value, Value} ->
            Value;
        none ->
            Default
    end.

-spec lookup(rpc(), klsn_list:path()) -> klsn:optnl(term()).
lookup(Rpc, Path) ->
    klsn_map:lookup(Path, Rpc).

-spec is_role_allowed(#{
        at_least := atom()
      , provided := klsn:binstr() | atom()
      , roles_left => [atom()]
    }) -> boolean().
is_role_allowed(#{at_least := Role, provided := JWTRole, roles_left := [Role|_]}) ->
    klsn_binstr:from_any(Role) =:= klsn_binstr:from_any(JWTRole);
is_role_allowed(#{at_least := Role, provided := JWTRole, roles_left := [H|T]}) ->
    klsn_binstr:from_any(H) =:= klsn_binstr:from_any(JWTRole) orelse
    is_role_allowed(#{at_least => Role, provided => JWTRole, roles_left => T});
is_role_allowed(#{at_least := Role, provided := JWTRole}) ->
    is_role_allowed(#{
        at_least => Role
      , provided => JWTRole
      , roles_left => qrpc_conf:get(roles)
    }).

-spec early_response(rpc()) -> no_return().
early_response(Rpc) ->
    erlang:throw({?MODULE, {early_response, Rpc}}).


-spec strip_rpc_for_log(rpc()) -> rpc().
strip_rpc_for_log(Rpc) ->
    Rpc.
