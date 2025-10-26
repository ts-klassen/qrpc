-module(qrpc_jwt).
-include_lib("qrpc/include/qrpc.hrl").

-define(HS256, "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9").
-define(B64OPT, #{padding => false, mode => urlsafe}).

-export([
        issue/1
      , encode/1
      , decode/1
      , validate/1
      , get/2
      , lookup/2
      , set/3
      , consume/1
    ]).

-export_type([
        jwt/0
      , payload/0
      , issue/0
      , key/0
      , value/0
    ]).

-type jwt() :: klsn:binstr().

-define(PAYLOAD, #{
        qrpc_map_calls := #{module := qrpc_jwt, type := payload, pos := head}
    }).
-type payload() :: #{
        qrpc_map_calls := #{module := qrpc_jwt, type := payload, pos := head}
      , key() := value()
    }.

-type issue() :: #{
        ttl => pos_integer()
      , sub => klsn:binstr()
      , tier => atom()
      , role => atom()
    }.

-type key() :: atom() | klsn:binstr().

-type value() :: klsn:binstr() | integer() | boolean() | null.

-spec issue(issue()) -> payload().
issue(Opts) ->
    TimeNow = erlang:system_time(second),
    klsn_map:filter(#{
        qrpc_map_calls => {value, #{module => qrpc_jwt, type => payload, pos => head}}
      , <<"iss">> => qrpc_conf:lookup(server_name)
      , <<"iat">> => {value, TimeNow}
      , <<"exp">> => case Opts of
            #{ttl := TTL} -> {value, TimeNow + TTL};
            _ -> {value, TimeNow + 24 * 60 * 60}
        end
      , <<"jti">> => {value, klsn_binstr:uuid()}
      , <<"sub">> => klsn_map:lookup([sub], Opts)
      , <<"tier">> => case klsn_map:lookup([tier], Opts) of
            {value, Tier0} -> {value, klsn_binstr:from_any(Tier0)};
            none -> none
        end
      , <<"role">> => case klsn_map:lookup([role], Opts) of
            {value, Role0} -> {value, klsn_binstr:from_any(Role0)};
            none -> none
        end
    }).

-spec encode(payload()) -> jwt().
encode(Payload0=?PAYLOAD) ->
    Payload = maps:remove(qrpc_map_calls, Payload0),
    B64Payload = try
        base64:encode(iolist_to_binary(json:encode(Payload)), ?B64OPT)
    catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, jwt, encode, payload]
              , fault_source => server
              , message => <<"Invalid JWT payload.">>
              , message_ja => <<"JWT ペイロードが不正です。"/utf8>>
              , detail => #{
                    payload => Payload
                }
              , is_known => true
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    HP = <<?HS256, ".", B64Payload/binary>>,
    Signature = hs256(HP),
    <<HP/binary, ".", Signature/binary>>.

-spec decode(jwt()) -> payload().
decode(JWT = <<?HS256, ".", PS/binary>>) ->
    {P, S} = case binary:split(PS, <<".">>) of
        [P0, S0] ->
            {P0, S0};
        _ ->
            ?QRPC_ERROR(#{
                id => [qrpc, jwt, decode, signature_missing]
              , fault_source => client
              , message => <<"JWT signature missing.">>
              , message_ja => <<"JWT 署名が不足しています。"/utf8>>
              , detail => #{
                    alg => hs256
                  , jwt => JWT
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    HP = <<?HS256, ".", P/binary>>,
    case catch crypto:hash_equals(S, hs256(HP)) of
        true ->
            ok;
        _ ->
            ?QRPC_ERROR(#{
                id => [qrpc, jwt, decode, invalid_signature]
              , fault_source => client
              , message => <<"Invalid JWT signature.">>
              , message_ja => <<"JWT 署名が不正です。"/utf8>>
              , detail => #{
                    alg => hs256
                  , jwt => JWT
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    Payload0 = try json:decode(base64:decode(P, ?B64OPT)) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, jwt, decode, payload]
              , fault_source => client
              , message => <<"Invalid JWT payload.">>
              , message_ja => <<"JWT ペイロードが不正です。"/utf8>>
              , detail => #{
                    alg => hs256
                  , jwt => JWT
                }
              , is_known => true
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    Payload1 = Payload0#{
        qrpc_map_calls => #{module => qrpc_jwt, type => payload, pos => head}
    },
    %% Normalize tier/role to binstr if present
    Payload2 = case Payload1:lookup(role) of
        {value, R0} -> Payload1:set(role, klsn_binstr:from_any(R0));
        none -> Payload1
    end,
    Payload = case Payload2:lookup(tier) of
        {value, T0} -> Payload2:set(tier, klsn_binstr:from_any(T0));
        none -> Payload2
    end,
    Payload:validate(),
    Payload;
decode(JWT) ->
    ?QRPC_ERROR(#{
        id => [qrpc, jwt, decode, invalid_header]
      , fault_source => client
      , message => <<"Invalid JWT header.">>
      , message_ja => <<"JWT ヘッダが不正です。"/utf8>>
      , detail => #{
            jwt => JWT
        }
      , is_known => true
      , is_retryable => false
      , version => 1
    }).

-spec validate(payload()) -> ok.
validate(Payload=?PAYLOAD) ->
    TimeNow = erlang:system_time(second),
    case Payload:lookup(nbf) of
        none ->
            ok;
        {value, Nbf} ->
            case Nbf =< TimeNow of
                true ->
                    ok;
                false ->
                    ?QRPC_ERROR(#{
                        id => [qrpc, jwt, validate, nbf]
                      , fault_source => client
                      , message => <<"JWT not yet valid.">>
                      , message_ja => <<"JWT の有効時刻前です。"/utf8>>
                      , detail => #{
                            time_now => TimeNow
                          , nbf => Nbf
                        }
                      , is_known => true
                      , is_retryable => false
                      , version => 1
                    })
            end
    end,
    case Payload:lookup(exp) of
        none ->
            ok;
        {value, Exp} ->
            case Exp - TimeNow > 0 of
                true ->
                    ok;
                false ->
                    ?QRPC_ERROR(#{
                        id => [qrpc, jwt, validate, exp]
                      , fault_source => client
                      , message => <<"JWT expired.">>
                      , message_ja => <<"JWT の有効期限切れです。"/utf8>>
                      , detail => #{
                            time_now => TimeNow
                          , exp => Exp
                        }
                      , is_known => true
                      , is_retryable => false
                      , version => 1
                    })
            end
    end,
    ok.

-spec get(payload(), key()) -> value().
get(Payload=?PAYLOAD, Key) ->
    case lookup(Payload, Key) of
        {value, Value} ->
            Value;
        none ->
            ?QRPC_ERROR(#{
                id => [qrpc, jwt, get, missing_claim]
              , fault_source => server
              , message => <<"Missing JWT claim.">>
              , message_ja => <<"JWT のクレームが不足しています。"/utf8>>
              , detail => #{
                    key => Key
                  , payload => Payload
                }
              , is_known => false
              , is_retryable => false
              , version => 1
            })
    end.

-spec lookup(payload(), key()) -> klsn:optnl(value()).
lookup(Payload=?PAYLOAD, Key0) ->
    Key = klsn_binstr:from_any(Key0),
    klsn_map:lookup([Key], Payload).

-spec set(payload(), key(), value()) -> payload().
set(Payload=?PAYLOAD, Key0, Value) ->
    Key = klsn_binstr:from_any(Key0),
    klsn_map:upsert([Key], Value, Payload).

-spec consume(payload()) -> ok.
consume(Payload=?PAYLOAD) ->
    TierConf0 = qrpc_conf:get(tier, #{default => #{secondly => 1}}),
    TierConf = maps:from_list(lists:map(fun({Key, Value}) ->
        {klsn_binstr:from_any(Key), Value}
    end, maps:to_list(TierConf0))),
    Default = maps:get(<<"default">>, TierConf, #{secondly => 1}),
    Policy = case Payload:lookup(tier) of
        {value, Tier} ->
            maps:get(klsn_binstr:from_any(Tier), TierConf, Default);
        none ->
            Default
    end,
    OptnlExp = maps:fold(fun(Key, Value, Acc) ->
        {Id, Exp} = case Payload:lookup(sub) of
            {value, Sub} ->
                {Sub, qrpc_counter:parse_exp({slot, Key})};
            none ->
                {
                    Payload:get(jti)
                  , min(qrpc_counter:parse_exp({slot, Key}), Payload:get(exp))
                }
        end,
        case qrpc_counter:add({?MODULE, jti, Id, Key}, Exp, 1) > Value of
            true ->
                case Acc of
                    {value, AccExp} when AccExp > Exp ->
                        Acc;
                    _ ->
                        {value, qrpc_counter:parse_exp({slot, Key})}
                end;
            false ->
                Acc
        end
    end, none, Policy),
    case OptnlExp of
        {value, EExp} ->
            ?QRPC_ERROR(#{
                id => [qrpc, jwt, consume, exceeded]
              , fault_source => client
              , message => <<"Rate limit exceeded.">>
              , message_ja => <<"レートリミットに達しました。"/utf8>>
              , detail => #{
                    payload => Payload
                }
              , is_known => true
              , is_retryable => true
              , should_auto_retry => true
              , retry_after => EExp - erlang:system_time(second) + 1
              , version => 1
            });
        none ->
            ok
    end.

-spec hs256(klsn:binstr()) -> klsn:binstr().
hs256(Data) ->
    base64:encode(
        crypto:mac(
            hmac,
            sha256,
            qrpc_conf:get(jwt_hs256_secret),
            Data
        ),
        ?B64OPT
    ).
