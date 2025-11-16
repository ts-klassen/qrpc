-module(qrpc_s3).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% I HATE proplist(). Use map().
%% I HATE record(). Use map().
%% I HATE string(). Use klsn:binstr() aka binary().
%% I HATE undefined. Omit the field in map.
%% I HATE null. Use klsn:optnl(V) :: {value, V} | none.
%%  NEVER maps:get(Key, Map, undefined) -> Value | undefined.
%%    USE klsn_map:lookup([Key], Map) -> {value, Value} | none.
%%    USE klsn_map:filter(#{Key => klsn_map:lookup([Key], Map)}).
%%  AVOID #{ Field := klsn:optnl(Value) }
%%    USE #{ Field => Value }
%% I HATE {error, term()}. Use ?QRPC_ERROR(qrpc_error:partial_payload()).
%% I HATE specs and type definitions with term(), any(), map(), list().
%%
%% Here are thy types and specs of what you will be using.
%% I am using term() and map() because it REALY can by anything.
%% Do NOT use term() nor map() in your case unless it REALY can be anything.
%% In most cases, AI generated code with term() is instantly rejected.
%%  -type klsn:optnl(Value) :: {value, Value} | none.
%%  -type klsn_map:path() :: [term()].
%%  -spec klsn_map:lookup(klsn_map:path(), map()) -> klsn:optnl(term()).
%%  -spec filter(maps:map(Key, klsn:optnl(Value))) -> maps:map(Key, Value).
%%
%% Here is an example:
%%  1> Map = #{field1 => value1},
%%     MaybeField1 = klsn_map:lookup([field1], Map).
%%  {value,value1}
%%  2> MaybeField2 = klsn_map:lookup([field2], Map).
%%  none
%%  3> klsn_map:filter(#{new_field1 => MaybeField1, new_field2 => MaybeField2}).
%%   #{new_field1 => value1}

-export([
        create_bucket/4
      , list_buckets/2
      , make_presigned_url/3
      , get_bucket_lifecycle/3
      , put_bucket_lifecycle/4
      , delete_bucket_lifecycle/3
      , get_bucket_attribute/4
      , set_bucket_attribute/5
    ]).

-export_type([
        access_key/0
      , config/0
      , bucket/0
      , key/0
      , ttl/0
      , http_method/0
      , owner/0
      , bucket_info/0
      , list_buckets_result/0
      , create_bucket_opts/0
      , presigned_url/0
      , make_presigned_url_opts/0
      , acl_grantee/0
      , acl_grant/0
      , acl_attribute/0
      , logging_attribute/0
      , notification_filter/0
      , notification_config/0
      , notification_attribute/0
      , bucket_attribute_name_get/0
      , bucket_attribute_name_set/0
      , bucket_attribute_get/0
      , bucket_attribute_set/0
      , bucket_lifecycle/0
      , lifecycle_rule/0
      , lifecycle_expiration/0
      , lifecycle_transition/0
      , lifecycle_noncurrent_version_transition/0
      , lifecycle_noncurrent_version_expiration/0
    ]).

-type access_key() :: #{
        access_key_id := klsn:binstr()
      , secret_access_key := klsn:binstr()
    }.

-type config() :: #{
        s3_scheme => klsn:binstr()
      , s3_host => klsn:binstr()
      , s3_port => inet:port_number()
      , s3_follow_redirect => boolean()
      , s3_follow_redirect_count => non_neg_integer()
      , s3_bucket_access_method => vhost | path | auto
      , s3_bucket_after_host => boolean()
    }.

-type bucket() :: klsn:binstr().

-type key() :: klsn:binstr().

%% HTTP method allowed in presigning
-type http_method() :: get | put | head | delete.

-type owner() :: #{
        id => klsn:binstr()
      , display_name => klsn:binstr()
      , uri => klsn:binstr()
    }.

-type acl_grantee() :: #{
        type := klsn:binstr()
      , id => klsn:binstr()
      , display_name => klsn:binstr()
      , uri => klsn:binstr()
    }.

-type acl_grant() :: #{
        grantee := acl_grantee()
      , permission := atom()
    }.

-type acl_attribute() :: #{
        owner := owner()
      , access_control_list := [acl_grant()]
    }.

-type logging_attribute() :: #{
        enabled := boolean()
      , target_bucket => klsn:binstr()
      , target_prefix => klsn:binstr()
      , target_grants => [acl_grant()]
    }.

-type notification_filter() :: #{
        name := atom()
      , value := klsn:binstr()
    }.

-type notification_config() :: #{
        type := topic_configuration
              | queue_configuration
              | cloud_function_configuration
      , arn := klsn:binstr()
      , id => klsn:binstr()
      , events := [klsn:binstr()]
      , filter := [notification_filter()]
    }.

-type notification_attribute() :: [notification_config()].
-type bucket_attribute_name_get() :: erlcloud_s3:s3_bucket_attribute_name().
-type bucket_attribute_name_set() ::
        acl
      | logging
      | mfa_delete
      | request_payment
      | versioning
      | notification.

-type mfa_delete_attribute_get() :: enabled | disabled.
-type versioning_attribute_get() :: enabled | disabled | suspended.

-type mfa_delete_attribute_set() ::
        #{
           status := enabled | suspended,
           mfa_delete => enabled | disabled
         }.

-type versioning_attribute_set() ::
        #{
           status := enabled | suspended,
           mfa_delete => enabled | disabled
         }.

-type bucket_attribute_get() ::
        acl_attribute()
      | klsn:binstr()                      % location
      | logging_attribute()                % logging
      | mfa_delete_attribute_get()         % mfa_delete
      | versioning_attribute_get()         % versioning
      | requester | bucket_owner           % request_payment
      | notification_attribute().          % notification (normalized configs)

-type bucket_attribute_set() ::
        acl_attribute()
      | logging_attribute()                % logging
      | mfa_delete_attribute_set()         % mfa_delete
      | versioning_attribute_set()         % versioning
      | requester | bucket_owner           % request_payment
      | notification_attribute().          % notification (normalized configs)

-type bucket_info() :: #{
        name := klsn:binstr()
      , creation_date := calendar:datetime()
    }.

-type list_buckets_result() :: #{
        owner := owner()
      , buckets := [bucket_info()]
    }.

-type lifecycle_expiration() :: #{
        date => klsn:binstr(),
        days => non_neg_integer()
    }.

-type lifecycle_transition() :: #{
        date => klsn:binstr(),
        days => non_neg_integer(),
        storage_class := klsn:binstr()
    }.

-type lifecycle_noncurrent_version_transition() :: #{
        noncurrent_days := non_neg_integer(),
        storage_class   := klsn:binstr()
    }.

-type lifecycle_noncurrent_version_expiration() :: #{
        noncurrent_days := non_neg_integer()
    }.

-type lifecycle_rule() :: #{
        id => klsn:binstr(),
        prefix := klsn:binstr(),
        status := enabled | disabled,
        expiration => lifecycle_expiration(),
        noncurrent_version_expiration => lifecycle_noncurrent_version_expiration(),
        noncurrent_version_transition => lifecycle_noncurrent_version_transition(),
        transition => lifecycle_transition()
    }.

-type bucket_lifecycle() :: [lifecycle_rule()].

-type create_bucket_opts() :: #{
        s3_bucket_acl => erlcloud_s3:s3_bucket_acl()
      , s3_location_constraint => erlcloud_s3:s3_location_constraint()
    }.

-type presigned_url() :: klsn:binstr().
-type make_presigned_url_opts() :: #{
        bucket := bucket(),
        key := key(),
        method => http_method(),      % default: get
        ttl => ttl(),                 % default: 3600
        region => klsn:binstr()       % default: <<"us-east-1">>
    }.

%% time to live (second) from now
-type ttl() :: non_neg_integer().

-spec create_bucket(
        bucket(), create_bucket_opts(), access_key(), config()
    ) -> ok.
create_bucket(Bucket, Opts, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    BucketStr = binary_to_list(Bucket),
    Acl = maps:get(s3_bucket_acl, Opts, private),
    Location = maps:get(s3_location_constraint, Opts, none),
    try erlcloud_s3:create_bucket(BucketStr, Acl, Location, AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, create_bucket, failed]
              , fault_source => external
              , message => <<"S3 bucket creation failed">>
              , message_ja => <<"S3 バケットの作成に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , create_bucket_opts => Opts
                  , config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

-spec get_bucket_attribute(
        bucket(), bucket_attribute_name_get(), access_key(), config()
    ) -> bucket_attribute_get().
get_bucket_attribute(Bucket, AttributeName, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    BucketStr = binary_to_list(Bucket),
    Raw = try erlcloud_s3:get_bucket_attribute(BucketStr, AttributeName, AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, get_bucket_attribute, failed]
              , fault_source => external
              , message => <<"Getting S3 bucket attribute failed">>
              , message_ja => <<"S3 バケット属性の取得に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , attribute_name => AttributeName
                  , config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    try
        normalize_bucket_attribute(AttributeName, Raw)
    catch
        Class1:Reason1:Stack1 ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, get_bucket_attribute, invalid_response]
              , fault_source => external
              , message => <<"Received invalid response from external api">>
              , message_ja => <<"外部 API から不正な応答がありました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , attribute_name => AttributeName
                  , config => Conf
                  , erlcloud_result => Raw
                }
              , is_known => false
              , is_retryable => false
              , class => Class1
              , reason => Reason1
              , stacktrace => Stack1
              , version => 1
            })
    end.

-spec get_bucket_lifecycle(
        bucket(), access_key(), config()
    ) -> bucket_lifecycle().
get_bucket_lifecycle(Bucket, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    BucketStr = binary_to_list(Bucket),
    Raw = try erlcloud_s3:get_bucket_lifecycle(BucketStr, AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, get_bucket_lifecycle, failed]
              , fault_source => external
              , message => <<"Getting S3 bucket lifecycle failed">>
              , message_ja => <<"S3 バケットライフサイクル設定の取得に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    case Raw of
        {ok, RulesPL} ->
            try
                normalize_bucket_lifecycle(RulesPL)
            catch
                Class1:Reason1:Stack1 ->
                    ?QRPC_ERROR(#{
                        id => [qrpc, s3, get_bucket_lifecycle, invalid_response]
                      , fault_source => external
                      , message => <<"Received invalid response from external api">>
                      , message_ja => <<"外部 API から不正な応答がありました"/utf8>>
                      , detail => #{
                            bucket => Bucket
                          , config => Conf
                          , erlcloud_result => Raw
                        }
                      , is_known => false
                      , is_retryable => false
                      , class => Class1
                      , reason => Reason1
                      , stacktrace => Stack1
                      , version => 1
                    })
            end;
        {error, Reason2} ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, get_bucket_lifecycle, failed]
              , fault_source => external
              , message => <<"Getting S3 bucket lifecycle failed">>
              , message_ja => <<"S3 バケットライフサイクル設定の取得に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , config => Conf
                  , erlcloud_result => Raw
                }
              , is_known => false
              , is_retryable => false
              , class => error
              , reason => Reason2
              , stacktrace => []
              , version => 1
            })
    end.

-spec put_bucket_lifecycle(
        bucket(), bucket_lifecycle(), access_key(), config()
    ) -> ok.
put_bucket_lifecycle(Bucket, Policy, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    BucketStr = binary_to_list(Bucket),
    PolicyPL = denormalize_bucket_lifecycle(Policy),
    Raw = try erlcloud_s3:put_bucket_lifecycle(BucketStr, PolicyPL, AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, put_bucket_lifecycle, failed]
              , fault_source => external
              , message => <<"Setting S3 bucket lifecycle failed">>
              , message_ja => <<"S3 バケットライフサイクル設定の更新に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , policy => Policy
                  , config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    case Raw of
        ok ->
            ok;
        {error, Reason2} ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, put_bucket_lifecycle, failed]
              , fault_source => external
              , message => <<"Setting S3 bucket lifecycle failed">>
              , message_ja => <<"S3 バケットライフサイクル設定の更新に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , policy => Policy
                  , config => Conf
                  , erlcloud_result => Raw
                }
              , is_known => false
              , is_retryable => false
              , class => error
              , reason => Reason2
              , stacktrace => []
              , version => 1
            })
    end.

-spec delete_bucket_lifecycle(
        bucket(), access_key(), config()
    ) -> ok.
delete_bucket_lifecycle(Bucket, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    BucketStr = binary_to_list(Bucket),
    Raw = try erlcloud_s3:delete_bucket_lifecycle(BucketStr, AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, delete_bucket_lifecycle, failed]
              , fault_source => external
              , message => <<"Deleting S3 bucket lifecycle failed">>
              , message_ja => <<"S3 バケットライフサイクル設定の削除に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    case Raw of
        ok ->
            ok;
        {error, Reason2} ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, delete_bucket_lifecycle, failed]
              , fault_source => external
              , message => <<"Deleting S3 bucket lifecycle failed">>
              , message_ja => <<"S3 バケットライフサイクル設定の削除に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , config => Conf
                  , erlcloud_result => Raw
                }
              , is_known => false
              , is_retryable => false
              , class => error
              , reason => Reason2
              , stacktrace => []
              , version => 1
            })
    end.

-spec set_bucket_attribute(
        bucket(), bucket_attribute_name_set(), bucket_attribute_set(), access_key(), config()
    ) -> ok.
set_bucket_attribute(Bucket, AttributeName, Attribute, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    BucketStr = binary_to_list(Bucket),
    RawAttribute = denormalize_bucket_attribute(AttributeName, Attribute),
    try erlcloud_s3:set_bucket_attribute(BucketStr, AttributeName, RawAttribute, AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, set_bucket_attribute, failed]
              , fault_source => external
              , message => <<"Setting S3 bucket attribute failed">>
              , message_ja => <<"S3 バケット属性の設定に失敗しました"/utf8>>
              , detail => #{
                    bucket => Bucket
                  , attribute_name => AttributeName
                  , attribute => Attribute
                  , config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

-spec list_buckets(
        access_key(), config()
    ) -> list_buckets_result().
list_buckets(AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    Result0 = try erlcloud_s3:list_buckets(AWS) catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, list_buckets, failed]
              , fault_source => external
              , message => <<"Listing S3 bucket failed">>
              , message_ja => <<"S3 バケットの一覧取得に失敗しました"/utf8>>
              , detail => #{
                    config => Conf
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end,
    try
        Result = p2m(Result0),
        Owner0 = p2m(maps:get(owner, Result, [])),
        Owner = klsn_map:filter(#{
            id => s2b(klsn_map:lookup([id], Owner0)),
            display_name => s2b(klsn_map:lookup([display_name], Owner0)),
            uri => s2b(klsn_map:lookup([uri], Owner0))
        }),
        Buckets = lists:map(fun(Bucket0) ->
            Bucket = p2m(Bucket0),
            #{
                name => s2b(maps:get(name, Bucket)),
                creation_date => maps:get(creation_date, Bucket)
            }
        end, maps:get(buckets, Result, [])),
        #{
            owner => Owner
          , buckets => Buckets
        }
    catch
        Class1:Reason1:Stack1 ->
            ?QRPC_ERROR(#{
                id => [qrpc, s3, list_buckets, invalid_response]
              , fault_source => external
              , message => <<"Received invalid response from external api">>
              , message_ja => <<"外部 API から不正な応答がありました"/utf8>>
              , detail => #{
                    config => Conf
                  , erlcloud_result => Result0
                }
              , is_known => false
              , is_retryable => false
              , class => Class1
              , reason => Reason1
              , stacktrace => Stack1
              , version => 1
            })
    end.

-spec make_presigned_url(
        make_presigned_url_opts(), access_key(), config()
    ) -> presigned_url().
make_presigned_url(Opts, AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    Bucket = maps:get(bucket, Opts),
    Key = maps:get(key, Opts),
    MethodAtom = maps:get(method, Opts, get),
    TTL = maps:get(ttl, Opts, 3600),
    RegionBin = maps:get(region, Opts, <<"us-east-1">>),
    Region = binary_to_list(RegionBin),
    Method = string:to_upper(atom_to_list(MethodAtom)),
    Now = erlang:universaltime(),
    AmzDate = iso8601_basic_time(Now),
    DateScope = lists:sublist(AmzDate, 8),
    Service = "s3",
    SignedHeaders = "host",
    Host = presign_host(Bucket, AWS),
    HostWithPort = lists:flatten([Host, port_spec(AWS)]),
    Path = presign_path(Bucket, Key, AWS),
    Credential = lists:flatten([
        AWS#aws_config.access_key_id, $/,
        DateScope, $/, Region, $/, Service, "/aws4_request"
    ]),
    QParams0 = [
        {"X-Amz-Algorithm", "AWS4-HMAC-SHA256"},
        {"X-Amz-Credential", Credential},
        {"X-Amz-Date", AmzDate},
        {"X-Amz-Expires", integer_to_list(TTL)},
        {"X-Amz-SignedHeaders", SignedHeaders}
    ],
    QParams1 = case AWS#aws_config.security_token of
        undefined -> QParams0;
        Token -> QParams0 ++ [{"X-Amz-Security-Token", Token}]
    end,
    CanonicalQueryString = canonical_query_string(QParams1),
    PayloadHash = "UNSIGNED-PAYLOAD",
    CanonicalHeaders = lists:flatten(["host:", HostWithPort, "\n"]),
    CanonicalRequest = lists:flatten([
        Method, $\n,
        Path, $\n,
        CanonicalQueryString, $\n,
        CanonicalHeaders, $\n,
        SignedHeaders, $\n,
        PayloadHash
    ]),
    CanonicalRequestHash = hash_encode(CanonicalRequest),
    CredentialScope = lists:flatten([
        DateScope, $/, Region, $/, Service, "/aws4_request"
    ]),
    StringToSign = lists:flatten([
        "AWS4-HMAC-SHA256\n",
        AmzDate, $\n,
        CredentialScope, $\n,
        CanonicalRequestHash
    ]),
    SigningKey = signing_key(DateScope, Region, Service, AWS#aws_config.secret_access_key),
    SignatureBin = erlcloud_util:sha256_mac(SigningKey, StringToSign),
    Signature = hex_encode(SignatureBin),
    FinalQueryString = lists:flatten([
        CanonicalQueryString,
        "&X-Amz-Signature=", Signature
    ]),
    URL = lists:flatten([
        AWS#aws_config.s3_scheme,
        HostWithPort,
        Path, $?,
        FinalQueryString
    ]),
    klsn_binstr:from_any(URL).


%% Internal private functions %%

-spec aws_config(access_key(), config()) -> erlcloud_s3:config().
aws_config(AccessKey, Config) ->
    PlainConfig = #aws_config{},
    %% In aws_config of erlcloud, it has all the host address of aws.
    %% Even if I'm only using s3, I need to send this huge config
    %% with all the non related host addresses.
    %% This is scary because it might send a request to real aws service.
    %% So, I decided to fill every `*_host` fields with fake host.
    FakeHostConfig = begin
        Fields = record_info(fields, aws_config),
        [aws_config|Values] = tuple_to_list(PlainConfig),
        Pairs = lists:zip(Fields, Values),
        FakeValues = lists:map(fun
            % s3_bucket_after_host=false::boolean() is fine.
            ({_, Boolean}) when is_boolean(Boolean) ->
                Boolean;
            ({Field, Val}) ->
                case lists:suffix("_host", atom_to_list(Field)) of
                    true  -> "qrpc-aws-fake-host.local";
                    false -> Val
                end
        end, Pairs),
        list_to_tuple([aws_config | FakeValues])
    end,
    FakeHostConfig#aws_config{
        access_key_id = binary_to_list(maps:get(access_key_id, AccessKey))
      , secret_access_key = binary_to_list(maps:get(secret_access_key, AccessKey))
      , s3_scheme = binary_to_list(maps:get(s3_scheme, Config, <<"http://">>))
      , s3_host = binary_to_list(maps:get(s3_host, Config, <<"localhost">>))
      , s3_port = maps:get(s3_port, Config, 9000)
      , s3_follow_redirect = maps:get(s3_follow_redirect, Config, false)
      , s3_follow_redirect_count = maps:get(s3_follow_redirect_count, Config, 2)
      , s3_bucket_access_method = maps:get(s3_bucket_access_method, Config, path)
      , s3_bucket_after_host = maps:get(s3_bucket_after_host, Config, false)
    }.

iso8601_basic_time({{Yr, Mo, Da}, {H, M, S}}) ->
    lists:flatten(
      io_lib:format("~4.10.0b~2.10.0b~2.10.0bT~2.10.0b~2.10.0b~2.10.0bZ",
                    [Yr, Mo, Da, H, M, S])).

presign_host(Bucket, AWS) ->
    case AWS#aws_config.s3_bucket_access_method of
        vhost ->
            lists:flatten([
                binary_to_list(Bucket), $.,
                AWS#aws_config.s3_host
            ]);
        _ ->
            AWS#aws_config.s3_host
    end.

presign_path(Bucket, Key, AWS) ->
    EncodedKey = erlcloud_http:url_encode_loose(binary_to_list(Key)),
    case AWS#aws_config.s3_bucket_access_method of
        vhost ->
            lists:flatten(["/", EncodedKey]);
        _ ->
            lists:flatten([
                "/", binary_to_list(Bucket), "/", EncodedKey
            ])
    end.

signing_key(DateScope, Region, Service, SecretAccessKey) ->
    KDate    = erlcloud_util:sha256_mac("AWS4" ++ SecretAccessKey, DateScope),
    KRegion  = erlcloud_util:sha256_mac(KDate, Region),
    KService = erlcloud_util:sha256_mac(KRegion, Service),
    erlcloud_util:sha256_mac(KService, "aws4_request").

canonical_query_string(Params) ->
    Encoded =
        [begin
             KEnc = erlcloud_http:url_encode(K),
             VEnc = erlcloud_http:url_encode(V),
             {KEnc, VEnc}
         end || {K, V} <- Params],
    Sorted = lists:sort(Encoded),
    Pairs = [lists:flatten([K1, "=", V1]) || {K1, V1} <- Sorted],
    string:join(Pairs, "&").

hash_encode(Data) ->
    Bin = erlang:iolist_to_binary(Data),
    hex_encode(crypto:hash(sha256, Bin)).

hex_encode(Bin) when is_binary(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).

port_spec(#aws_config{s3_port = 80}) -> "";
port_spec(#aws_config{s3_port = 443}) -> "";
port_spec(#aws_config{s3_port = Port}) ->
    [":", erlang:integer_to_list(Port)].

normalize_bucket_lifecycle(RulesPL) ->
    lists:map(fun normalize_lifecycle_rule/1, RulesPL).

normalize_lifecycle_rule(RulePL) ->
    Rule = p2m(RulePL),
    Exp0 = klsn_map:lookup([expiration], Rule),
    NonCurrentExp0 = klsn_map:lookup([noncurrent_version_expiration], Rule),
    NonCurrentTrans0 = klsn_map:lookup([noncurrent_version_transition], Rule),
    Trans0 = klsn_map:lookup([transition], Rule),
    StatusBin = s2b(maps:get(status, Rule)),
    Status = case StatusBin of
        <<"Enabled">> -> enabled;
        <<"Disabled">> -> disabled;
        <<"ENABLED">> -> enabled;
        <<"DISABLED">> -> disabled;
        _ -> disabled
    end,
    klsn_map:filter(#{
        id => s2b(klsn_map:lookup([id], Rule)),
        prefix => {value, s2b(maps:get(prefix, Rule))},
        status => {value, Status},
        expiration => normalize_lifecycle_expiration(Exp0),
        noncurrent_version_expiration =>
            normalize_noncurrent_version_expiration(NonCurrentExp0),
        noncurrent_version_transition =>
            normalize_noncurrent_version_transition(NonCurrentTrans0),
        transition => normalize_lifecycle_transition(Trans0)
    }).

normalize_lifecycle_expiration(none) ->
    none;
normalize_lifecycle_expiration({value, ExpirationPL}) ->
    Expiration = p2m(ExpirationPL),
    {value, klsn_map:filter(#{
        date => s2b(klsn_map:lookup([date], Expiration)),
        days => klsn_map:lookup([days], Expiration)
    })}.

normalize_lifecycle_transition(none) ->
    none;
normalize_lifecycle_transition({value, TransitionPL}) ->
    Transition = p2m(TransitionPL),
    {value, klsn_map:filter(#{
        date => s2b(klsn_map:lookup([date], Transition)),
        days => klsn_map:lookup([days], Transition),
        storage_class => {value, s2b(maps:get(storage_class, Transition))}
    })}.

normalize_noncurrent_version_transition(none) ->
    none;
normalize_noncurrent_version_transition({value, NonCurrentTransPL}) ->
    NonCurrentTrans = p2m(NonCurrentTransPL),
    {value, #{
        noncurrent_days => maps:get(noncurrent_days, NonCurrentTrans),
        storage_class => s2b(maps:get(storage_class, NonCurrentTrans))
    }}.

normalize_noncurrent_version_expiration(none) ->
    none;
normalize_noncurrent_version_expiration({value, NonCurrentExpPL}) ->
    NonCurrentExp = p2m(NonCurrentExpPL),
    {value, #{
        noncurrent_days => maps:get(noncurrent_days, NonCurrentExp)
    }}.

denormalize_bucket_lifecycle(Policy) ->
    lists:map(fun denormalize_lifecycle_rule/1, Policy).

denormalize_lifecycle_rule(Rule) ->
    Status = case maps:get(status, Rule, undefined) of
        enabled -> <<"Enabled">>;
        disabled -> <<"Disabled">>;
        Other -> klsn_binstr:from_any(Other)
    end,
    RuleMap = klsn_map:filter(#{
        id => s2b(klsn_map:lookup([id], Rule)),
        prefix => s2b(klsn_map:lookup([prefix], Rule)),
        status => {value, Status},
        expiration => klsn_map:lookup([expiration], Rule),
        noncurrent_version_expiration =>
            klsn_map:lookup([noncurrent_version_expiration], Rule),
        noncurrent_version_transition =>
            klsn_map:lookup([noncurrent_version_transition], Rule),
        transition =>
            klsn_map:lookup([transition], Rule)
    }),
    bin_map_to_str_proplist(RuleMap).

normalize_bucket_attribute(acl, Raw0) ->
    Raw = p2m(Raw0),
    Owner0 = p2m(maps:get(owner, Raw, [])),
    Owner = klsn_map:filter(#{
        id => s2b(klsn_map:lookup([id], Owner0)),
        display_name => s2b(klsn_map:lookup([display_name], Owner0)),
        uri => s2b(klsn_map:lookup([uri], Owner0))
    }),
    Grants0 = maps:get(access_control_list, Raw, []),
    Grants = lists:map(fun normalize_acl_grant/1, Grants0),
    #{
        owner => Owner,
        access_control_list => Grants
    };
normalize_bucket_attribute(location, Raw) ->
    s2b(Raw);
normalize_bucket_attribute(logging, {enabled, false}) ->
    #{enabled => false};
normalize_bucket_attribute(logging, Raw0) when is_list(Raw0) ->
    %% NOTE: erlcloud_s3:get_bucket_attribute/3 currently returns logging grants
    %% under the misspelled key 'target_trants'. We treat that as an upstream
    %% typo and normalize it here to the correctly named 'target_grants'.
    %% Raw is [{enabled,true}, {target_bucket,...}, {target_prefix,...}, {target_trants, GrantsPL}]
    Raw = p2m(Raw0),
    TargetTrants0 = maps:get(target_trants, Raw, []),
    TargetTrants = lists:map(fun normalize_acl_grant/1, TargetTrants0),
    klsn_map:filter(#{
        enabled => {value, maps:get(enabled, Raw, false)},
        target_bucket => s2b(klsn_map:lookup([target_bucket], Raw)),
        target_prefix => s2b(klsn_map:lookup([target_prefix], Raw)),
        target_grants => {value, TargetTrants}
    });
normalize_bucket_attribute(mfa_delete, Raw) ->
    Raw;
normalize_bucket_attribute(request_payment, Raw) ->
    Raw;
normalize_bucket_attribute(versioning, Raw) ->
    Raw;
normalize_bucket_attribute(notification, Raw) when is_list(Raw) ->
    %% erlcloud returns notification attributes as a list of single-element
    %% lists, each containing a {ConfType, ConfigPL} pair. ConfigPL itself
    %% is a proplist describing the configuration rather than a list of
    %% configuration proplists.
    lists:map(
        fun ([{ConfType, ConfigPL}]) ->
                normalize_notification_config(ConfType, ConfigPL)
        end,
        Raw
    ).

normalize_acl_grant(Grant0) ->
    Grant = p2m(Grant0),
    Grantee0 = p2m(maps:get(grantee, Grant, [])),
    Permission = maps:get(permission, Grant),
    Grantee = klsn_map:filter(#{
        type => {value, s2b(maps:get(type, Grantee0))},
        id => s2b(klsn_map:lookup([id], Grantee0)),
        display_name => s2b(klsn_map:lookup([display_name], Grantee0)),
        uri => s2b(klsn_map:lookup([uri], Grantee0))
    }),
    #{
        grantee => Grantee,
        permission => Permission
    }.

normalize_notification_config(ConfType, ConfPL) ->
    Conf = p2m(ConfPL),
    Arn0 = case ConfType of
        topic_configuration -> maps:get(topic, Conf);
        queue_configuration -> maps:get(queue, Conf);
        cloud_function_configuration -> maps:get(cloud_function, Conf)
    end,
    Arn = s2b(Arn0),
    %% erlcloud encodes notification events as a single `{event, [StringEvents]}`
    %% entry (see `?S3_BUCKET_EVENTS_LIST` in erlcloud). `get_all_values/2`
    %% would therefore yield a list whose only element is the entire list of
    %% string events, which in turn would be collapsed into a single binary.
    %% Instead, fetch the inner list and map over it so each event is preserved.
    Events = lists:map(
        fun s2b/1,
        maps:get(event, Conf)
    ),
    Filter = lists:map(fun({Name, Value}) ->
        #{
            name => Name,
            value => s2b(Value)
        }
    end, maps:get(filter, Conf, [])),
    klsn_map:filter(#{
        type => {value, ConfType},
        arn => {value, Arn},
        id => s2b(klsn_map:lookup([id], Conf)),
        events => {value, Events},
        filter => {value, Filter}
    }).

denormalize_bucket_attribute(acl, Attr) ->
    Owner = maps:get(owner, Attr, #{}),
    Grants = maps:get(access_control_list, Attr, []),
    OwnerPL = bin_map_to_str_proplist(Owner),
    GrantsPL = lists:map(fun denormalize_acl_grant/1, Grants),
    [
        {owner, OwnerPL},
        {access_control_list, GrantsPL}
    ];
denormalize_bucket_attribute(logging, Attr) ->
    Enabled = maps:get(enabled, Attr, false),
    case Enabled of
        false ->
            [{enabled, false}];
        true ->
            TargetGrants = maps:get(target_grants, Attr, []),
            TargetGrantsPL = lists:map(fun denormalize_acl_grant/1, TargetGrants),
            PLMap = klsn_map:filter(#{
                enabled => {value, true},
                target_bucket => klsn_map:lookup([target_bucket], Attr),
                target_prefix => klsn_map:lookup([target_prefix], Attr),
                target_grants => {value, TargetGrantsPL}
            }),
            bin_map_to_str_proplist(PLMap)
    end;
denormalize_bucket_attribute(mfa_delete, Attr) when is_map(Attr) ->
    PLMap = klsn_map:filter(#{
        status => {value, klsn_map:get([status], Attr)},
        mfa_delete => klsn_map:lookup([mfa_delete], Attr)
    }),
    bin_map_to_str_proplist(PLMap);
denormalize_bucket_attribute(mfa_delete, Attr) ->
    Attr;
denormalize_bucket_attribute(request_payment, Attr) ->
    Attr;
denormalize_bucket_attribute(versioning, Attr) when is_map(Attr) ->
    PLMap = klsn_map:filter(#{
        status => {value, klsn_map:get([status], Attr)},
        mfa_delete => klsn_map:lookup([mfa_delete], Attr)
    }),
    bin_map_to_str_proplist(PLMap);
denormalize_bucket_attribute(versioning, Attr) ->
    Attr;
denormalize_bucket_attribute(notification, Attr) when is_list(Attr) ->
    lists:map(fun denormalize_notification_config/1, Attr).

denormalize_acl_grant(Grant) ->
    Grantee = maps:get(grantee, Grant, #{}),
    Permission = maps:get(permission, Grant),
    GranteePL = bin_map_to_str_proplist(Grantee),
    [
        {grantee, GranteePL},
        {permission, Permission}
    ].

denormalize_notification_config(Conf) ->
    ConfType = maps:get(type, Conf),
    Arn = maps:get(arn, Conf),
    ArnStr = binary_to_list(Arn),
    Events = maps:get(events, Conf, []),
    Filter0 = maps:get(filter, Conf, []),
    Filter =
        case Filter0 of
            [] ->
                none;
            List when is_list(List) ->
                FilterList =
                    lists:map(
                      fun(F) ->
                          {maps:get(name, F),
                           binary_to_list(maps:get(value, F))}
                      end,
                      List
                     ),
                {value, FilterList}
        end,
    BaseMap = klsn_map:filter(#{
        id => klsn_map:lookup([id], Conf),
        event => {value, Events},
        filter => Filter
    }),
    ConfPLBase = bin_map_to_str_proplist(BaseMap),
    ConfPL =
        case ConfType of
            topic_configuration ->
                [{topic, ArnStr} | ConfPLBase];
            queue_configuration ->
                [{queue, ArnStr} | ConfPLBase];
            cloud_function_configuration ->
                [{cloud_function, ArnStr} | ConfPLBase]
        end,
    [{ConfType, ConfPL}].

bin_map_to_str_proplist(Map) when is_map(Map) ->
    maps:to_list(maps:map(fun(_, Value) ->
        bin_map_to_str_proplist(Value)
    end, Map));
bin_map_to_str_proplist(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
bin_map_to_str_proplist(List) when is_list(List) ->
    lists:map(fun(Elem) ->
        bin_map_to_str_proplist(Elem)
    end, List);
bin_map_to_str_proplist(Term) ->
    Term.

-spec s2b(klsn:binstr()) -> klsn:binstr();
         (string()) -> klsn:binstr();
         ({value, klsn:binstr() | string()}) -> {value, klsn:binstr()};
         (none) -> none.
s2b(String) when is_list(String) ->
    iolist_to_binary(String);
s2b({value, Value}) ->
    {value, s2b(Value)};
s2b(Other) ->
    Other.

-spec p2m(proplists:proplist()) -> map().
p2m(Proplist) ->
    proplists:to_map(Proplist).
