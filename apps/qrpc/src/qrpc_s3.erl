-module(qrpc_s3).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([
        create_bucket/4
      , list_buckets/2
      , make_presigned_url/3
      , get_bucket_attribute/4
    ]).

-export_type([
        access_key/0
      , config/0
      , bucket/0
      , key/0
      , ttl/0
      , http_method/0
      , s3_owner/0
      , s3_bucket_info/0
      , list_buckets_result/0
      , create_bucket_opts/0
      , presigned_url/0
      , make_presigned_url_opts/0
      , s3_acl_grantee/0
      , s3_acl_grant/0
      , s3_acl_attribute/0
      , s3_logging_attribute/0
      , s3_notification_filter/0
      , s3_notification_config/0
      , s3_notification_attribute/0
      , s3_bucket_attribute_name/0
      , s3_bucket_attribute/0
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

-type s3_owner() :: #{
        id => klsn:binstr()
      , display_name => klsn:binstr()
      , uri => klsn:binstr()
    }.

-type s3_acl_grantee() :: #{
        type := klsn:binstr()
      , id => klsn:binstr()
      , display_name => klsn:binstr()
      , uri => klsn:binstr()
    }.

-type s3_acl_grant() :: #{
        grantee := s3_acl_grantee()
      , permission := atom()
    }.

-type s3_acl_attribute() :: #{
        owner := s3_owner()
      , access_control_list := [s3_acl_grant()]
    }.

-type s3_logging_attribute() :: #{
        enabled := boolean()
      , target_bucket => klsn:binstr()
      , target_prefix => klsn:binstr()
      , target_grants => [s3_acl_grant()]
    }.

-type s3_notification_filter() :: #{
        name := atom()
      , value := klsn:binstr()
    }.

-type s3_notification_config() :: #{
        type := topic_configuration
              | queue_configuration
              | cloud_function_configuration
      , arn := klsn:binstr()
      , id => klsn:binstr()
      , events := [klsn:binstr()]
      , filter := [s3_notification_filter()]
    }.

-type s3_notification_attribute() :: [s3_notification_config()].
-type s3_bucket_attribute_name() :: erlcloud_s3:s3_bucket_attribute_name().

-type s3_bucket_attribute() ::
        s3_acl_attribute()
      | klsn:binstr()                      % location
      | s3_logging_attribute()             % logging
      | enabled | disabled | suspended     % mfa_delete, versioning
      | requester | bucket_owner           % request_payment
      | s3_notification_attribute().       % notification (normalized configs)

-type s3_bucket_info() :: #{
        name := klsn:binstr()
      , creation_date := calendar:datetime()
    }.

-type list_buckets_result() :: #{
        owner := s3_owner()
      , buckets := [s3_bucket_info()]
    }.

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
        bucket(), s3_bucket_attribute_name(), access_key(), config()
    ) -> s3_bucket_attribute().
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

-spec list_buckets(
        access_key(), config()
    ) -> list_buckets_result().
list_buckets(AKey, Conf) ->
    AWS = aws_config(AKey, Conf),
    Result = try erlcloud_s3:list_buckets(AWS) catch
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
        OwnerPL = proplists:get_value(owner, Result, []),
        Owner = klsn_map:filter(#{
            id => owner_field(id, OwnerPL),
            display_name => owner_field(display_name, OwnerPL),
            uri => owner_field(uri, OwnerPL)
        }),
        BucketsPL = proplists:get_value(buckets, Result, []),
        Buckets = lists:map(
          fun(BucketPL) ->
                  #{
                      name => klsn_binstr:from_any(proplists:get_value(name, BucketPL)),
                      creation_date => proplists:get_value(creation_date, BucketPL)
                  }
          end,
          BucketsPL
        ),
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
                  , erlcloud_result => Result
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

owner_field(Key, OwnerPL) ->
    case proplists:get_value(Key, OwnerPL, undefined) of
        undefined ->
            none;
        Value ->
            {value, klsn_binstr:from_any(Value)}
    end.

normalize_bucket_attribute(acl, Raw) ->
    OwnerPL = proplists:get_value(owner, Raw, []),
    Owner = klsn_map:filter(#{
        id => owner_field(id, OwnerPL),
        display_name => owner_field(display_name, OwnerPL),
        uri => owner_field(uri, OwnerPL)
    }),
    GrantsPL = proplists:get_value(access_control_list, Raw, []),
    Grants = lists:map(fun normalize_acl_grant/1, GrantsPL),
    #{
        owner => Owner,
        access_control_list => Grants
    };
normalize_bucket_attribute(location, Raw) ->
    klsn_binstr:from_any(Raw);
normalize_bucket_attribute(logging, {enabled, false}) ->
    #{enabled => false};
normalize_bucket_attribute(logging, Raw) when is_list(Raw) ->
    %% NOTE: erlcloud_s3:get_bucket_attribute/3 currently returns logging grants
    %% under the misspelled key 'target_trants'. We treat that as an upstream
    %% typo and normalize it here to the correctly named 'target_grants'.
    %% Raw is [{enabled,true}, {target_bucket,...}, {target_prefix,...}, {target_trants, GrantsPL}]
    Enabled = proplists:get_value(enabled, Raw, false),
    TargetBucket = proplists:get_value(target_bucket, Raw, undefined),
    TargetPrefix = proplists:get_value(target_prefix, Raw, undefined),
    TargetTrantsPL = proplists:get_value(target_trants, Raw, []),
    TargetTrants = lists:map(fun normalize_acl_grant/1, TargetTrantsPL),
    klsn_map:filter(#{
        enabled => {value, Enabled},
        target_bucket => maybe_bin(TargetBucket),
        target_prefix => maybe_bin(TargetPrefix),
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

normalize_acl_grant(GrantPL) ->
    GranteePL = proplists:get_value(grantee, GrantPL, []),
    Permission = proplists:get_value(permission, GrantPL),
    Grantee = klsn_map:filter(#{
        type => maybe_bin(proplists:get_value(type, GranteePL, undefined)),
        id => maybe_bin(proplists:get_value(id, GranteePL, undefined)),
        display_name => maybe_bin(proplists:get_value(display_name, GranteePL, undefined)),
        uri => maybe_bin(proplists:get_value(uri, GranteePL, undefined))
    }),
    #{
        grantee => Grantee,
        permission => Permission
    }.

normalize_notification_config(ConfType, ConfPL) ->
    Arn = case ConfType of
        topic_configuration -> proplists:get_value(topic, ConfPL);
        queue_configuration -> proplists:get_value(queue, ConfPL);
        cloud_function_configuration -> proplists:get_value(cloud_function, ConfPL)
    end,
    Id = proplists:get_value(id, ConfPL, undefined),
    %% erlcloud encodes notification events as a single `{event, [StringEvents]}`
    %% entry (see `?S3_BUCKET_EVENTS_LIST` in erlcloud). `get_all_values/2`
    %% would therefore yield a list whose only element is the entire list of
    %% string events, which in turn would be collapsed into a single binary.
    %% Instead, fetch the inner list and map over it so each event is preserved.
    Events = lists:map(
        fun klsn_binstr:from_any/1,
        proplists:get_value(event, ConfPL, [])
    ),
    Filter0 = proplists:get_value(filter, ConfPL, []),
    Filter =
        case Filter0 of
            {filter, List} ->
                lists:map(
                    fun({Name, Value}) ->
                        #{
                            name => Name,
                            value => klsn_binstr:from_any(Value)
                        }
                    end,
                    List
                );
            _ ->
                []
        end,
    klsn_map:filter(#{
        type => {value, ConfType},
        arn => maybe_bin(Arn),
        id => maybe_bin(Id),
        events => {value, Events},
        filter => {value, Filter}
    }).

maybe_bin(undefined) ->
    none;
maybe_bin(V) ->
    {value, klsn_binstr:from_any(V)}.
