-module(qrpc_s3).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([
        create_bucket/4
      , list_buckets/2
    ]).

-export_type([
        config/0
      , access_key/0
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

-type s3_owner() :: #{
        id => klsn:binstr()
      , display_name => klsn:binstr()
      , uri => klsn:binstr()
    }.

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

owner_field(Key, OwnerPL) ->
    case proplists:get_value(Key, OwnerPL, undefined) of
        undefined ->
            none;
        Value ->
            {value, klsn_binstr:from_any(Value)}
    end.
