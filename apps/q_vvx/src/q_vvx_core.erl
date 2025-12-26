-module(q_vvx_core).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
        super_simple_tts/1
    ]).

-define(MAX_TEXT_BYTES, 1500).
-define(MINUTELY_LIMIT, 10000).
-define(MAX_STYLE_ID, 4294967295).
-define(DEFAULT_MODELS_PATH, <<"/opt/qrpc/pkg/etc/voicevox_core/models.json">>).
-define(STYLE_LIST_CACHE_KEY, {?MODULE, style_list}).

-export([load_style_map/0]).

super_simple_tts(Rpc) ->
    StyleId = Rpc:get([payload, <<"style_id">>]),
    ensure_supported_style(StyleId),
    Text = Rpc:get([payload, <<"text">>]),
    ensure_supported_text(Text),
    ensure_rate_limit(Text),
    #{  id := EventId
      , get := GetUrl
      , put := PutUrl
    } = qrpc_s3:share_urls(?QRPC_SUBCONF_GET(wav_s3_profiles)),
    % FIXME: This creates queue every time. Should use a per-session id
    EventNamespace = klsn_binstr:uuid(),
    ok = send_job(StyleId, #{
        <<"style_id">> => StyleId,
        <<"text">> => Text,
        <<"destination_url">> => PutUrl,
        <<"qrpc_event_namespace">> => EventNamespace,
        <<"qrpc_event_id">> => EventId
    }),
    #{
        payload => #{
            presigned_url => GetUrl,
            event_namespace => EventNamespace,
            event_id => EventId
        }
    }.

% FIXME: Only allow listed user
ensure_supported_style(StyleId)
    when is_integer(StyleId),
         StyleId >= 0,
         StyleId =< ?MAX_STYLE_ID ->
    case supported_style_map() of
        {ok, Styles} ->
            case maps:is_key(StyleId, Styles) of
                true ->
                    ok;
                false ->
                    ?QRPC_ERROR(#{
                        id => [q_vvx, core, super_simple_tts, unsupported_style]
                      , fault_source => client
                      , message => <<"Unsupported style">>
                      , message_ja => <<"未対応のスタイルです"/utf8>>
                      , detail => #{
                            style_id => StyleId
                        }
                      , is_known => true
                      , is_retryable => false
                      , version => 1
                    })
            end;
        {error, Reason} ->
            ?QRPC_ERROR(#{
                id => [q_vvx, core, super_simple_tts, style_list_unavailable]
              , fault_source => server
              , message => <<"Style list unavailable">>
              , message_ja => <<"スタイル一覧の読み込みに失敗しました"/utf8>>
              , detail => #{
                    reason => Reason
                  , path => models_path()
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end;
ensure_supported_style(StyleId) ->
    ?QRPC_ERROR(#{
        id => [q_vvx, core, super_simple_tts, unsupported_style]
      , fault_source => client
      , message => <<"Unsupported style">>
      , message_ja => <<"未対応のスタイルです"/utf8>>
      , detail => #{
            style_id => StyleId
        }
      , is_known => true
      , is_retryable => false
      , version => 1
    }).

supported_style_map() ->
    case persistent_term:get(?STYLE_LIST_CACHE_KEY, undefined) of
        {ok, Styles} ->
            {ok, Styles};
        undefined ->
            load_style_map()
    end.

load_style_map() ->
    Path = models_path(),
    case file:read_file(Path) of
        {ok, Binary} ->
            case parse_models(Binary, Path) of
                {ok, Styles} ->
                    persistent_term:put(?STYLE_LIST_CACHE_KEY, {ok, Styles}),
                    {ok, Styles};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {read_failed, Path, Reason}}
    end.

parse_models(Binary, Path) ->
    try json:decode(Binary) of
        Models when is_map(Models) ->
            case build_style_map(Models, Path) of
                {ok, Map} when map_size(Map) > 0 ->
                    {ok, Map};
                {ok, _} ->
                    {error, {empty_list, Path}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {invalid_format, Path}}
    catch Class:Reason ->
        {error, {decode_failed, Path, Class, Reason}}
    end.

build_style_map(Models, Path) ->
    maps:fold(
        fun(_, Value, {ok, Acc}) ->
            case Value of
                #{<<"metas">> := Metas} ->
                    add_styles_from_metas(Metas, Acc, Path);
                _ ->
                    {error, {invalid_entry, Path, Value}}
            end;
           (_, _, Error = {error, _}) ->
                Error
        end,
        {ok, #{}},
        Models
    ).

add_styles_from_metas(Metas, Acc, Path) when is_list(Metas) ->
    lists:foldl(
        fun(Character, {ok, Map}) ->
            case Character of
                #{<<"styles">> := Styles} when is_list(Styles) ->
                    add_style_ids(Styles, Map, Path);
                _ ->
                    {error, {invalid_metas, Path, Character}}
            end;
           (_, Error = {error, _}) ->
                Error
        end,
        {ok, Acc},
        Metas
    );
add_styles_from_metas(_, _Acc, Path) ->
    {error, {invalid_metas, Path}}.

add_style_ids([], Acc, _Path) ->
    {ok, Acc};
add_style_ids([Style | Rest], Acc, Path) ->
    case Style of
        #{<<"id">> := Id}
            when is_integer(Id),
                 Id >= 0,
                 Id =< ?MAX_STYLE_ID ->
            add_style_ids(Rest, maps:put(Id, true, Acc), Path);
        _ ->
            {error, {invalid_style, Path, Style}}
    end.

models_path() ->
    ?DEFAULT_MODELS_PATH.

ensure_supported_text(Text)
    when is_binary(Text),
         size(Text) > 0,
         size(Text) < ?MAX_TEXT_BYTES ->
    ok;
ensure_supported_text(Text) ->
    ?QRPC_ERROR(#{
        id => [q_vvx, core, super_simple_tts, unsupported_text]
      , fault_source => client
      , message => <<"Unsupported text">>
      , message_ja => <<"未対応の文字列です"/utf8>>
      , detail => #{
            text => Text
        }
      , is_known => true
      , is_retryable => false
      , version => 1
    }).

ensure_rate_limit(Text) ->
    Queue = rate_limit_key(),
    case qrpc_counter:add({?MODULE, super_simple_tts, Queue}, {slot, minutely}, size(Text)) of
        Count when Count < ?MINUTELY_LIMIT ->
            ok;
        Count ->
            Exp = qrpc_counter:parse_exp({slot, minutely}),
            ?QRPC_ERROR(#{
                id => [q_vvx, core, super_simple_tts, minutely_limit_exceeded]
              , fault_source => server
              , message => <<"minutely limit exceeded on server side">>
              , message_ja => <<"サーバー側の1分あたりの制限に達しました"/utf8>>
              , detail => #{
                    exp => Exp
                  , count => Count
                }
              , is_known => true
              , is_retryable => true
              , should_auto_retry => true
              , retry_after => Exp - erlang:system_time(second) + 1
              , version => 1
            })
    end.

send_job(StyleId, Payload) ->
    PrevTrap = process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(amqp_client),
    {ok, Conn} = amqp_connection:start(connection_params()),
    try
        {ok, Chan} = amqp_connection:open_channel(Conn),
        Queue = queue_name(StyleId),
        RoutingKey = routing_key(StyleId),
        declare_queue(Chan, Queue),
        declare_exchange(Chan, worker_exchange()),
        bind_queue(Chan, Queue, worker_exchange(), RoutingKey),
        Body = iolist_to_binary(json:encode(Payload)),
        Msg = #amqp_msg{
            props = #'P_basic'{content_type = <<"application/json">>, delivery_mode = 2},
            payload = Body
        },
        ok = amqp_channel:cast(
            Chan,
            #'basic.publish'{exchange = worker_exchange(), routing_key = RoutingKey},
            Msg
        ),
        declare_exchange(Chan, wake_exchange()),
        WakeBody = iolist_to_binary(json:encode(#{<<"style_id">> => StyleId})),
        WakeMsg = #amqp_msg{
            props = #'P_basic'{content_type = <<"application/json">>, delivery_mode = 1},
            payload = WakeBody
        },
        ok = amqp_channel:cast(
            Chan,
            #'basic.publish'{exchange = wake_exchange(), routing_key = wake_routing_key()},
            WakeMsg
        ),
        ensure_closed(catch amqp_channel:close(Chan))
    after
        process_flag(trap_exit, PrevTrap),
        ensure_closed(catch amqp_connection:close(Conn))
    end.

queue_name(StyleId) ->
    StyleBin = klsn_binstr:from_any(StyleId),
    <<"q_vvx_tts_", StyleBin/binary>>.

routing_key(StyleId) ->
    klsn_binstr:from_any(StyleId).

worker_exchange() ->
    <<"q_vvx_tts_exchange">>.

wake_exchange() ->
    <<"q_vvx_tts_wake_exchange">>.

wake_routing_key() ->
    <<"wake">>.

rate_limit_key() ->
    <<"q_vvx_tts">>.

connection_params() ->
    Host = qrpc_conf:get([rabbitmq, host], <<"localhost">>),
    #amqp_params_network{
        host = binary_to_list(Host),
        port = qrpc_conf:get([rabbitmq, port], 5672),
        username = qrpc_conf:get([rabbitmq, username], <<"guest">>),
        password = qrpc_conf:get([rabbitmq, password], <<"guest">>),
        virtual_host = qrpc_conf:get([rabbitmq, vhost], <<"/">>)
    }.

ensure_closed(ok) ->
    ok;
ensure_closed(closing) ->
    ok;
ensure_closed({'EXIT', _}) ->
    ok;
ensure_closed({'DOWN', _, process, _, _}) ->
    ok;
ensure_closed({error, Reason}) ->
    error(Reason).

declare_queue(Chan, Queue) ->
    case catch amqp_channel:call(Chan, #'queue.declare'{
        queue = Queue,
        durable = true
    }) of
        #'queue.declare_ok'{} ->
            ok;
        {'EXIT', Reason} ->
            exit(Reason)
    end.

declare_exchange(Chan, Exchange) ->
    case catch amqp_channel:call(Chan, #'exchange.declare'{
        exchange = Exchange,
        type = <<"direct">>,
        durable = true
    }) of
        #'exchange.declare_ok'{} ->
            ok;
        {'EXIT', Reason} ->
            exit(Reason)
    end.

bind_queue(Chan, Queue, Exchange, RoutingKey) ->
    case catch amqp_channel:call(Chan, #'queue.bind'{
        queue = Queue,
        exchange = Exchange,
        routing_key = RoutingKey
    }) of
        #'queue.bind_ok'{} ->
            ok;
        {'EXIT', Reason} ->
            exit(Reason)
    end.
