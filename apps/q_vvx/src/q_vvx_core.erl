-module(q_vvx_core).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
        super_simple_tts/1
    ]).

-define(MAX_TEXT_BYTES, 1500).
-define(MINUTELY_LIMIT, 10000).
-define(MAX_SPEAKER_ID, 4294967295).
-define(DEFAULT_SPEAKER_LIST_PATH, <<"/opt/qrpc/etc/q_vvx_worker/speakers.json">>).
-define(SPEAKER_LIST_CACHE_KEY, {?MODULE, speaker_list}).

super_simple_tts(Rpc) ->
    Speaker = Rpc:get([payload, <<"id">>]),
    ensure_supported_speaker(Speaker),
    Text = Rpc:get([payload, <<"text">>]),
    ensure_supported_text(Text),
    ensure_rate_limit(Text),
    #{  id := EventId
      , get := GetUrl
      , put := PutUrl
    } = qrpc_s3:share_urls(?QRPC_SUBCONF_GET(wav_s3_profiles)),
    % FIXME: This creates queue every time. Should use a per-session id
    EventNamespace = klsn_binstr:uuid(),
    ok = send_job(Speaker, #{
        <<"speaker_id">> => Speaker,
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
ensure_supported_speaker(Speaker)
    when is_integer(Speaker),
         Speaker >= 0,
         Speaker =< ?MAX_SPEAKER_ID ->
    case supported_speaker_map() of
        {ok, Speakers} ->
            case maps:is_key(Speaker, Speakers) of
                true ->
                    ok;
                false ->
                    ?QRPC_ERROR(#{
                        id => [q_vvx, core, super_simple_tts, unsupported_speaker]
                      , fault_source => client
                      , message => <<"Unsupported speaker">>
                      , message_ja => <<"未対応の話者です"/utf8>>
                      , detail => #{
                            speaker => Speaker
                        }
                      , is_known => true
                      , is_retryable => false
                      , version => 1
                    })
            end;
        {error, Reason} ->
            ?QRPC_ERROR(#{
                id => [q_vvx, core, super_simple_tts, speaker_list_unavailable]
              , fault_source => server
              , message => <<"Speaker list unavailable">>
              , message_ja => <<"話者一覧の読み込みに失敗しました"/utf8>>
              , detail => #{
                    reason => Reason
                  , path => speaker_list_path()
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end;
ensure_supported_speaker(Speaker) ->
    ?QRPC_ERROR(#{
        id => [q_vvx, core, super_simple_tts, unsupported_speaker]
      , fault_source => client
      , message => <<"Unsupported speaker">>
      , message_ja => <<"未対応の話者です"/utf8>>
      , detail => #{
            speaker => Speaker
        }
      , is_known => true
      , is_retryable => false
      , version => 1
    }).

supported_speaker_map() ->
    case persistent_term:get(?SPEAKER_LIST_CACHE_KEY, undefined) of
        {ok, Speakers} ->
            {ok, Speakers};
        undefined ->
            load_speaker_map()
    end.

load_speaker_map() ->
    Path = speaker_list_path(),
    case file:read_file(Path) of
        {ok, Binary} ->
            case parse_speaker_list(Binary, Path) of
                {ok, Speakers} ->
                    persistent_term:put(?SPEAKER_LIST_CACHE_KEY, {ok, Speakers}),
                    {ok, Speakers};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {read_failed, Path, Reason}}
    end.

parse_speaker_list(Binary, Path) ->
    try json:decode(Binary) of
        #{<<"speakers">> := Speakers} when is_list(Speakers) ->
            case build_speaker_map(Speakers, #{}) of
                {ok, Map} when map_size(Map) > 0 ->
                    {ok, Map};
                {ok, _} ->
                    {error, {empty_list, Path}};
                {error, Reason} ->
                    {error, {invalid_entry, Path, Reason}}
            end;
        _ ->
            {error, {invalid_format, Path}}
    catch Class:Reason ->
        {error, {decode_failed, Path, Class, Reason}}
    end.

build_speaker_map([], Acc) ->
    {ok, Acc};
build_speaker_map([Speaker | Rest], Acc) ->
    case Speaker of
        #{<<"id">> := Id}
            when is_integer(Id),
                 Id >= 0,
                 Id =< ?MAX_SPEAKER_ID ->
            build_speaker_map(Rest, maps:put(Id, true, Acc));
        _ ->
            {error, Speaker}
    end.

speaker_list_path() ->
    qrpc_conf:get(
        [subsystem, q_vvx, q_vvx_core, speaker_list_path],
        ?DEFAULT_SPEAKER_LIST_PATH
    ).

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

send_job(Speaker, Payload) ->
    PrevTrap = process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(amqp_client),
    {ok, Conn} = amqp_connection:start(connection_params()),
    try
        {ok, Chan} = amqp_connection:open_channel(Conn),
        Queue = queue_name(Speaker),
        RoutingKey = routing_key(Speaker),
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
        WakeBody = iolist_to_binary(json:encode(#{<<"speaker_id">> => Speaker})),
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

queue_name(Speaker) ->
    SpeakerBin = klsn_binstr:from_any(Speaker),
    <<"q_vvx_tts_", SpeakerBin/binary>>.

routing_key(Speaker) ->
    klsn_binstr:from_any(Speaker).

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
