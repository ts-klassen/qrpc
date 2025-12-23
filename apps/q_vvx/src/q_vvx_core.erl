-module(q_vvx_core).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
        super_simple_tts/1
    ]).

-define(MAX_TEXT_BYTES, 1500).
-define(MINUTELY_LIMIT, 10000).

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
    ok = send_job(#{
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

ensure_supported_speaker(3) ->
    ok;
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
    Queue = queue_name(),
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

send_job(Payload) ->
    PrevTrap = process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(amqp_client),
    {ok, Conn} = amqp_connection:start(connection_params()),
    try
        {ok, Chan} = amqp_connection:open_channel(Conn),
        declare_queue(Chan),
        declare_exchange(Chan),
        bind_queue(Chan),
        Queue = queue_name(),
        Body = iolist_to_binary(json:encode(Payload)),
        Msg = #amqp_msg{
            props = #'P_basic'{content_type = <<"application/json">>, delivery_mode = 2},
            payload = Body
        },
        ok = amqp_channel:cast(
            Chan,
            #'basic.publish'{exchange = worker_exchange(), routing_key = Queue},
            Msg
        ),
        ensure_closed(catch amqp_channel:close(Chan))
    after
        process_flag(trap_exit, PrevTrap),
        ensure_closed(catch amqp_connection:close(Conn))
    end.

queue_name() ->
    <<"super_simple_tts">>.

worker_exchange() ->
    <<"super_simple_worker_exchange">>.

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

declare_queue(Chan) ->
    Queue = queue_name(),
    case catch amqp_channel:call(Chan, #'queue.declare'{
        queue = Queue,
        durable = true
    }) of
        #'queue.declare_ok'{} ->
            ok;
        {'EXIT', Reason} ->
            exit(Reason)
    end.

declare_exchange(Chan) ->
    Exchange = worker_exchange(),
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

bind_queue(Chan) ->
    Queue = queue_name(),
    Exchange = worker_exchange(),
    case catch amqp_channel:call(Chan, #'queue.bind'{
        queue = Queue,
        exchange = Exchange,
        routing_key = Queue
    }) of
        #'queue.bind_ok'{} ->
            ok;
        {'EXIT', Reason} ->
            exit(Reason)
    end.
