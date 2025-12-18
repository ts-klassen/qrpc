-module(q_vvx_tmp).

% Delete this when you are done.

-include_lib("amqp_client/include/amqp_client.hrl").

-export([send_example_job/1]).

%% @doc Helper to enqueue a demo super_simple_worker job from the shell.
%%      The only required argument is a presigned S3 PUT URL (binary).
send_example_job(DestinationUrl) when is_binary(DestinationUrl) ->
    PrevTrap = process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(amqp_client),
    {ok, Conn} = amqp_connection:start(connection_params()),
    try
        {ok, Chan} = amqp_connection:open_channel(Conn),
        declare_queue(Chan),
        declare_exchange(Chan),
        bind_queue(Chan),
        Queue = queue_name(),
        Payload = iolist_to_binary(json:encode(sample_payload(DestinationUrl))),
        Msg = #amqp_msg{
            props = #'P_basic'{content_type = <<"application/json">>, delivery_mode = 2},
            payload = Payload
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

sample_payload(DestinationUrl) ->
    #{
        <<"speaker_id">> => 3,
        <<"text">> => <<"こんにちは VOICEVOX!"/utf8>>,
        <<"destination_url">> => DestinationUrl,
        <<"qrpc_event_namespace">> => <<"demo_namespace">>,
        <<"qrpc_event_id">> => <<"demo_job_001">>
    }.

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
