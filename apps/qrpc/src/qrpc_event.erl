-module(qrpc_event).

-include_lib("qrpc/include/qrpc.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
        send/1
      , fetch/1
    ]).

-export_type([
        namespace/0
      , id/0
      , message/0
      , event_payload/0
      , fetch_payload/0
      , fetch_response/0
      , config/0
    ]).

-type namespace() :: klsn:binstr().
-type id() :: klsn:binstr().
-type message() :: klsn:binstr().

-type event_payload() :: #{
        namespace := namespace()
      , id := id()
      , message => message()
      , worker_id => klsn:binstr()
    }.

-type fetch_payload() :: #{
        namespace := namespace()
      , timeout => timeout()
    }.

-type fetch_response() :: #{
        payload := [event_payload()]
    }.

-type config() :: #{
        rabbitmq_host => klsn:binstr()
      , rabbitmq_port => inet:port_number()
      , rabbitmq_username => klsn:binstr()
      , rabbitmq_password => klsn:binstr()
      , rabbitmq_vhost => klsn:binstr()
    }.

-define(MAIN_QUEUE_TTL, 3600000).  % 60 minutes in milliseconds
-define(SUB_QUEUE_TTL, 60000).     % 1 minute in milliseconds
-define(MAIN_QUEUE_EXPIRES, 3900000).  % 65 minutes (TTL + 5 min grace)
-define(SUB_QUEUE_EXPIRES, 120000).    % 2 minutes (TTL + 1 min grace)
-define(EXCHANGE, <<"qrpc_event_exchange">>).

%%% Flow Diagram
%%%
%%% Send Event
%%% ----------
%%% client  server  mainqueue  subqueue
%%% |       |       |          |
%%% |send   |       |          |
%%% |------>|       |          |
%%% |       |publish|          |
%%% |       |------>|          |
%%% |       |  ok   |          |
%%% |       |<------|          |
%%% |  ok   |       |          |
%%% |<------|       |          |
%%%
%%% Fetch Event (complete flow)
%%% ---------------------------
%%% client  server  mainqueue  subqueue
%%% |       |       |          |
%%% |fetch  |       |          |
%%% |------>|       |          |
%%% |       |       |          |
%%% |       |---- Step 1: Get all from subqueue (timeout=0) ----
%%% |       |       |          |
%%% |       |get    |          |
%%% |       |-------|--------->|
%%% |       |msg(A) |          |
%%% |       |<------|----------|
%%% |       |publish|          | (add to end, resets TTL)
%%% |       |-------|--------->|
%%% |       |  ack  |          |
%%% |       |-------|--------->|
%%% |       |get    |          |
%%% |       |-------|--------->|
%%% |       |msg(B) |          |
%%% |       |<------|----------|
%%% |       |publish|          | (add to end, resets TTL)
%%% |       |-------|--------->|
%%% |       |  ack  |          |
%%% |       |-------|--------->|
%%% |       |get    |          |
%%% |       |-------|--------->|
%%% |       |msg(A) |          | (duplicate ID!)
%%% |       |<------|----------| (stop, no ack)
%%% |       |       |          |
%%% |       |---- Step 2: Get from mainqueue (timeout=user specified) ----
%%% |       |       |          |
%%% |       |consume|          |
%%% |       |------>|          |
%%% |       |   ok  |          |
%%% |       |<------|          |
%%% |       |(wait timeout...) |
%%% |       |msg(C) |          |
%%% |       |<------|          |
%%% |       |publish|          | (BEFORE ack!)
%%% |       |-------|--------->|
%%% |       |  ack  |          |
%%% |       |------>|          |
%%% |       |       |          |
%%% |       |---- Step 3: Return subqueue ++ mainqueue ----
%%% |       |       |          |
%%% |[A,B,C]|       |          |
%%% |<------|       |          |
%%%
%%% TTL Details
%%% -----------
%%% mainqueue: 60 min message TTL (set once on publish)
%%%            65 min queue expires (auto-delete if unused)
%%% subqueue:  1 min message TTL (resets on each republish)
%%%            2 min queue expires (auto-delete if unused)
%%%
%%% Key Points
%%% ----------
%%% - Subqueue uses circular buffer: get -> publish to end -> ack
%%% - Stop when duplicate ID seen (cycled through all messages)
%%% - Mainqueue: publish to subqueue BEFORE ack (safety)
%%% - Always return: SubEvents ++ MainEvents
%%% - Subqueue timeout = 0 (instant)
%%% - Mainqueue timeout = user specified
%%% - Queues auto-delete when unused (x-expires) for scalability
%%% - Supports 10^5+ namespaces/day via auto-cleanup
%%%
%%% Important: Queue Arguments Cannot Be Changed
%%% --------------------------------------------
%%% If external programs create queues with different arguments (e.g., without
%%% x-expires or x-message-ttl), queue.declare will fail with PRECONDITION_FAILED.
%%% External programs MUST use the same queue arguments if sharing namespaces.

%% API Functions

-spec send(#{payload := event_payload()}) -> #{payload := ok}.
send(#{payload := Payload0}) ->
    Payload = qrpc_sanitizer:normalize(server, event_payload_rule(), Payload0),
    Namespace = maps:get(namespace, Payload),
    Id = maps:get(id, Payload),
    Message = maps:get(message, Payload, <<>>),

    Config = get_config(),

    try
        {ok, Connection} = amqp_connection:start(connection_params(Config)),
        {ok, Channel} = amqp_connection:open_channel(Connection),

        %% Declare exchange
        ExchangeDeclare = #'exchange.declare'{
            exchange = ?EXCHANGE,
            type = <<"topic">>,
            durable = true
        },
        #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

        %% Declare queue with TTL and auto-expire
        QueueName = queue_name(Namespace),
        QueueDeclare = #'queue.declare'{
            queue = QueueName,
            durable = true,
            arguments = [
                {<<"x-message-ttl">>, long, ?MAIN_QUEUE_TTL},
                {<<"x-expires">>, long, ?MAIN_QUEUE_EXPIRES}
            ]
        },
        #'queue.declare_ok'{} = amqp_channel:call(Channel, QueueDeclare),

        %% Bind queue to exchange
        QueueBind = #'queue.bind'{
            queue = QueueName,
            exchange = ?EXCHANGE,
            routing_key = Namespace
        },
        #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

        %% Publish message
        EventData = encode_event(Namespace, Id, Message),
        Publish = #'basic.publish'{
            exchange = ?EXCHANGE,
            routing_key = Namespace
        },
        Props = #'P_basic'{
            delivery_mode = 2,  % persistent
            content_type = <<"application/json">>
        },
        Msg = #amqp_msg{
            props = Props,
            payload = EventData
        },
        ok = amqp_channel:cast(Channel, Publish, Msg),

        %% Cleanup
        ok = amqp_channel:close(Channel),
        ok = amqp_connection:close(Connection),

        #{payload => ok}
    catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, event, send, failed]
              , fault_source => external
              , message => <<"Failed to send event to RabbitMQ">>
              , message_ja => <<"RabbitMQ へのイベント送信に失敗しました"/utf8>>
              , detail => #{
                    namespace => Namespace
                  , id => Id
                  , message => Message
                }
              , is_known => false
              , is_retryable => true
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

-spec fetch(#{payload := fetch_payload()}) -> fetch_response().
fetch(#{payload := Payload0}) ->
    Payload = qrpc_sanitizer:normalize(server, fetch_payload_rule(), Payload0),
    Namespace = maps:get(namespace, Payload),
    Timeout = maps:get(timeout, Payload, 5000),

    Config = get_config(),

    try
        {ok, Connection} = amqp_connection:start(connection_params(Config)),
        {ok, Channel} = amqp_connection:open_channel(Connection),

        %% Declare exchange (idempotent)
        ExchangeDeclare = #'exchange.declare'{
            exchange = ?EXCHANGE,
            type = <<"topic">>,
            durable = true
        },
        #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

        %% Declare main queue with TTL and auto-expire
        QueueName = queue_name(Namespace),
        QueueDeclare = #'queue.declare'{
            queue = QueueName,
            durable = true,
            arguments = [
                {<<"x-message-ttl">>, long, ?MAIN_QUEUE_TTL},
                {<<"x-expires">>, long, ?MAIN_QUEUE_EXPIRES}
            ]
        },
        #'queue.declare_ok'{} = amqp_channel:call(Channel, QueueDeclare),

        %% Bind queue to exchange
        QueueBind = #'queue.bind'{
            queue = QueueName,
            exchange = ?EXCHANGE,
            routing_key = Namespace
        },
        #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

        %% Declare subqueue with 1 min TTL and auto-expire
        SubQueueName = sub_queue_name(Namespace),
        SubQueueDeclare = #'queue.declare'{
            queue = SubQueueName,
            durable = false,
            arguments = [
                {<<"x-message-ttl">>, long, ?SUB_QUEUE_TTL},
                {<<"x-expires">>, long, ?SUB_QUEUE_EXPIRES}
            ]
        },
        #'queue.declare_ok'{} = amqp_channel:call(Channel, SubQueueDeclare),

        %% Always get all messages from subqueue (timeout = 0, instant)
        SubEvents = get_all_from_queue(Channel, SubQueueName),

        %% Always try main queue with given timeout
        Subscribe = #'basic.consume'{
            queue = QueueName,
            no_ack = false
        },
        amqp_channel:subscribe(Channel, Subscribe, self()),

        %% Wait for consume_ok
        ConsumerTag = receive
            #'basic.consume_ok'{consumer_tag = Tag} -> Tag
        end,

        %% Wait for message or timeout
        MainEvents = receive
            {#'basic.deliver'{delivery_tag = DeliveryTag},
             #amqp_msg{payload = EventData}} ->
                %% Decode event
                Event = decode_event(EventData),

                %% Publish to subqueue BEFORE ack (safety: if process dies, message not lost)
                SubPublish = #'basic.publish'{
                    exchange = <<>>,
                    routing_key = SubQueueName
                },
                SubProps = #'P_basic'{
                    delivery_mode = 1,  % non-persistent
                    content_type = <<"application/json">>
                },
                SubMsg = #amqp_msg{
                    props = SubProps,
                    payload = EventData
                },
                ok = amqp_channel:cast(Channel, SubPublish, SubMsg),

                %% Ack the main queue AFTER publish to subqueue
                amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),

                [Event]
        after Timeout ->
                []
        end,

        %% Cancel subscription
        amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),

        %% Always return subqueue ++ mainqueue
        AllEvents = SubEvents ++ MainEvents,

        %% Cleanup
        ok = amqp_channel:close(Channel),
        ok = amqp_connection:close(Connection),

        #{payload => AllEvents}
    catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, event, fetch, failed]
              , fault_source => external
              , message => <<"Failed to fetch event from RabbitMQ">>
              , message_ja => <<"RabbitMQ からのイベント取得に失敗しました"/utf8>>
              , detail => #{
                    namespace => Namespace
                  , timeout => Timeout
                }
              , is_known => false
              , is_retryable => true
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

%% Internal Functions

-spec event_payload_rule() -> qrpc_sanitizer:rule().
event_payload_rule() ->
    #{
        namespace => {r, binstr}
      , id => {r, binstr}
      , message => {o, binstr}
      , worker_id => {o, binstr}
    }.

-spec fetch_payload_rule() -> qrpc_sanitizer:rule().
fetch_payload_rule() ->
    #{
        namespace => {r, binstr}
      , timeout => {o, timeout}
    }.

-spec connection_params(config()) -> #amqp_params_network{}.
connection_params(Config) ->
    #amqp_params_network{
        host = binary_to_list(maps:get(rabbitmq_host, Config)),
        port = maps:get(rabbitmq_port, Config),
        username = maps:get(rabbitmq_username, Config),
        password = maps:get(rabbitmq_password, Config),
        virtual_host = maps:get(rabbitmq_vhost, Config)
    }.

-spec get_config() -> config().
get_config() ->
    #{
        rabbitmq_host => qrpc_conf:get([rabbitmq, host], <<"localhost">>)
      , rabbitmq_port => qrpc_conf:get([rabbitmq, port], 5672)
      , rabbitmq_username => qrpc_conf:get([rabbitmq, username], <<"guest">>)
      , rabbitmq_password => qrpc_conf:get([rabbitmq, password], <<"guest">>)
      , rabbitmq_vhost => qrpc_conf:get([rabbitmq, vhost], <<"/">>)
    }.

-spec queue_name(namespace()) -> klsn:binstr().
queue_name(Namespace) ->
    <<"qrpc_event_", Namespace/binary>>.

-spec sub_queue_name(namespace()) -> klsn:binstr().
sub_queue_name(Namespace) ->
    <<"qrpc_event_sub_", Namespace/binary>>.

-spec encode_event(namespace(), id(), message()) -> binary().
encode_event(Namespace, Id, Message) ->
    iolist_to_binary(json:encode(#{
        <<"namespace">> => Namespace,
        <<"id">> => Id,
        <<"message">> => Message
    })).

-spec decode_event(binary()) -> event_payload().
decode_event(Binary) ->
    Map = json:decode(Binary),
    qrpc_sanitizer:normalize(external, event_payload_rule(), Map).

-spec get_all_from_queue(pid(), klsn:binstr()) -> [event_payload()].
get_all_from_queue(Channel, QueueName) ->
    get_all_from_queue(Channel, QueueName, [], sets:new()).

-spec get_all_from_queue(pid(), klsn:binstr(), [event_payload()], sets:set(id())) -> [event_payload()].
get_all_from_queue(Channel, QueueName, Acc, SeenIds) ->
    Get = #'basic.get'{queue = QueueName, no_ack = false},
    case amqp_channel:call(Channel, Get) of
        {#'basic.get_ok'{delivery_tag = Tag}, #amqp_msg{payload = Data}} ->
            Event = decode_event(Data),
            EventId = maps:get(id, Event),

            %% Check if we've already seen this ID (cycled through queue)
            case sets:is_element(EventId, SeenIds) of
                true ->
                    %% Already seen - we've cycled through all messages
                    %% Don't ack, just stop and return what we have
                    lists:reverse(Acc);
                false ->
                    %% New message - add to result, publish to end of queue, ack
                    SubPublish = #'basic.publish'{
                        exchange = <<>>,
                        routing_key = QueueName
                    },
                    SubProps = #'P_basic'{
                        delivery_mode = 1,  % non-persistent
                        content_type = <<"application/json">>
                    },
                    SubMsg = #amqp_msg{
                        props = SubProps,
                        payload = Data
                    },
                    ok = amqp_channel:cast(Channel, SubPublish, SubMsg),

                    %% Ack after publishing
                    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

                    %% Continue with this ID marked as seen
                    NewSeenIds = sets:add_element(EventId, SeenIds),
                    get_all_from_queue(Channel, QueueName, [Event | Acc], NewSeenIds)
            end;
        #'basic.get_empty'{} ->
            lists:reverse(Acc)
    end.
