super_simple_worker
===================

Prototype RabbitMQ worker that powers `super_simple_tts` by running VOICEVOX Core locally, uploading the generated WAV to a presigned S3 URL, then emitting a `qrpc_event` indicating completion.

## Requirements

- RabbitMQ reachable via `AMQP_ADDR` (defaults to `amqp://guest:guest@localhost:5672/%2f`)
- VOICEVOX Core assets installed under `/opt/qrpc/pkg/share/voicevox_core` (same layout as `examples/rust/simple_voicevox`)
- Presigned HTTPS PUT URLs for the browser uploads

## Building

```bash
cargo build
```

The crate uses the `voicevox_core` git dependency pinned to tag `0.16.2`, so the first build will pull that repository.

## Running

```bash
AMQP_ADDR=amqp://guest:guest@localhost:5672/%2f \
QUEUE_NAME=super_simple_tts \
VOICEVOX_ASSETS=/opt/qrpc/pkg/share/voicevox_core \
cargo run --release
```

Jobs are JSON blobs published to `QUEUE_NAME` with the following shape:

```json
{
  "style_id": 3,
  "text": "こんにちは！",
  "destination_url": "https://example.s3.amazonaws.com/presigned-put",
  "qrpc_event_namespace": "namespace",
  "qrpc_event_id": "message-id"
}
```

Environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `AMQP_ADDR` | `amqp://guest:guest@localhost:5672/%2f` | RabbitMQ connection string |
| `QUEUE_NAME` | `super_simple_tts` | Queue to consume jobs from |
| `VOICEVOX_ASSETS` | `/opt/qrpc/pkg/share/voicevox_core` | Root directory of VOICEVOX assets |
| `VOICEVOX_DICT` | `$VOICEVOX_ASSETS/dict/open_jtalk_dic_utf_8-1.11` | Override dictionary path |
| `VOICEVOX_MODEL` | `$VOICEVOX_ASSETS/models/vvms/0.vvm` | Voice model file to load |
| `VOICEVOX_ORT_LIB` | `$VOICEVOX_ASSETS/onnxruntime/lib/<libvoicevox_onnxruntime>` | Path to the VOICEVOX onnxruntime shared library |
| `EXPECTED_STYLE_ID` | `3` | Reject jobs whose `style_id` differs |

If multiple workers are desired, run additional OS processes; each worker keeps a single-threaded tokio runtime to accommodate the blocking VOICEVOX Core APIs.

## Erlang Shell Example

Quick test script to enqueue a job straight from an Erlang shell (assumes `amqp_client` is available and RabbitMQ matches the defaults above):

```erlang
application:ensure_all_started(amqp_client),
Params = #amqp_params_network{
    host = "localhost",
    port = 5672,
    username = <<"guest">>,
    password = <<"guest">>,
    virtual_host = <<"/">>
},
{ok, Conn} = amqp_connection:start(Params),
{ok, Chan} = amqp_connection:open_channel(Conn),
Queue = <<"super_simple_tts">>,
#'queue.declare_ok'{} =
    amqp_channel:call(Chan, #'queue.declare'{queue = Queue, durable = true}),
Payload = json:encode(#{
    <<"style_id">> => 3,
    <<"text">> => <<"こんにちは VOICEVOX!">>,
    <<"destination_url">> => <<"https://example.com/presigned-put-url">>,
    <<"qrpc_event_namespace">> => <<"demo_namespace">>,
    <<"qrpc_event_id">> => <<"demo_job_001">>
}),
Msg = #amqp_msg{
    props = #'P_basic'{content_type = <<"application/json">>, delivery_mode = 2},
    payload = Payload
},
ok = amqp_channel:cast(
    Chan,
    #'basic.publish'{exchange = <<>>, routing_key = Queue},
    Msg
),
ok = amqp_channel:close(Chan),
ok = amqp_connection:close(Conn).
```

Adjust the destination URL to a real presigned PUT target before running the command sequence.
