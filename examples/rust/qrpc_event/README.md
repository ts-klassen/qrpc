# qrpc_event Rust Example

Simple example demonstrating how to send events to qrpc_event from Rust using lapin.

## Requirements

- RabbitMQ running on localhost:5672
- Rust toolchain

## Usage

Send an event:
```bash
cargo run -- <namespace> <id> <message>
```

Examples:
```bash
# Simple event
cargo run -- my_namespace msg_001 "Hello from Rust"

# Default values (if no args)
cargo run
```

## Important: Queue Arguments

External programs MUST use the same queue arguments as qrpc_event:

- `x-message-ttl`: 3600000 (60 minutes)
- `x-expires`: 3900000 (65 minutes)
- `durable`: true

If queue already exists with different arguments, `queue_declare` will fail with PRECONDITION_FAILED.

## Event Format

Events are JSON encoded:
```json
{
  "namespace": "my_namespace",
  "id": "msg_001",
  "message": "Hello from Rust"
}
```

All fields must be strings (binaries).
