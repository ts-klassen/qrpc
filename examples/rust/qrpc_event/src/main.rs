use lapin::{
    options::*, types::FieldTable, BasicProperties, Connection, ConnectionProperties,
    ExchangeKind,
};
use serde_json::json;

/// Simple example of sending an event to qrpc_event
///
/// Usage: cargo run -- <namespace> <id> <message>
/// Example: cargo run -- my_namespace msg_001 "Hello from Rust"
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let namespace = args.get(0).map(String::as_str).unwrap_or("test_namespace");
    let id = args.get(1).map(String::as_str).unwrap_or("msg_001");
    let message = args.get(2).map(String::as_str).unwrap_or("Hello from Rust!");

    // Connect to RabbitMQ
    let addr = "amqp://guest:guest@localhost:5672/%2f";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    // Declare exchange (idempotent)
    channel
        .exchange_declare(
            "qrpc_event_exchange",
            ExchangeKind::Topic,
            ExchangeDeclareOptions {
                durable: true,
                ..Default::default()
            },
            FieldTable::default(),
        )
        .await?;

    // Declare queue with same arguments as Erlang code
    let queue_name = format!("qrpc_event_{}", namespace);
    let mut queue_args = FieldTable::default();
    queue_args.insert("x-message-ttl".into(), 3600000i64.into()); // 60 min
    queue_args.insert("x-expires".into(), 3900000i64.into()); // 65 min

    channel
        .queue_declare(
            &queue_name,
            QueueDeclareOptions {
                durable: true,
                ..Default::default()
            },
            queue_args,
        )
        .await?;

    // Bind queue to exchange
    channel
        .queue_bind(
            &queue_name,
            "qrpc_event_exchange",
            namespace,
            QueueBindOptions::default(),
            FieldTable::default(),
        )
        .await?;

    // Create JSON event payload
    let event = json!({
        "namespace": namespace,
        "id": id,
        "message": message
    });
    let payload = serde_json::to_vec(&event)?;

    // Publish to exchange
    channel
        .basic_publish(
            "qrpc_event_exchange",
            namespace,
            BasicPublishOptions::default(),
            &payload,
            BasicProperties::default()
                .with_delivery_mode(2) // persistent
                .with_content_type("application/json".into()),
        )
        .await?
        .await?; // Wait for confirmation

    println!(" [x] Sent event to namespace '{}'", namespace);
    println!("     ID: {}", id);
    println!("     Message: {}", message);

    conn.close(0, "").await?;

    Ok(())
}
