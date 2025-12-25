q_vvx_worker
============

VOICEVOX Core worker for per-speaker RabbitMQ queues with dynamic model loading.

Configuration
-------------

The worker reads TOML config from `/opt/qrpc/etc/q_vvx_worker/config.toml` by default.
Override with `--config /path/to/config.toml`.

Example config:

```toml
amqp_addr = "amqp://guest:guest@localhost:5672/%2f"
assets_dir = "/opt/qrpc/pkg/share/voicevox_core"
http_timeout_secs = 30

# Optional overrides for assets_dir-relative paths
# dict_path = "dict/open_jtalk_dic_utf_8-1.11"
# onnxruntime_path = "onnxruntime/lib/libvoicevox_onnxruntime.so"

[[speakers]]
id = 3
model = "models/vvms/0.vvm"

[[speakers]]
id = 8
model = "models/vvms/1.vvm"
```

Paths can be absolute or relative to `assets_dir`.
