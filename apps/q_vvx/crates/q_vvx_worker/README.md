q_vvx_worker
============

VOICEVOX Core worker for per-style RabbitMQ queues with dynamic model loading.

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
```

Paths can be absolute or relative to `assets_dir`.
The worker discovers every `*.vvm` under `assets_dir/models/vvms` and uses VOICEVOX Core
metadata to map style IDs to their voice model files.
