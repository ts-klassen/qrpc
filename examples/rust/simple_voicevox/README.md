# simple_voicevox Rust Example

Tiny CLI that synthesizes a WAV using [VOICEVOX Core](https://github.com/VOICEVOX/voicevox_core). Runtime assets are loaded from `/opt/qrpc/pkg/share/voicevox_core`.

## Prerequisites

- Rust toolchain
- VOICEVOX Core assets installed to `/opt/qrpc/pkg/share/voicevox_core`

`/opt/qrpc` is provisioned by running `./Build install` at the repository root.

## Run

```bash
cargo run
```

The program synthesizes a fixed sample text into `output.wav` under this directory using the first style in `0.vvm`.

The `voicevox_core` dependency is pinned to commit `91981ddf1fe37585d96e659dde12a3469a85ca68`.
