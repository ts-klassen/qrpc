#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
crate_dir="$repo_root/apps/q_vvx/crates/super_simple_worker"
bin_dir="$repo_root/apps/q_vvx/priv/bin"
target_bin="$crate_dir/target/release/super_simple_worker"

cd "$crate_dir"
/opt/qrpc/pkg/bin/cargo build --release

mkdir -p "$bin_dir"
install -m 755 "$target_bin" "$bin_dir/super_simple_worker"
