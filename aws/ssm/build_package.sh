#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <repo_url> <tag> <s3_bucket> <s3_prefix_unused>" >&2
}

if [ "$#" -lt 4 ]; then
  usage
  exit 1
fi

REPO_URL="$1"
TAG="$2"
S3_BUCKET="$3"
S3_PREFIX_UNUSED="$4"

expected_kernel="5.15.0"
expected_arch="x86_64"
expected_version="22.04"

actual_kernel="$(uname -r | cut -d- -f1)"
actual_arch="$(uname -m)"
actual_version=""

if [ -r /etc/os-release ]; then
  . /etc/os-release
  actual_version="${VERSION_ID:-}"
fi

if [ "$actual_kernel" != "$expected_kernel" ] || [ "$actual_arch" != "$expected_arch" ] || [ "$actual_version" != "$expected_version" ]; then
  echo "Build host requirements not met. kernel=$actual_kernel arch=$actual_arch version=$actual_version" >&2
  exit 1
fi

if ! command -v aws >/dev/null 2>&1; then
  echo "aws CLI missing on build host" >&2
  exit 1
fi

workdir="/opt/qrpc-build"
repo_dir="$workdir/qrpc"

mkdir -p "$workdir"
rm -rf "$repo_dir"

git clone --depth 1 --branch "$TAG" "$REPO_URL" "$repo_dir"

cd "$repo_dir"

export QRPC_BUILD_SERVER=1
./Build bootstrap
previous_devel_key="$(aws s3api list-objects-v2 \
  --bucket "$S3_BUCKET" \
  --query "Contents[?ends_with(Key, '-devel.tar.gz')]|sort_by(@,&LastModified)[-1].Key" \
  --output text 2>/dev/null || true)"

if [ -n "$previous_devel_key" ] && [ "$previous_devel_key" != "None" ]; then
  tmp_dir="$(mktemp -d)"
  tmp_tar="$tmp_dir/qrpc-devel.tar.gz"
  aws s3 cp "s3://$S3_BUCKET/$previous_devel_key" "$tmp_tar"
  tar -zxf "$tmp_tar" -C "$tmp_dir"
  install_script="$(find "$tmp_dir" -maxdepth 2 -name install.sh -print -quit)"
  if [ -n "$install_script" ]; then
    chmod +x "$install_script"
    "$install_script"
  else
    echo "No install.sh found in previous devel package $previous_devel_key" >&2
    exit 1
  fi
  rm -rf "$tmp_dir"
else
  echo "No previous devel package found; continuing without cache."
fi

./Build devel

package_path="$(./Build package | tail -n 1)"

if [ ! -f "$package_path" ]; then
  echo "Package not found at $package_path" >&2
  exit 1
fi

package_key="$(basename "$package_path")"
aws s3 cp "$package_path" "s3://$S3_BUCKET/$package_key"

devel_path="$(./Build package-devel | tail -n 1)"

if [ ! -f "$devel_path" ]; then
  echo "Devel package not found at $devel_path" >&2
  exit 1
fi

devel_key="$(basename "$devel_path")"
aws s3 cp "$devel_path" "s3://$S3_BUCKET/$devel_key"

printf "Uploaded %s to s3://%s/%s\n" "$package_path" "$S3_BUCKET" "$package_key"
