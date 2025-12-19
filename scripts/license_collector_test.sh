#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
source "${SCRIPT_DIR}/license_collector.sh"

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

notice_entries=()
collect_local_license "qrpc" "${REPO_ROOT}/LICENSE" "$tmpdir" notice_entries "Apache-2.0"

if [ ! -f "${tmpdir}/qrpc.LICENSE" ]; then
  echo "FAIL: missing copied local license"
  exit 1
fi

notice_entries_pkg=()
collect_pkg_license "cronolog package" "cronolog" "$tmpdir" notice_entries_pkg

cronolog_copy_count=$(find "$tmpdir" -maxdepth 1 -type f -name 'cronolog-*-*.LICENSE' | wc -l)
if [ "$cronolog_copy_count" -eq 0 ]; then
  echo "FAIL: cronolog licenses not copied"
  exit 1
fi

tokens_output=$(license_expr_tokens "apache-2.0 AND gnu-gpl-v2 OR (mit)")
expected_tokens=$'apache-2.0\ngnu-gpl-v2\nmit'
if [ "$tokens_output" != "$expected_tokens" ]; then
  echo "FAIL: token parsing incorrect"
  exit 1
fi

whitelist_file="${tmpdir}/license_whitelist.txt"
printf 'pkg_tarup\n' > "$whitelist_file"
export PKGSRC_LICENSE_WHITELIST="$whitelist_file"
if ! license_whitelisted "pkg_tarup-1.9.1" "pkg_tarup-1.9.1" "pkg_tarup"; then
  echo "FAIL: whitelist did not match pkg_tarup"
  exit 1
fi

echo "license_collector tests passed"
