#!/usr/bin/env bash

# Utility helpers for gathering license texts for pkgsrc-built artifacts.
# Keeps logic out of Build and makes it unit-testable.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

: "${PKG_INFO:=/opt/qrpc/pkg/sbin/pkg_info}"
: "${PKGSRC_LICENSE_DIR:=${REPO_ROOT}/_pkgsrc/licenses}"
: "${PKGSRC_LICENSE_WHITELIST:=${REPO_ROOT}/config/license_whitelist.txt}"

warn() {
  printf 'license_collector warning: %s\n' "$*" >&2
}

license_expr_tokens() {
  tr '()[],' '     ' <<<"$1" \
    | tr '[:upper:]' '[:lower:]' \
    | awk '
        {
          for (i = 1; i <= NF; ++i) {
            if ($i == "and" || $i == "or") {
              continue
            }
            print $i
          }
        }
      '
}

copy_license_token() {
  local token="$1"
  local dest_dir="$2"
  local filename_prefix="$3"
  local src="${PKGSRC_LICENSE_DIR}/${token}"

  if [ ! -f "$src" ]; then
    warn "missing license text ${token} for ${filename_prefix}"
    return 1
  fi

  mkdir -p "$dest_dir"
  cp "$src" "${dest_dir}/${filename_prefix}-${token}.LICENSE"
}

license_whitelisted() {
  local pkg_ref="$1"
  local pkgname="$2"
  local pkgbase="$3"
  local whitelist="${PKGSRC_LICENSE_WHITELIST}"

  [ -f "$whitelist" ] || return 1
  while IFS= read -r line; do
    line="${line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [ -n "$line" ] || continue
    if [ "$line" = "$pkg_ref" ] || [ "$line" = "$pkgname" ] || { [ -n "$pkgbase" ] && [ "$line" = "$pkgbase" ]; }; then
      return 0
    fi
  done < "$whitelist"
  return 1
}

collect_pkg_license() {
  local label="$1"
  local pkg_ref="$2"
  local dest_dir="$3"
  local notice_array_name="$4"

  local spec pkgname pkgbase
  if [ ! -x "$PKG_INFO" ]; then
    warn "pkg_info tool not executable at ${PKG_INFO}; skipping ${pkg_ref}"
    eval "${notice_array_name}+=(\"${label}: UNKNOWN\")"
    return 1
  fi
  spec="$("$PKG_INFO" -Q LICENSE "$pkg_ref" 2>/dev/null || true)"
  pkgname="$("$PKG_INFO" -Q PKGNAME "$pkg_ref" 2>/dev/null || true)"
  pkgbase="$("$PKG_INFO" -Q PKGBASE "$pkg_ref" 2>/dev/null || true)"
  if [ -z "$pkgname" ]; then
    pkgname="$pkg_ref"
  fi
  if [ -z "$pkgbase" ]; then
    pkgbase="${pkgname%-*}"
  fi

  if [ -z "$spec" ]; then
    if license_whitelisted "$pkg_ref" "$pkgname" "$pkgbase"; then
      eval "${notice_array_name}+=(\"${label}: NO-LICENSE (whitelisted)\")"
      return 0
    fi
    warn "unable to determine LICENSE for ${pkg_ref}"
    eval "${notice_array_name}+=(\"${label}: UNKNOWN\")"
    return 1
  fi

  while IFS= read -r token; do
    [ -n "$token" ] || continue
    copy_license_token "$token" "$dest_dir" "$pkgname"
  done < <(license_expr_tokens "$spec")

  eval "${notice_array_name}+=(\"${label}: ${spec}\")"
}

collect_local_license() {
  local label="$1"
  local src_path="$2"
  local dest_dir="$3"
  local notice_array_name="$4"
  local spec="$5"

  if [ ! -f "$src_path" ]; then
    warn "local license file ${src_path} missing for ${label}"
    eval "${notice_array_name}+=(\"${label}: UNKNOWN\")"
    return 1
  fi

  mkdir -p "$dest_dir"
  cp "$src_path" "${dest_dir}/${label// /_}.LICENSE"
  eval "${notice_array_name}+=(\"${label}: ${spec}\")"
}

write_notice() {
  local notice_path="$1"
  shift
  mkdir -p "$(dirname "$notice_path")"
  {
    echo "qrpc packaged release contents"
    echo
    echo "Bundled components and their license identifiers:"
    for entry in "$@"; do
      printf ' - %s\n' "$entry"
    done
    echo
    echo "Full license texts reside in ./licenses/."
    echo "GPLed components come from pkgsrc; source is available at"
    echo "https://cdn.netbsd.org/pub/pkgsrc/distfiles/."
  } > "$notice_path"
}

pkg_for_filename() {
  local filename="$1"
  local match
  match=$(find /opt/qrpc/pkg -type f -name "$filename" -print -quit 2>/dev/null || true)
  if [ -z "$match" ]; then
    return
  fi
  if [ ! -x "$PKG_INFO" ]; then
    return
  fi
  "$PKG_INFO" -Fe "$match" 2>/dev/null | head -n1
}

gather_release_licenses() {
  local staging_arg="$1"
  local release_tar="$2"
  local staging_dir
  staging_dir="$(cd "$staging_arg" && pwd)"
  local licenses_dir="${staging_dir}/licenses"
  local notice_path="${staging_dir}/NOTICE"
  local -a notice_entries=()

  if [ -f "${REPO_ROOT}/LICENSE" ]; then
    collect_local_license "qrpc (${release_tar})" "${REPO_ROOT}/LICENSE" "$licenses_dir" notice_entries "Apache-2.0"
  else
    warn "qrpc LICENSE file missing at ${REPO_ROOT}/LICENSE"
  fi

  shopt -s nullglob
  for archive in "${staging_dir}"/*.tgz; do
    local base pkgname
    base="$(basename "$archive")"
    if [ "$base" = "$release_tar" ]; then
      continue
    fi
    pkgname="${base%.tgz}"
    collect_pkg_license "$base" "$pkgname" "$licenses_dir" notice_entries
  done
  shopt -u nullglob

  for extra in "${staging_dir}"/*; do
    [ -f "$extra" ] || continue
    case "$extra" in
      *.tgz|*.sh|*.service|*.pkgrc|"$notice_path")
        continue
        ;;
    esac
    if [ "$extra" = "$licenses_dir" ]; then
      continue
    fi
    local base pkg_owner
    base="$(basename "$extra")"
    pkg_owner="$(pkg_for_filename "$base")"
    if [ -n "$pkg_owner" ]; then
      collect_pkg_license "${base} (${pkg_owner})" "$pkg_owner" "$licenses_dir" notice_entries
    else
      warn "unable to determine license owner for ${base}"
    fi
  done

  write_notice "$notice_path" "${notice_entries[@]}"
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  cmd="${1:-}"
  case "$cmd" in
    gather)
      staging="${2:?staging directory required}"
      release_name="${3:?release tar name required}"
      gather_release_licenses "$staging" "$release_name"
      ;;
    *)
      echo "usage: $0 gather <staging_dir> <release_tar_name>" >&2
      exit 1
      ;;
  esac
fi
