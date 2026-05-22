#!/bin/bash

set -euo pipefail

dhall_release_version="1.42.2"
dhall_json_version="1.7.12"

mkdir -p "${HOME}/.local/bin"

case "$(uname -s):$(uname -m)" in
  Linux:x86_64 | Linux:amd64)
    platform="x86_64-linux"
    archive_sha256="acbada5e29ecc9b6a723c3f390beb76b9db26df81546d1f472415a2f387bc457"
    ;;
  Darwin:x86_64)
    platform="x86_64-darwin"
    archive_sha256="f6b0bc2f120e5ade2c4c789555237cb4a0b4611fb2455f2a16a3bde4a441e589"
    ;;
  Darwin:arm64 | Darwin:aarch64)
    platform="aarch64-darwin"
    archive_sha256="761048afa225dc9978b9fb742cc9d4feee104f2656aefe37b6a6f157862b77dd"
    ;;
  *)
    echo "unsupported platform for dhall-to-yaml: $(uname -s):$(uname -m)" >&2
    exit 1
    ;;
esac

archive="dhall-json-${dhall_json_version}-${platform}.tar.bz2"
url="https://github.com/dhall-lang/dhall-haskell/releases/download/${dhall_release_version}/${archive}"
tmpdir="$(mktemp -d)"
trap 'rm -rf "${tmpdir}"' EXIT

curl --fail --location --silent --show-error "${url}" --output "${tmpdir}/${archive}"
if command -v sha256sum >/dev/null 2>&1; then
  actual_sha256="$(sha256sum "${tmpdir}/${archive}" | awk '{ print $1 }')"
else
  actual_sha256="$(shasum -a 256 "${tmpdir}/${archive}" | awk '{ print $1 }')"
fi
if [[ "${actual_sha256}" != "${archive_sha256}" ]]; then
  echo "checksum mismatch for ${archive}" >&2
  echo "expected: ${archive_sha256}" >&2
  echo "actual:   ${actual_sha256}" >&2
  exit 1
fi

expected_entries="$(printf '%s\n' \
  "bin/dhall-to-json" \
  "bin/dhall-to-yaml" \
  "bin/json-to-dhall")"
actual_entries="$(tar -tjf "${tmpdir}/${archive}")"
if [[ "${actual_entries}" != "${expected_entries}" ]]; then
  echo "unexpected entries in ${archive}" >&2
  echo "${actual_entries}" >&2
  exit 1
fi

if ! tar -tvjf "${tmpdir}/${archive}" | awk '
  BEGIN {
    ok["bin/dhall-to-json"] = 1
    ok["bin/dhall-to-yaml"] = 1
    ok["bin/json-to-dhall"] = 1
  }
  substr($1, 1, 1) != "-" || !($NF in ok) { exit 1 }
'; then
  echo "unsafe entries in ${archive}" >&2
  exit 1
fi

tar -xjf "${tmpdir}/${archive}" -C "${tmpdir}" bin/dhall-to-yaml
install -m 0755 "${tmpdir}/bin/dhall-to-yaml" "${HOME}/.local/bin/dhall-to-yaml"
"${HOME}/.local/bin/dhall-to-yaml" --version
