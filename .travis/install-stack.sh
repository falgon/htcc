#!/bin/bash

set -euo pipefail

travis_retry() {
  "$@" || (sleep 2 && "$@") || (sleep 10 && "$@")
}

STACK_VERSION=3.9.3
STACK_RELEASE_URL=https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}

case "$(uname)" in
  Darwin)
    STACK_PLATFORM=osx-x86_64
    STACK_SHA256=e9139c80dc0e2d5df6bef45d40e20c8ac9ca2dbd1bb31358782fa5446d0df38c
    ;;
  Linux)
    STACK_PLATFORM=linux-x86_64
    STACK_SHA256=bc45cf6e1d00910348dcceb510a469056f6d9c63997230f901c22cf997598b05
    ;;
  *)
    echo "Unsupported platform: $(uname)" >&2
    exit 1
    ;;
esac

STACK_ARCHIVE=stack-${STACK_VERSION}-${STACK_PLATFORM}.tar.gz
TMP_DIR=$(mktemp -d)
trap 'rm -rf "${TMP_DIR}"' EXIT

mkdir -p "${HOME}/.local/bin"
travis_retry curl -fsSLo "${TMP_DIR}/${STACK_ARCHIVE}" "${STACK_RELEASE_URL}/${STACK_ARCHIVE}"
printf '%s  %s\n' "${STACK_SHA256}" "${TMP_DIR}/${STACK_ARCHIVE}" | shasum -a 256 -c -
tar xzf "${TMP_DIR}/${STACK_ARCHIVE}" -C "${TMP_DIR}"
install -m 0755 "${TMP_DIR}/stack-${STACK_VERSION}-${STACK_PLATFORM}/stack" "${HOME}/.local/bin/stack"

travis_retry stack --no-terminal setup
