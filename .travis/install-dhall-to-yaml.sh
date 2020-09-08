#!/bin/bash

set -eux

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

DALL_BIN_URL=https://github.com/dhall-lang/dhall-haskell/releases/download
TAG_VERSION=1.34.0
DHALL_TO_JSON_VERSION=1.7.1

fetch_dhall-to-yaml_osx() {
  curl -sL $DALL_BIN_URL/$TAG_VERSION/dhall-json-$DHALL_TO_JSON_VERSION-x86_64-macos.tar.bz2 |\
      tar xjv -C ~/.local/bin --strip-components=1
}

fetch_dhall-to-yaml_linux() {
  curl -sL $DALL_BIN_URL/$TAG_VERSION/dhall-json-$DHALL_TO_JSON_VERSION-x86_64-linux.tar.bz2 |\
      tar xjv -C ~/.local/bin --strip-components=2
}

mkdir -p ~/.local/bin;
if [ "$(uname)" = "Darwin" ]; then
  travis_retry fetch_dhall-to-yaml_osx
else
  travis_retry fetch_dhall-to-yaml_linux
fi

