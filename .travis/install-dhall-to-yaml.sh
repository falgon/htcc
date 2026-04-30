#!/bin/bash

set -euo pipefail

mkdir -p "${HOME}/.local/bin"

stack --stack-yaml "${STACK_YAML:-stack.yaml}" install \
  dhall-json-1.7.12 \
  dhall-yaml-1.2.12 \
  --local-bin-path "${HOME}/.local/bin"
