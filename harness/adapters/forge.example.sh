#!/usr/bin/env bash
set -euo pipefail

# Template for the production Forge compiler adapter.
#
# Do not make the harness parse human-readable compiler output. Instead, wire
# this script to stable machine-readable commands in the production compiler
# and emit exactly one EDN map conforming to harness/protocol.md.
#
# Suggested eventual compiler capabilities:
#   forge --machine parse FILE
#   forge --machine check FILE
#   forge --machine run FILE
#
# The exact compiler CLI is intentionally not frozen here.

op="${1:-}"
file="${2:-}"

case "$op" in
  describe)
    printf '%s\n' '{:protocol 1 :implementation :forge :implementation-version "unconfigured" :capabilities #{} :canonical-ast-version nil}'
    ;;
  parse|check|run)
    printf '%s\n' "{:protocol 1 :implementation :forge :operation :$op :outcome :unsupported :phase :$op :diagnostics [{:category :harness/forge-adapter-not-configured :severity :error :message \"production Forge adapter not configured\"}]}"
    ;;
  *)
    printf '%s\n' 'usage: forge.example.sh describe | parse FILE | check FILE | run FILE' >&2
    exit 2
    ;;
esac
