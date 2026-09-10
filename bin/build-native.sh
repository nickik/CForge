#!/usr/bin/env bash
set -euo pipefail

if ! command -v clojure >/dev/null 2>&1; then
  echo "error: clojure CLI not found" >&2
  exit 1
fi

if ! command -v native-image >/dev/null 2>&1; then
  echo "error: GraalVM native-image not found" >&2
  exit 1
fi

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

rm -rf target/classes
mkdir -p target/classes target

clojure -M -e '(binding [*compile-path* "target/classes"] (compile (quote cforge.main)))'

CLASSPATH="target/classes:$(clojure -Spath)"

native-image \
  --no-fallback \
  -cp "$CLASSPATH" \
  cforge.main \
  target/cforge

echo "built: $ROOT/target/cforge"
