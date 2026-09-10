# Forge differential conformance harness

This directory contains the implementation-neutral harness used to compare independent Forge implementations.

The harness is deliberately separate from the CForge parser, type checker, and evaluator. A disagreement between implementations must be observable without either implementation importing code from the other.

## Goals

The harness must eventually answer these questions for every conformance fixture:

1. Do both implementations accept or reject the source at the same phase?
2. For rejected programs, do they report the same stable Forge diagnostic category?
3. For executable programs, do they produce the same program exit value, stdout, and stderr?
4. For parser fixtures, do their normalized ASTs describe the same Forge syntax?
5. Where results differ, can the complete raw outputs be retained for diagnosis?

Correctness is the only priority. The harness may run implementations serially and may retain verbose intermediate data.

## Directory layout

```text
harness/
  README.md
  protocol.md
  deps.edn
  config.edn
  src/forge_harness/main.clj
  src/forge_harness/process.clj
  src/forge_harness/compare.clj
  adapters/
    cforge.clj
    forge.example.sh
  fixtures/
    smoke/
      suite.edn
      arithmetic.fg
```

The canonical Forge language fixtures may continue to live in the `forge` repository. The `fixtures/` directory here is only for harness self-tests and adapter tests.

## Adapter boundary

Every implementation is exposed to the harness through an adapter executable.

An adapter is invoked as:

```text
ADAPTER describe
ADAPTER parse FILE
ADAPTER check FILE
ADAPTER run FILE
```

It writes exactly one EDN value to stdout. See `protocol.md`.

The harness does not parse human-oriented compiler messages and does not infer semantic categories from wording. The implementation adapter must normalize implementation-specific output into stable Forge categories.

## Comparison levels

The harness compares progressively stronger contracts:

- `:acceptance` — accepted/rejected/unsupported and failing phase.
- `:diagnostics` — stable Forge diagnostic categories.
- `:execution` — Forge program exit value and exact program stdout/stderr.
- `:ast` — canonical normalized AST, when both implementations expose one.

A suite test can choose a comparison level. During bootstrap, `:acceptance` and `:execution` are enough to make useful comparisons without prematurely freezing an AST serialization format.

## Important distinction: harness exit vs Forge program exit

An adapter process always exits `0` when it successfully reports a result, even if the Forge program itself returns `17`.

The Forge program result belongs inside the EDN response as `:program-exit 17`.

Adapter process failures are harness/infrastructure failures, not Forge language results.

## Current state

`adapters/cforge.clj` provides the CForge side directly through the CForge namespaces.

`adapters/forge.example.sh` documents the boundary for the production compiler. It intentionally does not guess the final production compiler command-line interface. Once that interface has stable machine-readable parse/check/run commands, the adapter can normalize them without changing the harness.

## Running

From this directory:

```bash
clojure -M -m forge-harness.main --config config.edn fixtures/smoke/suite.edn
```

The default configuration enables only CForge. Once a Forge compiler adapter is configured, enabling both implementations causes pairwise differential comparison.

## Correctness rules

- Never treat `:unsupported` as a pass.
- Never compare diagnostics by English message text.
- Never discard raw adapter output when implementations disagree.
- Never silently normalize numeric widths, signedness, paths, or source spans.
- AST comparison must use a separately versioned canonical AST schema.
- A differential disagreement creates a regression fixture before either implementation is changed.
