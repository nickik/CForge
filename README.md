# CForge

CForge is an independent, correctness-first reference implementation of the Forge language in Clojure.

It is intentionally an interpreter rather than an optimizing compiler. Its primary jobs are:

1. provide a small, auditable implementation of Forge semantics;
2. run the shared Forge conformance suite;
3. act as a differential oracle for the Rust Forge compiler;
4. produce useful diagnostics and optional traces of lexing, parsing, checking, and evaluation;
5. build with GraalVM `native-image` for fast CLI startup.

## Design rules

- Correctness over speed.
- No silent recovery that changes program meaning.
- No implicit use of JVM/Clojure numeric semantics as Forge semantics.
- Parser, semantic analysis, and evaluation are separate phases.
- Every diagnostic carries a stable category and source span where possible.
- The parser is hand written: lexer + recursive descent + Pratt expressions.
- CForge must not consume the Rust parser's AST; independence is valuable for differential testing.
- No third-party libraries without an explicit project decision. The bootstrap uses Clojure core only.

## Current bootstrap slice

The initial implementation targets the first shared executable conformance fixture: module/function declarations, local typed values, integer and boolean expressions, `if`/`else`, and `return`. Unsupported Forge syntax is rejected explicitly.

## Run

```sh
clojure -M -m cforge.main --tokens path/to/file.fg
clojure -M -m cforge.main --ast path/to/file.fg
clojure -M -m cforge.main --check path/to/file.fg
clojure -M -m cforge.main --run path/to/file.fg
clojure -M -m cforge.main --run --trace path/to/file.fg
```

`--pprint` pretty-prints structured output. `--trace` emits deterministic phase/evaluation events to stderr.

See `docs/conformance.md` for the compatibility contract and `docs/architecture.md` for implementation rules.
