# CForge Architecture

## Pipeline

```text
Forge source
  -> lexer
  -> spanned tokens
  -> hand-written recursive-descent + Pratt parser
  -> source-faithful AST
  -> declaration collection
  -> name resolution
  -> type checking / semantic normalization
  -> typed semantic tree
  -> reference evaluator
```

Each phase has one responsibility. No phase may silently compensate for an error in an earlier phase.

## Parser

The lexer and parser are intentionally independent from the Rust Forge frontend. This makes parser disagreements detectable by differential tests.

The parser uses recursive descent for files, declarations, statements and types, and Pratt parsing for expressions. Every AST node carries a byte span; lexer tokens also carry line and column information.

Parser helpers fail with `ex-info` carrying a structured diagnostic map. Host stack traces are development information, not Forge diagnostics.

## Semantics

CForge must model Forge semantics explicitly instead of inheriting Clojure/JVM behavior.

Examples:

- fixed-width Forge integers must be range checked explicitly;
- signedness and conversion rules must be explicit;
- Forge mutation should be represented by an interpreter store, not arbitrary Clojure atoms embedded in values;
- references/pointers will use an abstract Forge memory model;
- ordinary Forge control flow is represented as data, not JVM exceptions.

## Evaluation state

The evaluator evolves an explicit state map. The initial subset uses environments directly; mutable cells and abstract memory are added before Forge `var`/references become executable.

Control flow is represented by values such as:

```clojure
{:flow :normal}
{:flow :return :value value}
{:flow :break}
{:flow :continue}
```

## Instrumentation

Instrumentation must never alter language behavior.

A trace sink receives deterministic event maps:

```clojure
{:event :phase/start :phase :parse}
{:event :parse/node :kind :function :span {...}}
{:event :eval/expr :kind :binary :span {...}}
{:event :eval/return :value {...}}
{:event :phase/end :phase :eval}
```

Tracing is disabled by default. CLI `--trace` writes events to stderr. Tests can bind a collector function and assert events without parsing formatted text.

## Pretty printing

Human-readable pretty printing uses `clojure.pprint`, which ships with Clojure. Pretty printing is presentation only; no parser or evaluator logic depends on printed forms.

## Dependency policy

Bootstrap CForge uses only Clojure itself and JDK APIs. Any additional parsing, CLI, testing or data-format dependency requires an explicit project decision first.
