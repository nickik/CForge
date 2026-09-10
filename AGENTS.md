# CForge Agent Instructions

## Mission

CForge is the independent Clojure reference interpreter for Forge. Correctness, clarity and semantic independence from the Rust compiler are more important than speed.

## Non-negotiable rules

1. Do not add third-party libraries without an explicit project decision.
2. Do not copy parser implementation logic or AST serialization from the Rust frontend. The Forge specification and shared fixtures are the compatibility source.
3. Never make unsupported syntax or semantics succeed approximately. Return a structured unsupported diagnostic instead.
4. Preserve source spans through lexing, parsing and semantic analysis.
5. Use stable diagnostic categories; diagnostic prose is not normative.
6. Model Forge fixed-width integers, conversions, mutation, references and pointers explicitly. Do not inherit host-language behavior accidentally.
7. Keep lexer, parser, resolver/checker and evaluator separate.
8. Ordinary Forge control flow is data, not JVM exceptions. Exceptions are reserved for structured diagnostic unwinding/internal failures.
9. Every new executable language feature requires positive and negative tests.
10. Any bug found by differential testing must gain a minimal regression fixture.

## Parser policy

Use a hand-written lexer, recursive descent for declarations/statements/types, and Pratt parsing for expressions. Prefer small explicit functions over parser metaprogramming.

## Instrumentation

Use `cforge.trace/emit!` with structured event maps. Tracing must be deterministic and disabled by default. Never make semantics depend on whether tracing or pretty printing is enabled.

## Completion rule

Do not describe a feature as supported unless it parses, passes semantic analysis when applicable, executes correctly when applicable, and has tests.

`:unsupported` is useful during bootstrap but does not count as conformance.
