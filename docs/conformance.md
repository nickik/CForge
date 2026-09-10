# Forge Conformance Contract

## Purpose

CForge and the production Forge compiler must be tested against the same source-level conformance corpus. The corpus is an executable specification, not an implementation snapshot.

The canonical suite currently lives in `nickik/forge/examples/conformance` and uses `suite.fdn` as its manifest.

## Test kinds

### `:parse`

The source is syntactically valid Forge.

A conforming implementation must:

- accept the entire source file;
- produce no error-severity parser diagnostics;
- consume all non-trivia input;
- preserve source spans sufficiently to identify every declaration, statement, expression and type node.

AST serialization is **not** a compatibility surface. Two implementations may use different AST shapes.

### `:negative`

The source intentionally violates a Forge semantic rule.

A conforming implementation must:

- parse far enough to reach the intended semantic check;
- reject the program before execution/code generation;
- report the stable diagnostic category declared by `:expect` in the suite manifest.

Exact wording, punctuation and rendering of diagnostics are not normative.

Examples of stable categories include:

- `:lex/invalid-token`
- `:parse/unexpected-token`
- `:name/unresolved`
- `:name/duplicate`
- `:type/mismatch`
- `:type/distinct`
- `:type/return`
- `:type/overflow`
- `:flow/invalid-return`
- `:runtime/bounds`

New categories require a suite/specification decision; implementations must not invent aliases for an already-defined category.

### `:run`

The source is valid Forge and has observable behavior.

A conforming implementation must:

1. parse successfully;
2. pass semantic analysis;
3. execute `main` according to Forge semantics;
4. match the manifest's expected exit code;
5. when specified later, match stdout/stderr byte-for-byte after only the normalization explicitly stated by the test.

A crash, host exception, assertion failure, timeout, unsupported-feature fallback, or implementation-defined coercion is never a passing result.

## Result protocol

Every CForge test execution should reduce to one machine-readable result map:

```clojure
{:status :pass | :fail | :unsupported
 :kind :parse | :negative | :run
 :path "..."
 :phase :lex | :parse | :resolve | :check | :eval
 :diagnostics [{:category :type/mismatch
                :severity :error
                :span {:start 10 :end 15 :line 1 :column 11}
                :message "..."}]
 :exit 0}
```

`:unsupported` is useful during bootstrap but **never counts as conforming**. It exists so missing language coverage cannot be confused with an implementation bug or a passing test.

## Semantic comparison rules

The following are normative across implementations:

- accept/reject result;
- stable diagnostic category for negative tests;
- exact Forge type of every semantically checked expression;
- observable run behavior;
- overflow, bounds, conversion and mutation behavior;
- evaluation order where Forge specifies it;
- `defer` ordering and scope-exit behavior;
- pattern-match choice and exhaustiveness behavior;
- pointer/reference behavior defined by the Forge memory model.

The following are not normative:

- AST/HIR node class names;
- object identity;
- hash-map iteration order unless Forge explicitly exposes ordering;
- diagnostic prose;
- internal allocation addresses;
- implementation trace formatting.

## Differential testing

Where both implementations support a feature, CI should run the same fixture through CForge and the Rust implementation.

For `:parse`, compare acceptance and normalized structural facts rather than serialized AST bytes.

For `:negative`, compare rejection and diagnostic category.

For `:run`, compare exit code and observable output.

A disagreement is treated as a language/implementation issue requiring resolution against the Forge specification and an explicit regression fixture.

## Correctness policy

CForge is the reference semantic implementation, but the written Forge specification and conformance fixtures remain authoritative. CForge behavior does not become language law merely because it exists.

When semantics are unclear, CForge must return `:unsupported` or a deliberate diagnostic rather than guess.

## Bootstrap order

1. Lexer correctness and spans.
2. Parser coverage of existing `:parse` and `:run` fixtures.
3. Declaration collection and name resolution.
4. Exact primitive type checking.
5. Integer/boolean evaluation and control flow.
6. Functions and recursion.
7. Arrays/structs/references.
8. Stable negative diagnostic categories.
9. `defer`, tagged values, options/results, patterns.
10. Full suite differential runner.
