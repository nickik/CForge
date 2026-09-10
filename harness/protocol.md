# Forge harness adapter protocol v1

The harness communicates with every Forge implementation through this protocol.

## Transport

The adapter is an executable command. The harness invokes it with one operation and optional file path.

```text
ADAPTER describe
ADAPTER parse FILE
ADAPTER check FILE
ADAPTER run FILE
```

The adapter writes exactly one UTF-8 EDN map to stdout and exits `0` when the adapter itself completed successfully.

Anything written by the implementation for diagnostic/debug purposes must either be captured into the response map or written to the adapter's stderr. Adapter stderr is considered infrastructure/debug output, not Forge program stderr.

A non-zero adapter process exit means `:harness/adapter-failure`.

## `describe`

Example:

```clojure
{:protocol 1
 :implementation :cforge
 :implementation-version "bootstrap"
 :capabilities #{:parse :check :run}
 :canonical-ast-version nil}
```

## Common result fields

Every `parse`, `check`, and `run` result must contain:

```clojure
{:protocol 1
 :implementation :cforge
 :operation :check
 :outcome :accepted
 :phase :check
 :diagnostics []}
```

`:outcome` is one of:

```text
:accepted
:rejected
:unsupported
```

`:unsupported` means the source uses normative Forge functionality that this implementation does not implement yet. It never means pass.

`:phase` is one of:

```text
:lex
:parse
:resolve
:check
:run
```

Implementations may add more specific phases later, but these stable coarse phases remain available for comparison.

## Diagnostics

Diagnostics are structured data:

```clojure
{:category :type/mismatch
 :severity :error
 :span {:start 10 :end 15 :line 2 :column 5}
 :message "human-oriented text"}
```

Only `:category` is normative for differential comparison in protocol v1. Message text is retained for people but is not compared.

Spans are compared only when a suite explicitly asks for span comparison.

## Parse result

Minimal valid parse response:

```clojure
{:protocol 1
 :implementation :cforge
 :operation :parse
 :outcome :accepted
 :phase :parse
 :diagnostics []}
```

An implementation may additionally provide:

```clojure
:canonical-ast-version 1
:canonical-ast {...}
:raw-ast {...}
```

`:raw-ast` is never compared across implementations.

`:canonical-ast` is compared only when both implementations advertise the same non-nil `:canonical-ast-version`.

## Check result

```clojure
{:protocol 1
 :implementation :cforge
 :operation :check
 :outcome :rejected
 :phase :check
 :diagnostics [{:category :type/mismatch ...}]}
```

## Run result

```clojure
{:protocol 1
 :implementation :cforge
 :operation :run
 :outcome :accepted
 :phase :run
 :diagnostics []
 :program-exit 0
 :program-stdout "hello\n"
 :program-stderr ""}
```

`program-exit` is the Forge program's result, not the adapter process exit status.

Program stdout and stderr are byte-for-byte significant after UTF-8 decoding unless the suite explicitly selects a future normalization policy.

## Unsupported result

```clojure
{:protocol 1
 :implementation :cforge
 :operation :parse
 :outcome :unsupported
 :phase :parse
 :diagnostics [{:category :parse/unsupported-syntax ...}]}
```

## Harness invariant

An adapter may normalize representation. It must not normalize semantics.

Examples of forbidden normalization include:

- changing a signed value to unsigned because the magnitude matches;
- suppressing an overflow diagnostic;
- converting a rejected program into `:unsupported` merely to avoid a mismatch;
- changing program stdout whitespace;
- reordering diagnostics when source order is significant.
