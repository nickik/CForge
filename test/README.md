# CForge testing strategy

CForge uses two complementary test layers.

## 1. Bootstrap implementation tests

`cforge.core-test` contains focused executable regression tests for behavior that the current interpreter claims to implement.

These tests must stay green. They exercise concrete implementation details such as:

- lexer spans, comments and escapes;
- Pratt precedence;
- module/import parsing;
- fixed-width integer boundaries;
- checked arithmetic and traps;
- logical short circuiting;
- strict type matching;
- control-flow return analysis;
- entry-point rules;
- explicit rejection of unsupported features.

If a bug is found in an implemented feature, first add the smallest regression test that demonstrates it.

## 2. Forge v1 feature corpus

`fixtures/forge_v1/coverage.edn` is the long-term language coverage inventory. It intentionally contains valid and invalid Forge programs for features that CForge does not implement yet.

Every case records:

```clojure
{:id :arith/overflow
 :feature :arithmetic/checked-overflow
 :phase :run
 :expect :reject
 :category :runtime/overflow
 :bootstrap true
 :source "..."}
```

or, for not-yet-implemented language behavior:

```clojure
{:id :flow/match
 :feature :flow/match
 :phase :parse
 :expect :accept
 :status :future
 :source "..."}
```

`:future` does **not** mean optional. It means normative Forge v1 coverage that the current CForge implementation has not reached yet.

When CForge implements a feature:

1. make the implementation correct;
2. remove `:status :future` from the relevant cases;
3. add `:bootstrap true`;
4. make those cases pass in CI;
5. add smaller focused unit tests for important boundary behavior.

Never change a normative fixture merely to match an implementation bug. If the Forge specification itself changes, update the specification and fixture together with an explicit language decision.

## Coverage families

The feature corpus is expected to cover at least:

- source/lexical structure and literals;
- modules/imports/visibility;
- `val`, `var`, `const`, inference and definite initialization;
- fundamental types, structs, enums, tagged unions, distinct/alias/range/bitstruct types;
- arrays, slices, references, pointers, Option and Result;
- strict conversions;
- arithmetic, overflow, division, shifts, bitwise/logical operators and evaluation order;
- `if`, loops, switch, match and pattern forms;
- functions, `nfn`, overloads, function pointers, closures, propagation and tail calls;
- `defer`, panic and safety/unsafe behavior;
- allocation/context conventions;
- FDN reader forms and metadata;
- methods/`impl`;
- concurrency/select;
- C ABI and representation metadata;
- features deliberately absent from Forge v1.

The corpus is an executable implementation checklist and will later be shared with the production Forge compiler through the differential harness.
