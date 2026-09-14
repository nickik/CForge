# Runnable Forge examples

## Conway's Game of Life

`game_of_life.fg` is an 8x8 toroidal Conway simulation written entirely in the currently executable Forge bootstrap subset. It uses a `u64` bitboard so it exercises loops, mutable bindings, compound assignment, checked integer arithmetic, shifts, bitwise operations, strings, imports, and hosted console output without requiring arrays yet.

Run with Clojure:

```sh
clojure -M:native -- --run examples/game_of_life.fg
```

Build and run the native CForge executable:

```sh
bash bin/build-native.sh
./target/cforge --run examples/game_of_life.fg
```

CI compares both outputs byte-for-byte with `game_of_life.expected.txt`.

The update/render organization was informed by Bruce Hill's MIT-licensed console C implementation (`bruce-hill/conway`), while the Forge example was rewritten around a compact bitboard for the current language subset.
