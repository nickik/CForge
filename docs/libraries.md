# CForge library/bootstrap target

CForge follows the Forge v1 library model rather than defining its own library system.

Bootstrap roots:

```text
lib/core.fg
lib/std.fg
```

Planned first multi-unit graph:

```text
main.fg
  -> std
       -> core
```

CForge should implement this in stages:

1. recognize `pub`/module visibility;
2. map logical module names to root source files;
3. load and parse each source file once;
4. build the import graph;
5. detect unresolved imports and unsupported cycles;
6. collect public declarations as module interfaces;
7. resolve names across imported interfaces;
8. check modules independently;
9. evaluate/link the requested root program.

The reference interpreter must never resolve imports by textual source inclusion. Imports are semantic dependencies.

Default bootstrap bindings should eventually be equivalent to:

```text
core -> lib/core.fg
std  -> lib/std.fg
```

`--no-std` disables the implicit `std` root but leaves `core` available.

The allocator interfaces described by Forge's `docs/core-library.md` belong to the common freestanding library and should eventually be executable in CForge as ordinary Forge code where possible. Environment-specific backing providers are outside `core`.