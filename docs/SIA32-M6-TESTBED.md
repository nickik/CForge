# SIA32 M6 hosted conformance testbed

CForge is the hosted execution bed for Cosmic's SIA32 bring-up. It is not a second kernel and it does not implement SIA page-table policy. Cosmic's Forge source constructs page tables and performs AddressSpace transactions; CForge only supplies the hosted forms of the actual SIA architectural effects plus bootstrap storage used by the selected CForge platform provider.

## Deterministic SIA run state

`--run` accepts three M6 harness options:

```text
--sia-status DECIMAL
--sia-vmctx DECIMAL
--sia-observation PATH
```

Before the Forge program starts, CForge initializes STATUS and VMCTX to the supplied values (default zero). Initialization is harness setup and is not recorded as a SIA instruction.

After execution, `--sia-observation` writes the machine observation even if Forge returned a non-zero program exit code. Parser/check diagnostics still cause the normal CForge failure result.

All architectural operands are range checked as SIA32 values even though hosted CForge uses a wider `usize`.

## Observation format

The file is deliberately a tiny line protocol rather than a Clojure-specific data format. A Rust/C++ LightingSimulation harness or a native test runner can emit and compare it without embedding Clojure.

Version 1 begins with:

```text
sia32-machine-observation-v1
status<TAB>DECIMAL
vmctx<TAB>DECIMAL
```

It is followed by zero or more ordered events:

```text
event<TAB>SEQ<TAB>status_write<TAB>REQUESTED<TAB>STORED
event<TAB>SEQ<TAB>vmctx_write<TAB>VALUE
event<TAB>SEQ<TAB>tlb_fence_all
event<TAB>SEQ<TAB>tlb_fence_va<TAB>VA<TAB>CURRENT_ASID
event<TAB>SEQ<TAB>tlb_fence_asid<TAB>ASID
```

`TLBFENCE.VA` records the ASID from VMCTX at the instant the fence executes. This makes the architecturally implicit ASID dependency explicit in the external test observation without changing the Forge ABI.

STATUS stores both the requested value and the architectural value after reserved bits are masked.

Reads are not logged. Their correctness is tested by the Forge program itself through `status_read()` / `vmctx_read()`, while the observation stream is restricted to state-changing or invalidation effects.

## Comparison model

M6 uses two outputs:

1. **Forge program stdout / exit status** — page-table encodings, translations, capability/AddressSpace behavior and other portable semantics.
2. **SIA machine observation** — STATUS/VMCTX writes and TLB invalidation ordering.

A later native Forge run should execute the same `.fg` conformance program and produce the same portable output. A LightingSimulation wrapper should observe its real SREAD/SWRITE/TLBFENCE execution and emit the same observation protocol.

Neither comparison channel allows CForge to own PTE construction, table walking, allocation/reclamation, mapping rollback, or capability policy.
