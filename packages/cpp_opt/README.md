# CodeUChain C++ Optimization Prototype (`cpp_opt`)

This experimental package demonstrates compile-time chain composition (StaticChain) as outlined in `../cpp/CHAIN_PERFORMANCE_OPTIMIZATION.md`.

## Goals
- Show conceptual and empirical difference between:
  1. Direct inline function pipeline
  2. `StaticChain` (compile-time tuple of operations)
  3. Dynamic `Chain` with virtual dispatch + coroutine awaitable

## Key Artifact
- `include/codeuchain_opt/static_chain.hpp`: Header-only `StaticChain<Ops...>` template and example stateless ops.

## Build (from repository root)
```bash
cd packages/cpp/build   # assuming core library already configured
cmake --build . --target static_chain_demo -j
./packages/cpp_opt/static_chain_demo
```

If not yet configured, do:
```bash
cd packages/cpp
mkdir -p build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ../..  # top-level if aggregated, else adjust
cmake --build . --target static_chain_demo -j
```

## Interpreting Output
```
StaticChain Demo (ns/op)
 direct    : <baseline arithmetic>
 static    : <near-baseline if context overhead dominates>
 dynamic   : <higher due to virtual + coroutine + map copies>
```

`static` should be closer to `direct` than `dynamic`. Remaining gap is dominated by context mutation + variant access.

## Optimization Decision Guide

For guidance on **when** to apply advanced optimizations (StaticChain vs dynamic chain, mutability, slot caching, hybrid context) versus leaving code in the default dynamic form, see:

`OPTIMIZATION_DECISION_GUIDE.md`

Highlights:
- Do NOT optimize unless profiling shows micro-scale hot spots.
- Prefer dynamic chains for clarity and observability.
- Escalate to StaticChain + mutability + slot caching only in high-frequency trivial workloads.
- HybridContext + interning (planned) targets memory + lookup churn for further reductions.

This README remains focused on the prototype mechanics; the decision guide captures strategy.

## Next Steps (Planned)
- Hybrid context prototype (`HybridContext`) with inline storage
- Key interning & slot caching toggles
- JSON metrics export integration with main benchmark harness

## Disclaimer
Prototype code intended for design exploration; APIs may change or be removed.
