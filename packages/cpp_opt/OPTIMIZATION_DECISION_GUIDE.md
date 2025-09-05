# CodeUChain C++ Optimization Decision Guide

This guide helps decide **when** to apply advanced performance optimizations (StaticChain, mutability, slot caching, hybrid context) versus keeping standard dynamic chain usage.

> Core Principle: Optimize only when the structural overhead is material to your latency or throughput goals.

---
## 1. Quick Triage Flowchart (Textual)
```
Is this code path called > 1e6 times/sec per process?
  └─ No → Use standard dynamic Chain (STOP)
  └─ Yes → Are per-link operations trivial (< 2 µs useful work)?
        └─ No → Dynamic Chain acceptable (STOP)
        └─ Yes → Is introspection / dynamic reconfiguration required?
                └─ Yes → Consider StaticChain + instrumentation
                └─ No  → Can you fuse logic into a direct function?
                        └─ Yes → Write direct fused function (hot path)
                        └─ No  → Apply HybridContext + SlotHandle optimizations
```

---
## 2. Optimization Ladder
| Tier | Mode | When to Use | Overhead Profile | Trade-offs |
|------|------|-------------|------------------|------------|
| 0 | Direct Function Pipeline | Ultra-hot arithmetic / kernels | ~baseline | No chaining features |
| 1 | Dynamic Chain (Immutable) | Default orchestration | High micro overhead | Max flexibility, introspection |
| 2 | StaticChain (Immutable) | Known compile-time sequence | Removes virtual dispatch | Still context/lookup cost |
| 3 | StaticChain + Mutating Ops | Hot path, still structured | Cuts copy churn | Mutability risks debug clarity |
| 4 | StaticChain + Slot Caching | Hot key repeated access | Eliminates repeated lookups | Manual caching logic |
| 5 | StaticChain + HybridContext + SlotHandle (future) | Performance critical, memory churn sensitive | Near-direct cost target | Additional complexity, API expansion |

---
## 3. Cost Heuristic
Let:
- `O` = chain structural overhead per evaluation (µs)
- `W` = average useful work per link (µs)
- `L` = number of links

Rule-of-thumb thresholds:
- If `O < 0.15 * (W * L)` → Overhead acceptable.
- If `O ≈ (W * L)` → Consider Tier 2–4.
- If `O >> (W * L)` (micro pipelines) → Jump directly to Tier 4–5 or direct function.

---
## 4. Empirical Anchor (Representative Numbers)
| Variant | ns/op | Relative to Direct | Notes |
|---------|-------|--------------------|-------|
| direct | ~0.4 | 1.0× | Arithmetic only |
| static (immutable) | ~1300 | ~3000× | Copies + lookups dominate |
| static_mut | ~830 | ~2000× | Removes immutable copies |
| mutable (manual) | ~175 | ~430× | No chain dispatch, but lookups remain |
| hot_slot_imm | ~420 | ~1000× | Single final immutable write |
| hot_slot_mut | ~145 | ~345× | Cached value + single final store |

Interpretation: Majority cost = repeated map/variant interactions, not dispatch.

---
## 5. Decision Matrix
| Concern | Recommendation |
|---------|---------------|
| Need runtime link replacement | Stay dynamic (Tier 1) |
| Fixed linear pipeline, moderate invocations | StaticChain (Tier 2) |
| Fixed linear, high-frequency micro ops | StaticChain + mut ops (Tier 3) |
| Same key updated multiple times | Slot caching (Tier 4) |
| Many small contexts, churn heavy | HybridContext (Tier 5) |
| Extreme latency target (< 1 µs total) | Fused direct function (Tier 0) |

---
## 6. Migration Path
1. Start dynamic chain for clarity.
2. Profile: capture per-chain time and #executions.
3. If hot: switch to `StaticChain` version (mechanical transform).
4. Replace repeated immutable inserts with mut ops where safe.
5. Introduce slot caching for repeated key mutation.
6. Adopt `HybridContext` / key interning (when available) for further gains.
7. If still not sufficient: fuse to a hand-written function.

---
## 7. Instrumentation Suggestions (Planned)
Metrics to expose:
- `context_lookups`
- `context_mutations`
- `allocations`
- `variant_constructs`
- `hash_ops`
- `virtual_dispatches`

Advisor heuristic example output:
```
Chain Performance Advisor:
  82% time in context lookups
  11% time in variant construction
  Suggested actions: enable SlotHandle, enable HybridContext.
```

---
## 8. When NOT to Optimize
Avoid spending engineering time if:
- Chain executes < 100k times/sec.
- Per-link work includes IO, RPC, DB calls, or heavy compute (serialization, crypto, model inference).
- Performance SLA already met with margin.

---
## 9. Risks & Mitigations
| Risk | Mitigation |
|------|------------|
| Over-complex optimization layering | Keep additive, not replacing core APIs |
| Debug difficulty with mutability | Restrict mut ops to well-documented sections |
| Premature micro-focus | Add profiling gate before allowing Tier 3+ usage |

---
## 10. FAQ
**Q: Why so large a gap vs direct?**  
A: Hash map + variant + copies dominate when useful work is trivial; abstraction overhead becomes the work.

**Q: Will HybridContext really help?**  
A: Yes—cuts alloc/copy; combined with slot caching it removes main remaining structural costs.

**Q: Can I mix tiers?**  
A: Yes; reserve Tier 0–2 for most code, escalate only for the hotspots.

---
## 11. TL;DR
Default to dynamic chains for clarity and composability. Promote to StaticChain and storage-level optimizations only for genuinely hot, trivial workloads. Always measure first.

---
_Linked References_: See `../cpp/CHAIN_PERFORMANCE_OPTIMIZATION.md` (Sections 12–14) for deeper empirical data.
