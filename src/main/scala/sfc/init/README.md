# Initialization

The init package contains factory objects that build the initial simulation
state from a seed and configuration. All factories are stateless objects with
pure `create`/`initialize` methods — no mutable fields.

## Files

| File | Object | Role |
|------|--------|------|
| `WorldInit.scala` | `WorldInit` | Orchestrator — calls all sub-factories, assembles `InitResult` |
| `FirmInit.scala` | `FirmInit` | Creates firm array, assigns network topology, applies sector enhancements |
| `BankInit.scala` | `BankInit` | Creates 7-bank sector, distributes deposits/loans/bonds across banks |
| `ImmigrantInit.scala` | `ImmigrantInit` | Spawns initial immigrant households when immigration is enabled |

## Initialization order

```
WorldInit.initialize(seed, rc)
  1. FirmInit.create(rng)              → (firms, totalPop)
  2. Household.Init.create(rng, firms) → households
  3. ImmigrantInit.create(rng, ...)    → (households', totalPop')
  4. BankInit.create(firms, households)→ bankingSector
  5. Agent states: Nbp, Nbfi, Insurance, SocialSecurity, Jst, Immigration
  6. World.initial(...)                → World
```

`WorldInit` is the single entry point called from `McRunner.runSingle`.
