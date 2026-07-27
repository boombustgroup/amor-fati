# Architecture Decision Records

This directory stores durable architecture decision records. Proposed ADRs make
a candidate decision explicit while its owning RFC remains under review;
accepted ADRs preserve the chosen trade-off and its consequences. They are not
living tutorials; the current architecture narrative lives in
[docs/architecture](../architecture/).

ADR-0001 through ADR-0005 are retroactive records for decisions already
embedded in the codebase before this ADR series was started. Later ADRs should
normally be written when a new durable decision is made or an existing one is
revised.

Each retroactive ADR includes a `Historical Provenance` section with mainline
commit anchors found from file history and symbol-level searches. These anchors
are evidence of when the decision entered `main`; for decisions that evolved
over multiple PRs, the ADR records both the primary anchor and relevant
supporting commits. The `Date` field on a retroactive ADR uses the primary
historical anchor date, not the date the ADR text was written.

## Records

| ADR | Status | Decision |
| --- | --- | --- |
| [ADR-0001](0001-ledger-first-runtime.md) | Accepted | Runtime monetary flows execute through the ledger-first path. |
| [ADR-0002](0002-explicit-month-boundary.md) | Accepted | `FlowSimulation.SimState` and explicit month randomness define the public month boundary. |
| [ADR-0003](0003-separate-verified-ledger-repository.md) | Accepted | The verified accounting kernel lives in the separate `amor-fati-ledger` repository, checked out as `modules/ledger`. |
| [ADR-0004](0004-ledger-owned-financial-state.md) | Accepted | Supported ledger-backed financial stocks live in `LedgerFinancialState`. |
| [ADR-0005](0005-fixed-point-domain-numerics.md) | Accepted | Domain numerics use Long-backed fixed-point opaque types instead of untyped floating-point values. |
| [ADR-0006](0006-data-oriented-high-cardinality-state.md) | Accepted | High-cardinality runtime state uses data-oriented tables while typed objects remain at control and observation boundaries. |
| [ADR-0007](0007-controlled-model-core-replacement.md) | Accepted | The stateful model core is replaced natively on the target ontology while verified accounting and scientific infrastructure are preserved. |
| [ADR-0008](0008-explicit-reference-population-and-representation-scale.md) | Accepted | Runs separate a versioned reference population from their explicit representation scale and weights. |
| [ADR-0009](0009-research-api-as-supported-scientific-interface.md) | Proposed | A versioned Research API is the supported programmatic scientific interface across adapters. |
| [ADR-0010](0010-managed-almond-jupyter-runtime-and-committed-notebooks.md) | Proposed | Amor Fati supplies a managed Almond/Jupyter kernel and committed executable notebooks. |
| [ADR-0011](0011-first-target-model-ontology-and-resolution-boundaries.md) | Accepted | The first-target ontology and representation boundaries define units, relationships, contracts, assets, institutions, and promotion rules. |
| [ADR-0012](0012-synthetic-workplace-for-first-target-employment.md) | Accepted | A synthetic workplace is the first-target labor and production locus; its v1 location provenance is separate from the model ontology. |

## Format

New ADRs should use this shape:

```text
# ADR-NNNN: Title

Status: Proposed | Accepted | Superseded

Date: YYYY-MM-DD

## Context
## Decision
## Consequences
## References
```

When an ADR supersedes another ADR, keep both files and mark the old record as
superseded with a link to the replacement.
