# ADR-0012: Synthetic Workplace for First-Target Employment

Status: Accepted

Date: 2026-07-24

Amends: the first-target employment and location boundary in
[ADR-0011](0011-first-target-model-ontology-and-resolution-boundaries.md),
without changing `Enterprise` as the consolidated statistical-enterprise and
financial-counterparty unit.

## Context

ADR-0011 deliberately leaves establishments and local units unresolved. That
remains correct for legal, administrative, and empirical units: the first
target cannot reconstruct the real Polish establishment population or individual
employment contracts.

The first target nevertheless needs an explicit production and employment
locus. A person is employed at a place in the model; an enterprise may later
operate more than one such place. Reusing the enterprise's registered or
main-job seat as the model ontology would conflate a source concept with an
economic unit. Conversely, omitting `Workplace` would make later multi-site
production, workplace capacity, and commuting extensions change the meaning of
the employment relation and Research API.

For `PL-2025-Q4-v1`, available evidence supports a current aggregate
distribution by PKD 2007, sex, and the seat of the entity of the main job. It
does not identify an employee's actual work location. The model must preserve
that limitation in baseline provenance rather than embed it in type names.

## Decision

Amor Fati introduces `Workplace` as a first-target synthetic economic unit:

```text
Person --employment--> Workplace --operated-by--> Enterprise
```

`Workplace` is the model's locus of labor and production. It has a stable
`WorkplaceId`, an enterprise owner, production-sector classification, modeled
location, operational status, and the same represented quantity as its owning
enterprise in the first target. It is not a legal establishment, local unit,
registered seat, or claim that the model knows an empirical physical address.

The first target uses exactly one workplace for every ordinary synthetic
enterprise. The workplace-to-enterprise link may be stored as a typed foreign
key because it is one-to-one and has no independently modeled contract terms in
v1. Enterprise entry and exit create or retire its workplace atomically. A
workplace has no independent financial balance-sheet, ledger, banking, or
ownership role; those remain with the enterprise.

The `Employment` relation connects a person or labor cohort to a workplace.
The employer enterprise, sector, and any firm-level financial consequences are
resolved through that workplace. An employed person has at most one primary
employment assignment in v1. Self-employment and helping-family work follow a
separate declared population-compiler rule and are not silently rewritten as
employment by another enterprise.

For `PL-2025-Q4-v1`, each workplace's modeled location is initialized using a
baseline-declared **entity-seat proxy**. The baseline manifest must expose that
evidence class and the source's statistical universe. It must not label the
result as an observed actual workplace or use it as a validated commuting fact.

Future baselines may represent several workplaces per enterprise, use an
empirical actual-workplace source, or add commuting and capacity relationships.
Those extensions require the normal resolution-promotion evidence: a research
question, empirical controls, reconciliation rules, lifecycle semantics, and
validation evidence.

## Consequences

- The target ontology and Research API use `Workplace`, never a source-specific
  field such as `employerSeatPowiat`, for the economic unit.
- Baseline provenance states how a workplace location was initialized. That
  provenance is independently queryable from the model's economic state.
- The target data-oriented core has a `WorkplaceTable` and typed person-to-
  workplace employment link. Its physical layout remains a downstream RFC-0003
  decision.
- `Enterprise` remains the consolidated producer, owner of productive assets,
  financial counterparty, issuer, borrower, and lifecycle owner. `Workplace`
  does not duplicate those responsibilities.
- Current `HhStatus.Employed(firmId, sectorIdx, wage)` cannot be carried into
  the target unchanged: its employer reference becomes an employment link to a
  workplace, and sector is derived from the workplace unless a job-specific
  classification is explicitly needed.
- A `v1` result can answer where work occurs **in the model** and disclose its
  location evidence. It cannot claim that a person worked at that empirical
  location or that observed commuting has been reproduced.

## References

- [ADR-0011: First-Target Model Ontology and Resolution Boundaries](0011-first-target-model-ontology-and-resolution-boundaries.md)
- [RFC-0001: Population and Representation](../rfc/0001-population-and-representation.md)
- [RFC-0003: Model Ontology and State Architecture](../rfc/0003-model-ontology-and-state-architecture.md)
- [RFC-0003 Ontology Audit Matrix](../rfc/0003-model-ontology-matrix.md)
- [PL-2025-Q4-v1 employment-attachment evidence](https://github.com/amor-fati-systems/amor-fati-economies/tree/main/docs/baselines/PL-2025-Q4-v1)
