# Population-Control Bundle Format

## Status and Scope

`PopulationControlBundleLoader` reads a data-only population-control component
from UTF-8 TSV files. It constructs the typed
`PopulationControlSchema.Bundle`, checks component integrity, and applies the
schema's cross-table reconciliation rules before a future population compiler
can consume it.

This is not a full `BaselineBundle`, a public Research API, or a population
compiler. It does not load `SimParams`, execute Scala configuration, initialize
the runtime, or establish a Poland calibration. The committed
`synthetic-v1` fixture is a test artifact only; it is not an empirical baseline.

The target contract and the remaining compiler work are defined by
[RFC-0001](rfc/0001-population-and-representation.md). The relationship to the
future full baseline bundle and Research API is defined by
[RFC-0002's contract](rfc/0002-research-api-contract.md).

## Bundle Layout

Each component root must provide these files:

```text
manifest.tsv
tables.tsv
regions.tsv
age-bands.tsv
production-sectors.tsv
persons.tsv
households.tsv
household-membership.tsv
demographic-labour.tsv
regional-labour.tsv
employment.tsv
```

All files are UTF-8, tab-separated, and include one header row. Header names
and table values are part of the schema. Unknown classifications, enum values,
missing metadata, duplicate metadata entries, invalid counts, and invalid
cross-table identities reject the component. The current loader ignores
additional root entries; a future full baseline bundle will own its complete
artifact inventory.

## Manifest and Integrity

`manifest.tsv` has exactly one data row:

| Column | Meaning |
| --- | --- |
| `schema_version` | Must equal the supported `PopulationControlSchema` version. |
| `baseline_id` | Immutable identity of the baseline that will contain this component. |
| `population_scope` | Runtime population perimeter, currently `all_usual_residents` or `private_household_residents`. |
| `population_controls_digest` | Canonical SHA-256 digest for this component. |
| `region_classification_id`, `region_classification_version` | Region classification identity. |
| `age_classification_id`, `age_classification_version` | Age-band classification identity. |
| `production_sector_classification_id`, `production_sector_classification_version` | Production-sector classification identity. |

The component digest covers the schema version, baseline ID, population scope, all three
classification references, and the raw bytes of every other required TSV. The
digest field itself is excluded, so it does not self-reference. A changed table
or classification file must therefore be accompanied by a newly computed
digest and a new immutable baseline version under baseline governance.

The digest here is a component-level integrity check. A future full baseline
digest must additionally cover parameters, institutional opening state,
provenance, validation evidence, and its complete artifact inventory.

## Source Metadata

`tables.tsv` has one row for each control table: `persons`, `households`,
`household_membership`, `demographic_labour`, `regional_labour`, and
`employment`.

| Column | Meaning |
| --- | --- |
| `table` | Table identity. |
| `control_family` | Required family for that table: `persons`, `households`, `household_membership`, `labour`, or `employment`. |
| `statistical_universe` | Human-readable population universe represented by the control. |
| `strength` | `hard` or `calibrated_margin`. |
| `absolute_tolerance` | Non-negative integer reconciliation tolerance. |
| `source_provider`, `source_location` | Provider and stable source location. |
| `observation_period`, `release`, `accessed_at` | Source vintage. `accessed_at` is an ISO-8601 date. |
| `transformation` | Declared extraction, bridge, or transformation applied to the source. |

This metadata is evidence attached to each control table. It does not turn a
source into a real-time information vintage: that classification belongs to the
full baseline and experiment contracts.

## Classifications and Control Tables

`regions.tsv`, `age-bands.tsv`, and `production-sectors.tsv` declare the only
codes allowed by dependent tables. The rows are:

| File | Required columns |
| --- | --- |
| `regions.tsv` | `code` |
| `age-bands.tsv` | `code`, `min_inclusive`, `max_inclusive` (`max_inclusive` may be empty) |
| `production-sectors.tsv` | `code` |
| `persons.tsv` | `region`, `sex`, `age_band`, `residence`, `count` |
| `households.tsv` | `region`, `size`, `composition`, `count` |
| `household-membership.tsv` | `composition`, `member_role`, `age_band`, `count` |
| `demographic-labour.tsv` | `sex`, `age_band`, `status`, `count` |
| `regional-labour.tsv` | `region`, `status`, `count` |
| `employment.tsv` | `residence_region`, `workplace_region`, `production_sector`, `count` |

`count` is a non-negative `Long` represented-unit count. The accepted enum
codes are the explicit TSV projections of the typed schema; the loader does
not infer classifications from labels or IDs.

`employment.tsv` counts primary employment assignments: one main-job assignment
per employed resident. It does not count all job positions, FTE, vacancies, or
secondary jobs. This keeps its residence-region total in the same jobholder
unit as the employed-resident control.

After parsing, the loader delegates to `PopulationControlSchema.Validator`.
This enforces the declared reconciliation identities, including demographic and
regional labour totals, household membership, and employed-resident to
primary-employment-assignment reconciliation. A component is loadable only when all errors are
empty.

## Current Use

The format is currently exercised by the synthetic fixture in
`modules/model/src/test/resources/population-control-bundles/synthetic-v1` and
its loader specification. It is an internal target-core boundary, deliberately
separate from the legacy `BaselineCatalog` provider over `SimParams.defaults`.

The next implementation steps are to create an evidence-backed population
component for a real baseline, extend it with the remaining baseline
components, compile a population from it, and expose only the resulting
versioned baseline through the supported Research API.
