# RFC-0003: Model Ontology and State Architecture

Status: Draft for decision
Scope: Economic units, relationships, instruments, real assets, representation
resolution, persistent state, monthly workspace, evidence boundaries, and
data-oriented storage
Performance: Storage shape is in scope; performance claims and tuning are not

## Purpose

Amor Fati contains more economic structure than its current top-level runtime
state makes explicit. Persons and households are conflated, employment and bank
relationships are embedded in agent fields, firm ownership is represented by
Booleans, and several financial instruments are stored as balances by owner
rather than as bilateral contracts or holder-resolved positions. At the same
time, the model already contains named banks, public institutions, social funds,
financial markets, foreign-sector proxies, real assets, networks, and a verified
ledger boundary.

This RFC defines the target ontology and state architecture for the whole model.
It answers four separate questions:

1. What economic units, assets, claims, and relationships exist in the model?
2. At what resolution is each family represented in a particular baseline?
3. Which state is persistent at a month boundary, and which state is temporary
   calculation, flow, event, or observation data?
4. Which high-cardinality families require data-oriented storage?

The answers must remain separate. An entity can belong to the ontology while
remaining an aggregate singleton. A monthly result can require a columnar batch
without becoming a persistent economic entity. A small named population can
remain a typed Scala structure without disappearing from the ontology.

## Relationship to Other Documents

This RFC owns two distinct gates. Its semantic ontology gate follows the
population's statistical-unit definitions and precedes the Research API. Its
physical state-design gate follows the Research API's accepted access patterns
and translates the ontology into indexes, views, and storage boundaries. The
[ontology audit matrix](0003-model-ontology-matrix.md) inventories the current
code and records the decisions required for the first gate. Related documents
have narrower responsibilities:

- [Population and representation RFC](0001-population-and-representation.md)
  defines the reference economy, statistical population, representation scale,
  synthetic-population compiler, migration, and tourism policy.
- [Research API and notebook runtime RFC](0002-research-api-and-notebook-runtime.md)
  defines experiment construction, execution lifecycle, result queries,
  evidence manifests, committed notebooks, and the managed Almond/Jupyter
  adapter.
- [JVM runtime, JIT, and garbage collection policy RFC](0004-jvm-runtime-jit-and-garbage-collection-policy.md)
  defines the managed process topology, runtime profiles, provenance, and
  evidence used to qualify the target DOD allocation and live-set behavior.
- [ADR-0006](../adr/0006-data-oriented-high-cardinality-state.md) records the
  accepted storage principle for high-cardinality persistent state.
- [ADR-0007](../adr/0007-controlled-model-core-replacement.md) records the accepted
  strategy for replacing the stateful core without preserving its internal API
  or rewriting the verified accounting and scientific infrastructure.
- [ADR-0011](../adr/0011-first-target-model-ontology-and-resolution-boundaries.md)
  records the accepted first-target ontology and semantic resolution choices.
- [ADR-0012](../adr/0012-synthetic-workplace-for-first-target-employment.md)
  resolves the first-target synthetic workplace used by employment without
  promoting empirical establishments or local units.
- [ADR-0004](../adr/0004-ledger-owned-financial-state.md) records the single-owner
  rule for supported ledger-backed financial stocks.
- [State and ledger boundary](../architecture/state-and-ledger-boundary.md)
  documents the currently implemented ownership boundary.
- [Model notation and state vector](../model-notation-and-state-vector.md) remains
  the canonical description of implemented state, not of this proposed target.

This RFC must not be read as a claim that the target ontology is already
implemented. When accepted and implemented, canonical model and architecture
documentation must be updated separately.

## Proposed Decisions

1. Amor Fati has one explicit model ontology covering institutional units, real
   assets, relationships, financial instruments and positions, classifications,
   events, and aggregate market or policy state.
2. Every baseline declares the representation resolution of each ontological
   family. `1:1`, weighted synthetic units, cohorts, aggregate singletons, and
   intentionally absent families are distinct modes.
3. Ontological identity is not inferred from a Scala class or ledger sector.
   Stable typed IDs identify units, contracts, instruments, positions, and
   relationships across month boundaries and evidence exports.
4. High-cardinality homogeneous state and large batch workspaces use dedicated
   data-oriented storage. Low-cardinality institutional, policy, market, and
   control state remains typed.
5. Relationships that have independent economic meaning or lifecycle are state,
   not overloaded entity fields. Employment, ownership, accounts, credit,
   positions, tenure, and variable-degree networks receive explicit schemas.
6. A generic `FinancialContractTable` is not the target. Deposits, credit,
   securities, ownership stakes, insurance claims, pension claims, and
   interbank exposures have separate semantic schemas even where they share
   infrastructure.
7. Contract and instrument tables own identity, parties, terms, and lifecycle.
   The ledger owns supported financial principal and balances. The same stock
   must not have two independently mutable owners.
8. Markets, mechanisms, and `engine.economics` step outputs are not
   automatically entities. Their persistent state, transient workspace,
   commands, events, and observations are classified explicitly.
9. The explicit monthly transition remains authoritative. Data-oriented storage
   must preserve opening-state isolation, deterministic replay, atomic closing,
   and reconciliation before state publication.
10. Public research interfaces expose typed views, queries, manifests, and
    exports. Researchers do not configure primitive column layouts.
11. The target is delivered through the controlled model-core replacement
    recorded by ADR-0007. Compatibility with the current internal agent-state
    API is not a design requirement.

## Core Migration Strategy

[ADR-0007](../adr/0007-controlled-model-core-replacement.md) accepts a controlled
replacement of the stateful model core. This avoids designing the target around
temporary compatibility with `Household.State`, `FirmState`, copied entity
vectors, overloaded `bankId` routing, or object-row financial state.

The replacement is not a clean-room rewrite of Amor Fati. The boundary is:

| Preserve and reuse | Replace in the new core |
| --- | --- |
| Verified ledger library and ledger-first execution | Population initialization and compilation |
| Fixed-point domain numerics and typed identifiers where semantically valid | Household/person and enterprise persistent object rows |
| Random-stream ownership and deterministic seed schedule | Root ownership of high-cardinality state |
| Explicit month-boundary semantics | Embedded membership, employment, ownership, bank, and network relationships |
| Economic equations and pure mechanisms after dependency review | Agent-oriented kernels tied to whole-row copying and `copy` transitions |
| Flow vocabulary, SFC projection, reconciliation, and accounting invariants | Object-row `LedgerFinancialState` storage and aggregate-only routing where contracts are required |
| Aggregate market, policy, fiscal, monetary, and external state where ontology remains valid | Same-month DTOs that duplicate complete persistent populations |
| Calibration evidence, scenario definitions, diagnostics, and scientific exports where their meaning remains valid | Initialization controls whose population or scaling meaning is invalidated by the new baseline compiler |
| Existing tests as behavioral evidence after classification | Tests that merely encode an obsolete storage shape or conflated ontology |

The current engine is frozen as a behavioral and accounting oracle. It is not a
production compatibility layer for the new core. Test-only fixtures, projections,
or adapters may feed comparable inputs into both engines, but no production
month executes by translating persistent state back and forth.

The replacement follows these constraints:

1. The new core is developed in this repository so history, domain code,
   calibration, tests, and the ledger remain directly reusable.
2. The new core owns its ontology, tables, contracts, workspace, and monthly
   closing state natively from its first end-to-end execution.
3. Old APIs are reused only when their semantics remain correct. API
   compatibility is not a goal.
4. Porting is selective. A pure equation or mechanism is reused or moved; code
   whose behavior depends on the obsolete state shape is reimplemented against
   the new tables.
5. At no point may the old and new cores jointly own one production state.
6. Fixed-seed differential tests compare mechanisms on controlled inputs.
   Full-run time-series equality is not required where the corrected population
   ontology intentionally changes outcomes.
7. Aggregate, accounting, directional scenario, replay, and invariant evidence
   must pass the cutover gates before the new core replaces the old runtime.
8. Aggregate families remain aggregate unless a separately justified research
   requirement and empirical bridge require greater resolution.

The intended path is:

```text
freeze current engine as oracle
              |
              v
build native target core -> thin end-to-end month -> port active mechanisms
              |                                      |
              `---------- differential harness ------'
                                     |
                                     v
                              satisfy cutover gates
                                     |
                                     v
                    replace old core and remove oracle path
```

This strategy deliberately accepts a temporary second implementation in a
development and test context. It rejects permanent dual runtime, compatibility
architecture, and piecemeal production ownership split across old and new state.

## Delivery Scope

The complete ontology is deliberately larger than the first implementation
slice. Every family belongs to one of three delivery classes.

### Mandatory foundation

The first target foundation contains only the state required to correct
population meaning, representation scale, and currently active bilateral
behavior:

- baseline manifest, classification catalogs, representation modes, weights,
  and stable-ID policy;
- `PersonTable`, `HouseholdTable`, `EnterpriseTable`, and `WorkplaceTable`;
- household membership and employment relations;
- existing social and production networks in compact indexed storage;
- explicit opening deposit-account and credit-contract assignments for the
  products already active in the model;
- the existing named bank population and other required institutional IDs;
- a persistent columnar ledger projection with one authoritative balance owner;
  and
- typed views, change sets, and equivalence evidence for migrated kernels.

This foundation does not require resolved dwellings, individual insurers,
security-by-security bond books, bank shareholders, or individual local
governments.

### Initially aggregate

The following families remain typed aggregate singletons, small named
populations, weighted cohorts, or diagnostic projections in the initial target
unless a narrower design decision promotes them:

- NBP, central government, consolidated JST, ZUS/FUS, NFZ, PPK, and earmarked
  funds;
- insurers, NBFIs, investment funds, and quasi-fiscal vehicles;
- regional housing markets and aggregate dwelling wealth;
- foreign trade partner and sector cohorts;
- aggregate insurance reserves and pension or fund claims;
- government, corporate, and quasi-fiscal security portfolios at their current
  holder resolution; and
- market, policy, expectations, fiscal, monetary, and external-sector state.

Their representation mode and known loss of heterogeneity are recorded in the
manifest. They remain part of the ontology without becoming DOD populations.

### Optional future resolution

The following are extension paths, not prerequisites for the population and DOD
migration:

- individual dwellings, landlords, tenancy contracts, and collateral links;
- individual insurers, policies, beneficiaries, and claims;
- person-level pension entitlements and fund-unit positions;
- security-by-security government and corporate bond instruments;
- explicit bank shareholders and holder-resolved bank equity;
- individual JST units and their debt holders;
- named or synthetic foreign enterprises and counterparties; and
- micro-level visitor cohorts.

Promotion from aggregate to resolved state requires a research question,
empirical controls, reconciliation rules, lifecycle semantics, and validation
evidence. Architectural possibility alone is insufficient.

## Ontology and Storage Are Different

The ontology describes economic meaning. Storage describes an implementation
strategy. The following distinctions are normative:

| Concept | Question answered | Example |
| --- | --- | --- |
| Institutional unit | Who can make decisions, own assets, incur liabilities, or receive flows? | Household, enterprise, bank, NBP, central government. |
| Real asset | What non-financial object or stock is produced, used, occupied, or owned? | Dwelling, inventory, physical capital. |
| Relationship | Which independently meaningful link connects units or assets? | Membership, employment, ownership, tenancy, supply link. |
| Contract or instrument | What enforceable or accounting claim connects issuer, borrower, lender, or holder? | Mortgage, deposit account, bond, equity position. |
| Market or policy state | What aggregate price, rule, signal, or institutional regime persists? | Yield curve, policy rate, housing price index. |
| Event or flow | What occurred during a period? | Origination, default, migration, tax payment. |
| Observation or evidence | What is derived for validation, diagnostics, or export? | Gini coefficient, SFC residual, decision trace. |
| Storage family | How is a high-cardinality set processed efficiently? | Structure-of-arrays person table or indexed edge store. |

No rule requires one class per ontological concept or one table per class. No
rule permits an economic relationship to remain semantically ambiguous merely
because it is currently encoded as a scalar field.

## Representation Resolution

Every baseline manifest must classify each ontological family using one of the
following representation modes:

| Mode | Meaning |
| --- | --- |
| Named census unit | A known institution represented individually with weight one, such as a systemically relevant bank. |
| Synthetic micro unit | A disclosure-safe generated unit representing one empirical unit. |
| Weighted synthetic unit | A generated unit representing multiple empirical units in a controlled stratum. |
| Weighted cohort | A group represented by common state and a weight where individual identity is not behaviorally required. |
| Aggregate singleton | One consolidated state representing an entire institutional sector, market, fund, or mechanism. |
| Diagnostic projection | A derived view of authoritative state, not an independently evolving unit. |
| Not represented | A family deliberately outside the baseline, with the omission declared. |

Representation mode is baseline metadata, not a permanent property of the
ontology. For example, JST can be one aggregate singleton in the first Poland
baseline and later become a population of local-government units without
redefining what local government means. Foreign trade partners can remain
sector-partner cohorts while domestic enterprises use weighted synthetic units.

Each represented family declares:

- statistical or legal unit definition;
- institutional sector and relevant classifications;
- stable-ID namespace;
- representation mode and weight semantics;
- opening controls and reconciliation tolerances;
- lifecycle rules;
- authoritative state owner; and
- whether a row is an economic unit, cohort, contract, position, or diagnostic
  projection.

## Classifications and Reference Catalogs

Classifications are versioned reference data, not scattered enums or implicit
integer meanings. The target reference state includes at least:

- institutional sectors and subsectors;
- modeled entity and relationship types;
- industry classification plus the bridge to Amor Fati production sectors;
- geography, residency, and territorial coverage;
- enterprise size, legal form, listing status, and ownership or control class;
- labor-force, employment-contract, education, activity, and program status;
- account, credit, security, insurance, pension, and collateral product types;
- currency, valuation basis, maturity, seniority, and accounting classification;
  and
- representation mode, weight unit, aggregation, and consolidation policy.

Runtime codes can use compact numeric encodings, but the baseline manifest must
bind every code to a stable catalog version and human-readable meaning. A code
change cannot silently reinterpret a persisted population or result bundle.

## Economic Units

### Persons and households

`Person` is the demographic, residency, education, health, labor-supply, and
pension-eligibility unit. `Household` is the co-resident consumption, pooled
finance, housing, and retail-credit unit. Membership is an explicit relation.
The detailed statistical definitions belong to the population RFC.

### Non-financial enterprises

`Enterprise` is the default firm unit and follows the Eurostat statistical-unit
concept in [Council Regulation (EEC) No 696/93](https://eur-lex.europa.eu/eli/reg/1993/696/oj/eng):
an autonomous producer may rest on one or more legal units and operate at one
or more locations. In the first target it is also the consolidated synthetic
counterparty that owns productive assets, holds financial positions, and enters
contracts. A weighted row may represent multiple empirical enterprises in one
controlled stratum; a declared systemic enterprise may have weight one.

Legal units, enterprise groups, establishments, and local units are not
separately resolved first-target units. A group is an ownership/control layer,
not an additional ordinary enterprise. ADR-0012 nevertheless requires exactly
one synthetic `Workplace` per enterprise in the first target. It is the model's
labor and production locus, not a legal establishment, local unit, or observed
site. Its modeled location may be initialized from a baseline-declared
entity-seat proxy. Separate establishments, local units, or multiple workplaces
per enterprise require later promotion when regional production, site-level
employment, or capacity is behaviorally required. The baseline must record the
crosswalk, location-evidence class, and consolidation rules rather than
silently mixing these universes.

Enterprise state includes operational status, sector, size, region, technology,
production, inventories, capital, pricing, distress, and lifecycle. Employment,
ownership, banking, securities issuance, and supply-network links remain
separate relations or contracts.

### Financial corporations

The ontology distinguishes:

- commercial banks and other deposit-taking institutions;
- the central bank;
- insurers;
- investment funds and fund managers where the distinction matters;
- non-bank credit providers;
- pension or retirement-savings vehicles; and
- resolution or deposit-guarantee institutions where modeled as balance-sheet
  owners rather than policy mechanisms.

The first target keeps commercial banks as named units with weight one. The
insurance sector remains one aggregate institution, while TFI/investment funds
and credit NBFIs are two distinct aggregate institutions. Greater institution
resolution requires promotion under the evidence rule.

### General government and public institutions

The ontology distinguishes central government, local government, social
security, healthcare funds, earmarked funds, and any public corporation or
quasi-fiscal vehicle. Consolidation rules must be explicit. A treasury
settlement account is not automatically the same unit as central government,
and an execution shell is not an institutional owner.

NBP, central government, ZUS/FUS, NFZ, PPK, FP, PFRON, FGSP, and BFG are named
aggregate singletons. JST remains one consolidated local-government sector.
BGK and PFR are distinct named low-cardinality units even where the current
runtime carries one BGK/PFR-like state. Consolidation with general government
must be declared per accounting view rather than implied by shared storage.

### Rest of the world

The foreign sector distinguishes foreign institutional owners, trade partner
or foreign-firm cohorts, external investors, visitors, and settlement
counterparties. The current GVC rows are sector-partner proxies, not claims to
represent individual foreign companies. Runtime trade, income, capital, or
transfer settlement shells are not persistent foreign units.

## Real Assets and Non-financial Stocks

The ontology includes real assets even when their first representation remains
an aggregate stock:

- dwellings or housing cohorts, with region and tenure-relevant attributes;
- enterprise physical capital;
- green capital;
- inventories;
- productive technology or intangible-capital state where economically
  distinct; and
- collateral links between a real asset and a secured credit contract.

A scalar enterprise capital stock can remain an enterprise column until asset
vintage or individual-asset heterogeneity affects behavior. The first housing
target uses regional housing cohorts linked to households through explicit
tenure and collateral relations carrying represented quantity or value. It does
not assign identities to individual dwellings. Individual assets are promoted
only when transaction, construction-capacity, mobility, or collateral evidence
requires them; an aggregate price index is never treated as a dwelling
population.

## Relationships

The target relationship families are:

| Relationship | Parties | Minimum semantic state |
| --- | --- | --- |
| Household membership | Person to household | Role, start/end boundary, represented quantity where weighted. |
| Employment | Person or labor cohort to workplace | Contract type, job quantity, wage terms, sector, start/end, status. The workplace resolves to its operating enterprise through a typed foreign key. |
| Residency and migration episode | Person to geography | Usual-residence status, origin, destination, start/end, migration reason where modeled. |
| Tenure or occupancy | Household to dwelling or housing cohort | Owner/tenant/other tenure, housing cost, start/end. |
| Enterprise ownership | Owner unit to enterprise | Stake or control share, ownership class, listed/private status, start/end. |
| Deposit service | Household or enterprise to bank | Account identity, product, currency, status, terms, ledger balance key. |
| Credit | Borrower to lender, optionally collateral | Product, origination, maturity, rate terms, status, ledger balance key. |
| Security issuance | Issuer to instrument | Instrument terms, issue and maturity state. |
| Security holding | Holder to instrument | Position identity, quantity or represented holding, valuation basis, ledger balance key. |
| Insurance | Policyholder or beneficiary to insurer | Product, coverage, premium terms, claim or reserve relation. |
| Pension or fund entitlement | Person or household to scheme | Contribution and entitlement status, fund position where resolved. |
| Fiscal entitlement or obligation | Person, household, or enterprise to public institution | Program or tax type, eligibility or liability interval, represented amount where persisted. |
| Interbank exposure | Lender bank to borrower bank | Instrument, amount, maturity, seniority or recovery class. |
| Production or supply link | Enterprise to enterprise or foreign cohort | Input class, weight or capacity, active interval. |
| Social link | Person or household to peer | Network type, weight where required, active interval. |

Common one-to-one or many-to-one links may use a foreign-key column when the
relationship has no independent attributes or lifecycle. Variable-degree links
and economically meaningful contracts use dedicated indexed stores. The choice
must be justified by semantics first and access pattern second.

## Financial Instruments, Contracts, and Positions

The financial model requires distinct schemas rather than one universal row.
The initial target families are:

### Deposit accounts

`DepositAccountTable` links an account holder to a deposit-taking institution
and stores product, currency, contractual status, and a ledger balance key.
Demand and term deposits can share infrastructure while preserving different
terms and liquidity behavior.

### Credit contracts

`CreditContractTable` owns borrower, lender, product, origination, maturity,
rate convention, amortization, collateral reference, performance stage, and
contract status. Product-specific extension columns or tables cover mortgages,
consumer credit, firm credit, NBFI credit, and quasi-fiscal lending without
forcing every kernel through irrelevant fields.

Principal and accrued financial balances remain ledger-owned. Risk state such
as arrears, non-performance, staging, and recovery belongs to the contract or a
contract-indexed risk table, not only to aggregate bank buckets.

### Securities and ownership

`SecurityInstrumentTable` owns issuer and instrument terms for government
bonds, corporate bonds, quasi-fiscal bonds, fund units, and listed equity where
resolved. `SecurityPositionTable` links holders to instruments. Issuance and
holder positions reconcile to the ledger and to market valuation state.
Instrument attributes include currency, coupon or indexation, maturity, and
seniority where applicable. Holder-specific valuation and accounting classes,
including AFS or HTM, belong to positions rather than instruments.

The first target uses instrument cohorts rather than a security-by-security
ISIN book. Government, corporate, and quasi-fiscal cohorts preserve issuer or
issuer class, currency, coupon or indexation, maturity, seniority, and risk as
relevant. BGK and PFR issuance remains distinguishable. Listed equity uses
positions in preserved systemic issuers or issuer cohorts; it does not create
an individual shareholder register.

`EnterpriseOwnershipTable` covers control and beneficial ownership of listed
and unlisted enterprises. It replaces `foreignOwned` and `stateOwned` Booleans
with explicit owners or owner classes and shares. Listed equity ownership and
enterprise-control classification must reconcile but are not assumed to be
identical concepts.

The first target uses a mixed resolution policy. Ordinary domestic-private
enterprises carry a declared ownership/control class without a private-owner
cap table. Publicly controlled enterprises link to a declared public owner or
owner cohort; foreign-controlled enterprises link to a rest-of-world owner
cohort. Ownership relations may express mixed stakes, but the derived control
class is singular and validated under a declared control rule. Listed security
positions remain a separate instrument layer governed by the security
resolution decision.

### Insurance, pension, and reserve claims

Policy or entitlement tables are introduced only at the resolution needed by a
research question. Until then, technical reserves and fund positions remain
declared aggregate or household-distributed projections. A projection used to
balance a sector is labeled as such and does not imply that individual policy
contracts have been modeled.

### Interbank instruments

Net interbank position per bank is insufficient for bilateral default
propagation. An `InterbankExposureTable` or a small dense bank-by-bank matrix
owns lender-borrower exposure. Its row count is small today, so semantic
clarity, not DOD performance, determines the representation.

### Net worth, ownership equity, and regulatory capital

Accounting net worth, a transferable equity instrument, enterprise control, and
regulatory bank capital are different concepts:

- enterprise ownership records who controls or beneficially owns an enterprise;
- listed or otherwise transferable equity is an instrument with issuer and
  holder positions;
- accounting net worth is a balance-sheet residual or institutional state; and
- regulatory bank capital is the capital buffer used for prudential ratios,
  loss absorption, failure, and resolution.

The current `BankState.capital` is persistent regulatory and accounting state,
not a holder-resolved bank-equity position. It remains valid institutional state
while outside the transferable ledger slice. Adding bank shareholders later
requires an explicit bank-equity instrument and holder positions reconciled to,
but not confused with, regulatory capital. The same distinction applies to
private-enterprise ownership and firm equity balances.

Every stock-like family must therefore be classified as one of: supported
ledger-owned balance, contract or instrument state, institutional accounting or
risk state, aggregate metric, diagnostic projection, or known unsupported
ownership family. A field must not enter the ledger merely because it has
currency units.

Fixed-point money is a hard target invariant across every classification. All
monetary values, flows, stocks, and account balances use `PLN` or the project's
equivalent opaque fixed-point domain types, never `Double` or `Float`. A
continuous calculation may enter accounting or ledger state only through a
checked conversion with explicit scale, rounding, and overflow behavior that
preserves SFC identities, including deterministic residual allocation where
rounding requires it.

## Flows and Events

Flows record activity over a period. Events record a discrete transition. The
ontology includes, at minimum:

- births, deaths, household formation or dissolution, immigration, emigration,
  and residency changes;
- hiring, separation, retirement, retraining entry or completion, and contract
  changes;
- enterprise entry, exit, ownership change, technology adoption, investment,
  and bankruptcy;
- account opening or closing, origination, issuance, purchase, sale,
  amortization, maturity, delinquency, default, restructuring, recovery, and
  write-off;
- bank failure, bail-in, resolution, and counterparty-loss propagation;
- tax, contribution, benefit, pension, healthcare, insurance, and quasi-fiscal
  payments; and
- domestic trade, imports, exports, tourism, remittances, income flows, capital
  flows, and valuation changes.

An event can update several authoritative stores atomically. A flow that does
not survive closing remains executed-flow evidence. A receivable, payable,
arrears balance, or other claim that survives closing must be promoted to an
explicit contract, position, ledger balance, or declared institutional state.

## Target State Architecture

The target month-boundary state has the following conceptual shape:

```text
SimulationState
|-- ReferenceState
|   |-- classification catalogs
|   |-- baseline and representation manifest
|   `-- immutable entity profiles
|-- PopulationState
|   |-- PersonTable
|   |-- HouseholdTable
|   |-- EnterpriseTable
|   `-- WorkplaceTable
|-- InstitutionalState
|   |-- FinancialInstitutionState
|   |-- PublicInstitutionState
|   `-- ForeignCohortState
|-- RelationshipState
|   |-- HouseholdMembershipTable
|   |-- EmploymentTable
|   |-- EnterpriseOwnershipTable
|   |-- ResidencyAndTenureTables
|   `-- NetworkStore
|-- ContractAndInstrumentState
|   |-- DepositAccountTable
|   |-- CreditContractTable
|   |-- SecurityInstrumentTable
|   |-- SecurityPositionTable
|   |-- InsuranceAndPensionTables
|   `-- InterbankExposureStore
|-- RealAssetState
|   |-- HousingOrDwellingState
|   `-- EnterpriseAssetState
|-- LedgerState
|-- MarketPolicyAndMacroState
`-- BoundaryMetadata
```

This is a semantic ownership tree, not a requirement for one giant case class.
Packages may expose narrower state handles. A table may share an allocator or
column infrastructure with another table while retaining a distinct domain
schema.

The monthly execution path additionally owns non-boundary data:

```text
MonthExecution
|-- MonthWorkspace        reusable scratch columns and matching buffers
|-- CommandsAndChangeSets validated proposed transitions
|-- BatchedFlows           typed BatchedFlow commands and executed-flow evidence
|-- RuntimeLedgerExecution topology, imperative-interpreter result, and deltas
|-- AccountingStockProjection supported executed deltas projected into closing stocks
|-- Events                defaults, failures, migration, entry, exit
|-- Observations          aggregates and requested snapshots
`-- Trace                 replay and diagnostic evidence
```

Production monthly monetary execution emits `BatchedFlow` values through
`MonthFlowEmitter`, executes them against `RuntimeLedgerTopology` through
`ImperativeInterpreter`, and projects the supported executed deltas into the
accounting-controlled `LedgerFinancialState` slice before publishing closing
state. Legacy flat `Flow` emitters and `RuntimeLedgerTopology.toFlatFlows` are
restricted to tests, diagnostics, and explicit compatibility paths; they are
not production month-execution inputs.

None of these families becomes persistent merely because it appears in a step
output. Only validated closing state is published as the next boundary.

## Data-Oriented Storage Boundary

### Required data-oriented families

The following are high-cardinality or naturally batch-processed and should use
structure-of-arrays, compact typed columns, or indexed edge storage:

- persons, households, ordinary enterprises, and workplaces;
- household membership, person-to-workplace employment, and enterprise ownership;
- deposit accounts, credit contracts, and security positions when resolved;
- social, production, and other variable-degree networks;
- high-cardinality real assets when enabled;
- persistent ledger balances; and
- large per-entity monthly workspaces, deltas, and requested event buffers.

### Conditional data-oriented families

Banks, insurers, funds, local governments, securities, dwellings, foreign
counterparties, and visitor units use DOD when their resolved population or hot
access pattern justifies it. Their ontology does not depend on that choice.

### Typed low-cardinality families

Typed immutable values remain the default for:

- aggregate market and policy state;
- a small named bank or institutional population where row processing is not a
  bottleneck;
- configuration, classifications, baseline metadata, and manifests;
- commands, validated transitions, and public results;
- aggregate diagnostics and SFC validation results; and
- on-demand row views used by tests, diagnostics, and researchers.

Raw arrays are package-private. Domain APIs use typed IDs, validated accessors,
and explicit transition methods. Separate columns do not weaken economic
invariants.

Collector or JIT evidence may select column encodings, buffer-reuse policy, and
capacity strategy only after semantic and month-boundary requirements are met.
The runtime RFC owns those measurements; no GC preference can expose storage or
redefine the ontology.

## Persistent State, Workspace, and Evidence

Every state-bearing field must be classified as one of:

| Class | Lifetime | Rule |
| --- | --- | --- |
| Reference | Baseline lifetime | Immutable input or classification. |
| Persistent micro state | Across month boundaries | Authoritative entity, relationship, contract, or real-asset state. |
| Persistent aggregate state | Across month boundaries | Market, policy, institutional, or lagged macro state. |
| Monthly workspace | Within one execution month | Reusable calculations; never published as independent state. |
| Change set or command | Until validation and closing | Proposed transition applied atomically. |
| Flow or event | Occurrence during one period | Append-only evidence or immediate input to accounting and closing. |
| Observation | Derived on demand or at configured cadence | Must not feed behavior unless promoted explicitly to lagged state. |

This classification prevents monthly `StepOutput` DTOs from becoming a second
state owner. In particular, vectors of updated firms, households, balances, or
per-entity flows in `engine.economics` should migrate toward table handles,
workspace buffers, change sets, or snapshots according to their lifetime.

## Current Implementation Mapping

| Current representation | Target interpretation |
| --- | --- |
| `FlowSimulation.SimState` | Existing month-boundary root; will reference the target stores without changing explicit month semantics. |
| `Household.State` | Temporary conflation to split across person, household, membership, employment, network, and relationship state. |
| `FirmState` | Enterprise row plus embedded bank routing, ownership classification, and network links to extract. |
| `Banking.BankState` and `Banking.MarketState` | Small bank population and aggregate banking-market state; contracts and exposures are separate. |
| `World` and its segments | Typed persistent aggregate, institutional, market, policy, and mechanism state. |
| `LedgerFinancialState` owner rows | Current authoritative financial-stock slice; migrate to persistent columnar ledger state and more resolved contract keys. |
| `AssetOwnershipContract` | Current owner-sector and asset-family registry; a basis for, not a replacement for, instrument and position ontology. |
| `HousingMarket.State` | Aggregate or regional housing-market and mortgage projection; no resolved dwelling population yet. |
| `GvcTrade.ForeignFirm` | Foreign sector-partner cohort or proxy, not a named foreign enterprise. |
| Interbank exposure matrix | Bilateral exposure relation, semantically valid even while stored as a small dense matrix. |
| `PipelineState`, `MonthlyCalculus`, and economics `StepOutput` | Lagged signals, monthly calculation transcripts, workspaces, and observations; not new economic units. |
| Runtime ledger shells | Execution or settlement counterparties; never persistent institutional owners. |

## Month-Boundary Rules

Signal timing follows three distinct stages for execution month `T`:

- **Stage A -- pre-signal:** opening state and inherited `T - 1`
  `DecisionSignals` form the pre-decision surface.
- **Stage B -- same-month:** a decision may consume only Stage A and explicitly
  ordered same-month state or `OperationalSignals` produced before that
  decision. It cannot consume a downstream or closing outcome from Stage C.
- **Stage C -- post-outcome:** realized end-of-month outcomes are closing
  evidence and may become behavioral input only through `SeedOut` at the
  `T + 1` boundary.

The pre-signal, same-month, and post-outcome surfaces remain strictly distinct.
This timing asymmetry prevents a realized outcome from causally feeding a
decision that helped produce it in the same month.

1. Stable IDs and dense row indices are distinct.
2. Opening state is read-only to same-month kernels.
3. Dynamic fields use current/next buffers, validated change sets, or an
   equivalent isolation mechanism.
4. Entry, exit, birth, death, migration, contract origination, maturity,
   default, and resolution update all affected stores at a declared transition
   boundary.
5. Relationship and contract references are validated against live IDs before
   closing.
6. Ledger execution and stock-flow reconciliation complete before closing state
   is published.
7. Slot reuse or compaction occurs only at a deterministic boundary and updates
   every dependent index.
8. Requested observations are derived from one coherent boundary or explicitly
   identified same-month evidence.

## Hard Invariants

The target architecture must enforce at least:

1. Every live entity, relationship, contract, instrument, and position has one
   stable typed ID in its namespace.
2. Every relationship references live, type-correct parties at a boundary.
3. Representation weights are positive and their unit meaning is declared.
4. Person, household, employment, and population controls satisfy the
   population RFC invariants.
5. Enterprise ownership shares and classifications reconcile within declared
   tolerances.
6. Every credit or account contract has a valid institution and borrower or
   holder.
7. Instrument issuance reconciles to holder positions plus explicitly declared
   residual or aggregate holdings.
8. Bilateral interbank assets reconcile to counterpart liabilities and bank net
   positions.
9. Secured credit collateral references a valid real asset or an explicitly
   aggregate collateral pool.
10. Persistent financial principal and balances have one authoritative owner.
11. Every monetary value and account balance uses an opaque fixed-point domain
    type; floating-point values cannot enter accounting or ledger state.
12. Aggregate market or institutional projections identify their source and do
    not independently drift from authoritative micro or ledger state.
13. Execution and settlement shells cannot survive as end-of-month owners.
14. Current-month workspaces and observations cannot silently become behavioral
    memory.
15. A change of representation resolution preserves controlled true-scale
    stocks and flows within declared tolerances.
16. All state publication is atomic at the explicit month boundary.

## Research-Facing Contract

A researcher configures a baseline, representation policy, scenarios, seed,
requested evidence, and optional resolution modules. The run manifest reports:

- every ontological family and its representation mode;
- simulated and represented counts;
- institutional consolidation choices;
- enabled contract, asset, and network resolution;
- aggregate-only and diagnostic-only families;
- omitted families;
- opening reconciliation residuals;
- table and schema versions; and
- requested snapshots or event evidence.

The researcher-facing API uses domain names and typed selectors. Primitive
encodings, row compaction, buffer swaps, and ledger indices remain internal.
The complete public lifecycle, query, reproducibility, notebook, and adapter
contract belongs to the
[Research API and notebook runtime RFC](0002-research-api-and-notebook-runtime.md).

## Implementation Sequence

### Phase 0: freeze semantics and evidence

1. Complete the population and representation decisions required by the first
   target-core slice.
2. Freeze the model-wide ontology vocabulary, delivery classes, representation
   modes, stable-ID namespaces, authoritative owners, relationship and contract
   semantics, and state-lifetime taxonomy.
3. Complete the Research API decisions that define experiment lifecycle,
   public observations, evidence retention, and dominant access patterns over
   the accepted ontology.
4. Identify the existing tests, fixed-seed runs, ledger identities, SFC checks,
   aggregates, and diagnostics that form the comparison oracle for each future
   slice.
5. Record current behavior where coverage is insufficient before changing state
   ownership.

### Phase 1: build the native foundation

1. Translate the accepted population and Research API contracts into the native
   foundation without exposing storage internals through the public boundary.
2. Make the population compiler emit native person, household, enterprise,
   membership, and employment tables plus a reconciliation manifest.
3. Introduce typed entity-reference, relationship, allocator, table-view,
   workspace, and change-set infrastructure without introducing a generic ECS.
4. Introduce deposit-account and credit-contract schemas for the products
   required by the first end-to-end execution.
5. Build the new root state and explicit monthly closing boundary over these
   stores.
6. Validate the compiled opening state against empirical controls and opening
   ledger identities before implementing behavioral progression.

### Phase 2: complete a thin end-to-end month

Implement the smallest scientifically coherent execution:

```text
baseline and compiled population
        -> labor and wages
        -> household income and consumption
        -> enterprise production and decisions
        -> active financial flows
        -> ledger execution and SFC validation
        -> atomic next-month state
```

This path uses native target tables from initialization through closing. It may
reuse pure equations and aggregate institutional state, but it must not execute
through the old `Household.State` or `FirmState` runtime.

### Phase 3: port the active model surface

1. Port remaining household, labor, enterprise, entry, exit, migration,
   retirement, network, and distress mechanisms against the new stores.
2. Port active deposit, mortgage, consumer-credit, firm-credit, default,
   amortization, risk, and bank-aggregation mechanisms against explicit
   contracts.
3. Re-key ledger balances where required while preserving ADR-0004 single
   ownership and existing SFC semantics.
4. Reuse aggregate fiscal, monetary, housing, financial-market, and
   external-sector mechanisms where their state meaning remains valid.
5. Replace copied per-entity vectors with table views, reusable workspace,
   change sets, and cadence-controlled evidence as each mechanism is ported.
6. Maintain a test-only differential harness using matched fixtures and
   controlled inputs. Do not add a production old-to-new state adapter.

### Phase 4: qualify and cut over

1. Satisfy every cutover gate below on the new core.
2. Make the new core the only production runtime and default command path.
3. Remove the old agent core, old economics orchestration paths, comparison-only
   production wiring, and obsolete storage-shape tests.
4. Retain only intentionally reusable mechanisms, historical evidence, and the
   minimal test harness needed to explain validated behavioral differences.
5. Profile after semantic qualification. Performance evidence may select
   encodings but may not redefine ontology or timing.

### Phase 5: promote optional resolution only by evidence

Introduce security instruments and positions, interbank contracts, enterprise
owners, insurance claims, pension entitlements, dwellings, JST units, or foreign
counterparties only through separate decisions satisfying the promotion
criteria in this RFC. They are not part of the foundational migration merely
because their schemas are anticipated.

At every phase, update canonical model, state-vector, ODD, architecture,
validation, and model-card documentation only for the implemented slice that has
become authoritative.

## Cutover Gates

The new core may replace the current engine only when all gates pass:

1. **Population reconciliation:** compiled weighted persons, households,
   enterprises, employment, activity, migration, and opening relationships meet
   baseline controls within declared tolerances.
2. **Opening balance reconciliation:** contract and holder balances reconcile to
   institutional balance sheets and the ledger without undisclosed residuals.
3. **Accounting:** all applicable ledger conservation, SFC, balance-sheet, and
   stock-flow identities pass.
4. **Determinism and replay:** identical baseline, configuration, seed, and
   requested evidence produce identical results on repeated runs.
5. **Mechanism evidence:** preserved equations and mechanisms agree on
   controlled inputs, or every intentional difference is documented and
   validated.
6. **Lifecycle integrity:** entry, exit, birth, death, migration, employment,
   origination, default, maturity, and resolution preserve stable-ID and
   relationship invariants.
7. **Multi-month stability:** the qualification profiles complete their declared
   horizons and seeds without invariant failure, numerical corruption, or
   unexplained stock drift.
8. **Scenario directionality:** accepted policy and shock scenarios preserve
   expected causal direction or carry a documented scientific explanation for
   a changed response.
9. **Evidence contract:** required manifests, aggregates, diagnostics, traces,
   and scientific exports are available with explicit schema versions.
10. **Single runtime ownership:** no production path depends on translating
    persistent state through the old core or maintaining duplicate balances.

## Decision Inventory

The semantic portions of items 1 through 9, 12, and 13 are resolved in
[ADR-0011](../adr/0011-first-target-model-ontology-and-resolution-boundaries.md);
item 14 is resolved in
[ADR-0012](../adr/0012-synthetic-workplace-for-first-target-employment.md).
Their code audit and resolution history remain in the
[ontology audit matrix](0003-model-ontology-matrix.md#decision-register).
Allocator APIs and encodings under items 1 and 2, mutation strategy under item
10, and snapshot/query design under item 11 remain downstream physical or
Research API work.

1. Exact stable-ID namespaces and whether heterogeneous parties use a typed
   tagged reference or table-specific party columns.
2. Shared table infrastructure and allocator APIs that preserve domain-specific
   schemas without becoming a generic ECS.
3. First financial families to become contract- and holder-resolved.
4. Whether private enterprise ownership is represented by explicit owner units,
   owner cohorts, ownership classes, or a mixed policy in the first baseline.
5. Minimum security-instrument granularity for government and corporate bonds.
6. Initial boundary between individual credit contracts and weighted credit
   cohorts.
7. Whether housing begins with household-to-regional-cohort tenure links or
   resolved dwelling units.
8. Which public and financial institutions remain aggregate singletons in the
   first Poland baseline.
9. Which monthly event families are persisted and which are retained only when
   explicitly requested.
10. Which high-cardinality dynamic columns use double buffering, change sets, or
    validated in-place mutation.
11. Snapshot cadence and query APIs that support scientific use without
    materializing the full object graph every month.
12. Promotion criteria for replacing an aggregate projection with a resolved
    entity, relationship, contract, or position population.
13. Whether the first target defines an ordinary domestic firm as a statistical
    enterprise and consolidated synthetic financial counterparty, excluding
    legal units, enterprise groups, establishments, and local units as
    separately represented units.
14. Whether a workplace is a first-target synthetic model unit, how it relates
    to an enterprise, and how v1 discloses entity-seat-proxy location evidence.

## Implementation Anchors

- [`FlowSimulation.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/FlowSimulation.scala)
  for the current month-boundary root and step evidence.
- [`World.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/World.scala)
  and [`WorldStateSegments.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/WorldStateSegments.scala)
  for current aggregate, institutional, market, and mechanism state.
- [`Household.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala)
  and [`FirmModel.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/firm/FirmModel.scala)
  for currently embedded units and relationships.
- [`Banking.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala)
  and [`InterbankContagion.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/InterbankContagion.scala)
  for the bank population, aggregate financial rows, and bilateral exposure
  matrix.
- [`LedgerFinancialState.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/ledger/LedgerFinancialState.scala)
  and [`AssetOwnershipContract.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/ledger/AssetOwnershipContract.scala)
  for current financial-stock ownership and supported asset families.
- [`HousingMarket.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/HousingMarket.scala)
  and [`GvcTrade.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/GvcTrade.scala)
  for current aggregate real-asset and foreign-cohort representations.
- [`engine/economics`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/)
  for same-month calculation outputs that must be classified as state,
  workspace, change set, event, or observation during migration.
