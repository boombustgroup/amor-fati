# RFC-0003 Companion: Model Ontology Audit Matrix

Status: Semantic decisions complete; recorded by [ADR-0011](../adr/0011-first-target-model-ontology-and-resolution-boundaries.md) and [ADR-0012](../adr/0012-synthetic-workplace-for-first-target-employment.md)
Audit base: `main` at `9dce4256`
Owning RFC: [RFC-0003](0003-model-ontology-and-state-architecture.md)

## Purpose

This document maps the current codebase to the proposed model ontology. It is
not a physical table schema and does not claim that target concepts are
implemented. Its job is to make every important semantic conflation, ownership
boundary, and first-resolution choice visible before the Research API names the
model's public concepts or the target core fixes a storage layout.

Package names are evidence, not ontology. In particular, `agents` currently
contains economic units, institutional aggregates, mechanisms, classifications,
and diagnostics. A Scala `State` class is not automatically an institutional
unit, and a ledger sector or runtime settlement shell is not automatically an
economic owner.

Resolution labels used below:

| Label | Meaning |
| --- | --- |
| Observed | Description of current code, not a target decision. |
| Target invariant | Already required by an accepted ADR or an unambiguous model invariant. |
| ADR-aligned detail | Detailed target mapping consistent with the accepted ontology ADRs; it may evolve in its owning downstream gate without changing the accepted semantic boundary. |
| Owner decision | Economic or scientific choice that requires explicit acceptance. |
| Owner accepted | Explicitly chosen for the RFC target and canonicalized by an accepted ADR. |
| RFC resolved | Technical or semantic choice fixed from code evidence and accepted constraints, then canonicalized by an accepted ADR. |
| Deferred | Valid ontology family whose higher resolution is outside the first target. |

## Runtime Ownership Audit

| Current root | Ontology class | Current authority | Target interpretation | Resolution |
| --- | --- | --- | --- | --- |
| `FlowSimulation.SimState` | Month-boundary state root | Owns current `World`, firm, household, bank, aggregate-household, and ledger snapshots. | One published boundary composed of typed state stores; not an economic unit. | Target invariant |
| `World` | Aggregate and institutional state root | Owns policy, market, institutional, lagged-signal, and cached flow state. | Narrow state handles grouped by semantic ownership and lifetime; not one agent. | ADR-aligned detail |
| `LedgerFinancialState` | Financial-stock owner | Authoritative for the currently supported owner-by-asset balance slice. | Persistent ledger projection remains the only owner of supported monetary principal and balances. Contracts and positions reference its balance keys. | Target invariant |
| `PipelineState` and `DecisionSignals` | Lagged behavioral information | Own inherited pre-month signals used by month decisions. | Explicit `T - 1` decision-signal surface, separate from same-month operations and closing outcomes. | Target invariant |
| `OperationalSignals` | Same-month information | Transient surface produced and consumed inside month `T`. | Ordered same-month signal view; never persistent behavioral memory by accident. | Target invariant |
| `FlowState` | Derived monthly observations and compatibility memory | Stored under `World` although most fields are current-month aggregates, diagnostics, or projections. | Split into requested observations, mandatory reconciliation evidence, and the few explicitly lagged signals. | ADR-aligned detail |
| `MonthExecution` and economics `StepOutput` values | Monthly workspace/transcript | Carry same-month calculations and proposed closing values. | Transient workspace, commands, change sets, and observations; never a second persistent owner. | Target invariant |
| `MonthlyCalculus`, `BatchedFlow`, runtime ledger deltas | Flow plan, accounting commands, executed evidence | Production monetary execution path. | Typed commands executed through the ledger; successful execution produces flow evidence, not an additional closing-stock owner. | Target invariant |
| `MonthTrace`, snapshots, diagnostics | Evidence | Derived from coherent boundaries and executed month data. | Versioned, cadence-controlled evidence governed by the Research API. | ADR-aligned detail |

## Economic Units and Institutional Sectors

| Family | Current representation | Target ontology | First-target mode | Main gap | Resolution |
| --- | --- | --- | --- | --- | --- |
| Person | No separate unit. Labor status, wage, education, skill, immigration, region, and distress sit on `Household.State`. | Demographic, residency, education, labor-supply, and pension-eligibility unit with `PersonId`. | Weighted synthetic unit | Person and household meaning are conflated. | ADR-aligned detail |
| Household | `Household.State` is described as an individual household but behaves partly as one worker plus dependent-child count. | Co-resident consumption, pooled-finance, housing, and retail-credit unit with `HouseholdId`. | Weighted synthetic unit | Membership and household composition are absent. | ADR-aligned detail |
| Enterprise | `FirmState`; currently one operating firm row with sector, region, technology, capital, and networks. | Eurostat-aligned statistical enterprise and consolidated synthetic financial counterparty. Legal units, groups, establishments, and local units remain unresolved levels. | Weighted synthetic unit; declared systemic enterprises may use weight one | The current statistical universe, consolidation boundary, and weight are implicit. | Owner accepted |
| Workplace | Absent; employment embeds `firmId` on the household row. | Synthetic labor and production locus with `WorkplaceId`, operating `EnterpriseId`, sector, modeled location, operational status, and baseline location evidence. | Exactly one workplace per ordinary enterprise, with the same represented quantity | No separate site or establishment model exists; a workplace must not be read as an empirical worksite. | Owner accepted |
| Commercial bank | Ten named/archetype `BankState` rows plus aligned ledger balances. | Deposit-taking institutional unit with stable `BankId`. | Named unit, weight one | `BankId` is also a vector index; stable identity and row position are conflated. | Owner accepted |
| NBP | One `Nbp.State` plus ledger-owned bond, FX-asset, and reserve-liability balances. | Central-bank institutional unit. | Named aggregate singleton, weight one | Policy state and financial positions are split correctly but lack a shared stable unit identity. | Owner accepted |
| Central government | One `FiscalBudget.GovState` plus government ledger balances. | Central-government institutional unit with explicit consolidation policy. | Named aggregate singleton | Fiscal-rule debt metric and holder-resolved bond principal require a declared reconciliation relation. | Owner accepted |
| Local government | One consolidated `Jst.State` plus ledger-owned JST cash. | Consolidated local-government subsector; individual JST units require later promotion. | Aggregate singleton | No individual JST identities; current debt is unsupported institutional accounting state. | Owner accepted |
| ZUS/FUS | Aggregate contribution and pension flows plus ledger cash. | Social-security institutional unit/fund. | Named aggregate singleton | Beneficiaries and entitlements are not resolved. | Owner accepted |
| NFZ | Aggregate contribution and health-spending flows plus ledger cash. | Healthcare-financing institutional unit. | Named aggregate singleton | Providers, insured persons, and contracts are not resolved. | Owner accepted |
| PPK | Aggregate contribution flow plus fund bond holdings. | Pension/retirement-savings scheme or fund. | Named aggregate singleton | Individual entitlements and fund units are not resolved. | Owner accepted |
| FP, PFRON, FGSP | Three named flow buckets plus separate ledger cash slots. | Named earmarked public funds. | Named aggregate singletons | Beneficiaries and obligations are aggregate. | Owner accepted |
| BFG | `MechanismsState.bfgFundBalance` plus bank-resolution flows and diagnostics; the runtime levy currently settles to the Treasury account. | Deposit-guarantee and resolution institution/fund whose persistent cash balance is authoritatively owned by a dedicated BFG slot in `LedgerFinancialState`. | Named aggregate singleton | Reconcile closing BFG cash to opening cash plus executed `BankBfgLevy` inflows minus executed guarantee or resolution outflows; outflows are zero until such flows exist. `MechanismsState.bfgFundBalance` is a migration-only projection, and the Treasury-routed levy must move to the BFG account. | Owner accepted |
| Insurer | One consolidated life/non-life sector with flow diagnostics and ledger positions. | Insurance corporations; policies and claims are separate contract families. | One aggregate insurance sector | No insurer units, policies, beneficiaries, or claims. | Owner accepted |
| TFI and other NBFI | One combined NBFI/fund bucket with monthly flow diagnostics and ledger positions. | Investment-fund and non-bank-credit institutional families. | Two distinct aggregate institutions: TFI/investment funds and credit NBFI | Fund management, fund units, and lender identity are conflated. | Owner accepted |
| Quasi-fiscal vehicles | BGK/PFR-like activity is consolidated in one state and ledger balance family. | BGK and PFR as distinct named public financial/quasi-fiscal units. | Two named low-cardinality singletons | BGK and PFR are not distinguished and consolidation with government is scenario-dependent. | Owner accepted |
| Rest of the world | Aggregate BoP/foreign holder plus GVC partner-sector proxies. | Foreign institutional sectors, counterpart cohorts, investors, and visitors. | Aggregate sector plus weighted partner-sector cohorts | Runtime proxies can be mistaken for foreign enterprises. | ADR-aligned detail |
| `GvcTrade.ForeignFirm` | Twelve sector-by-partner proxy rows. | Foreign trade-partner cohort, not a named or synthetic enterprise. | Weighted cohort | Class name overstates ontological resolution. | ADR-aligned detail |
| Visitor | No unit; inbound and outbound tourism are aggregate flows. | Non-resident visitor cohort only when composition or capacity affects behavior. | Not represented; aggregate flows only | No gap for the first target if omission remains explicit. | Deferred |

## Relationships and Networks

| Family | Current encoding | Target semantic contract | First-target representation | Resolution |
| --- | --- | --- | --- | --- |
| Household membership | Absent; dependent children are a count on `Household.State`. | Person-to-household relation with role, interval, and represented quantity. | Dedicated indexed relation | ADR-aligned detail |
| Employment | `HhStatus.Employed(firmId, sectorIdx, wage)` embeds employer, sector, and wage on the household row. | Person/labor-cohort to workplace relation with contract, job quantity, wage terms, and lifecycle; the operating enterprise is resolved through the workplace. | Dedicated relation; sector derived from workplace unless explicitly job-specific | ADR-aligned detail |
| Labor inactivity and retirement | Retirees and working-age population are aggregate `DemographicsState`; no person status. | Person activity state and pension relation; aggregate counts are derived. | Person columns plus optional entitlement relation | ADR-aligned detail |
| Residency and migration | `isImmigrant`, household region, and aggregate immigrant stock/flows. | Person-to-geography residence episode with usual-residence semantics and interval. | Person state plus migration events | ADR-aligned detail |
| Household dependency | `numDependentChildren` count. | Membership roles and age determine dependent status; fiscal eligibility is a derived rule. | Derived from membership/person state | ADR-aligned detail |
| Deposit service | `bankId` on household and firm rows; balances live separately. | Holder-to-bank deposit-account contract with product, currency, status, terms, and ledger balance key. | One weighted represented demand-deposit account per holder-bank-product initially | Owner accepted |
| Household distress-peer network | `socialNeighbors: Array[HhId]` drives distress-contagion exposure between current household rows. | Household-to-household peer relation used by the distress mechanism; it is not person-level social contact. | Compact adjacency store | ADR-aligned detail |
| Enterprise technology-peer network | `neighbors: Vector[FirmId]` drives the technology-adoption demonstration effect. | Enterprise-to-enterprise technology-peer relation; a future production or supply network is a distinct relationship family. | Compact adjacency store | ADR-aligned detail |
| Enterprise ownership/control | Independently assigned `foreignOwned` and `stateOwned` Booleans can overlap; FDI acquisition flips only `foreignOwned`. | Owner-to-enterprise relation or declared ownership/control class with shares, interval, and a validated singular control classification. | Mixed policy accepted in Decision O-04 | Owner accepted |
| Housing tenure | Rent and aggregate housing wealth exist, but no household-to-asset relation. | Household-to-regional-housing-cohort tenure and collateral relation. | Regional cohort link; no dwelling identity | Owner accepted |
| Interbank exposure | Dense matrix reconstructed from current net positions during banking execution. | Bilateral lender-bank to borrower-bank exposure with amount, maturity, and recovery/seniority class. | Small dense relation keyed by stable bank IDs | Owner accepted |

## Financial Contracts, Instruments, and Positions

| Family | Current authoritative state | Target semantic contract | First-target resolution | Resolution |
| --- | --- | --- | --- | --- |
| Demand deposit | Household balances, firm cash, and bank liability rows in `LedgerFinancialState`; `bankId` supplies routing. | Deposit account identifies holder, bank, product, status, represented quantity, and balance key. | One weighted represented account per holder-bank-product | Owner accepted |
| Term deposit | Aggregate bank liability only. | Deposit account with term/liquidity semantics and holder position. | Aggregate residual until holder allocation is empirically supported | ADR-aligned detail |
| Mortgage | Household principal and maturity plus aggregate bank mirror and housing-market projection. | Secured credit contract with borrower, lender, terms, represented quantity, collateral reference/pool, performance state, and ledger keys. | One weighted represented facility per borrower-lender-product | Owner accepted |
| Consumer credit | Household principal and bank aggregate asset; distress/default state sits on household and bank aggregates. | Unsecured credit contract with borrower, lender, represented quantity, origination, maturity, rate, stage, and status. | One weighted represented facility per borrower-lender-product | Owner accepted |
| Firm bank credit | Firm liability and bank aggregate assets; firm routing uses `bankId`. | Enterprise credit contract with lender, represented quantity, terms, purpose/product, stage, maturity, and ledger keys. | One weighted represented facility per borrower-lender-product | Owner accepted |
| NBFI credit | Aggregate fund loan stock and monthly flows. | Non-bank lender-to-borrower credit contract or declared weighted loan cohort. | Weighted credit cohort | ADR-aligned detail |
| Interbank credit | Net balance per bank plus reconstructed bilateral matrix. | Bilateral interbank instrument/exposure. | Resolved for the small named bank population | Owner accepted |
| Standing facilities | Delta-only NBP backstop channel; public ledger asset has no persisted engine contract. | Central-bank lending or deposit facility with explicit counterparty and terms if a balance survives closing. | Executed monthly flow only while no stock survives the boundary | ADR-aligned detail |
| Government bonds | Issuer outstanding stock and holder buckets in the ledger; fiscal-rule debt remains separate. | Government security instrument cohort plus issuer and holder positions. | Instrument cohorts by issuer, currency, coupon/indexation, and maturity; AFS/HTM belongs to holder positions; no ISIN book | Owner accepted |
| Corporate bonds | Firm issuance totals and holder buckets; market price/yield memory is separate. | Corporate security instrument cohort plus issuer and holder positions. | Preserved issuer or issuer-class cohorts by currency, maturity, seniority, and risk | Owner accepted |
| Quasi-fiscal bonds | Aggregate issuer stock with bank/NBP holder split. | Quasi-fiscal security instrument cohorts and holder positions. | Separate BGK/PFR issuer cohorts at the supported term resolution | Owner accepted |
| Listed equity | Firm issued balances, household, foreign, insurance, and NBFI holder balances, plus aggregate market capitalization. | Equity instrument/position family reconciled to issuer shares and valuation state. | Preserved systemic issuers or issuer cohorts with holder positions; no shareholder cap table | Owner accepted |
| Private enterprise ownership | Control flags only; no transferable position. | Ownership/control relation distinct from listed equity and accounting net worth. | Domestic-private ownership class without a private cap table; explicit public and foreign owner cohorts where causal | Owner accepted |
| Bank capital | `BankState.capital`. | Regulatory/accounting institutional state used for loss absorption and prudential ratios. | Per named bank; not holder-resolved equity | ADR-aligned detail |
| Bank equity ownership | Not represented. | Optional bank-equity instrument and holder positions reconciled to, but distinct from, regulatory capital. | Not represented | Deferred |
| Insurance technical reserves | Insurance liabilities and household mirror assets in ledger rows. | Reserve liability plus policyholder positions; policy contracts only when resolved. | Aggregate reserve with weighted household projection | ADR-aligned detail |
| TFI fund units | Aggregate issued units and fund assets. | Fund-unit instrument and holder positions. | Aggregate fund instrument; household holders unresolved initially | ADR-aligned detail |
| PPK entitlement | Contributions and aggregate bond holdings only. | Pension/fund entitlement relation and optional fund-unit position. | Aggregate entitlement projection | Deferred |
| Bank reserves at NBP | Bank reserve assets and NBP reserve liability. | Central-bank account/balance relation. | One account per named bank | Owner accepted |
| NBP foreign assets | NBP ledger asset plus aggregate BoP reserve metrics. | Central-bank foreign-asset position reconciled to external accounts. | Aggregate position | ADR-aligned detail |

This table covers every current public ledger family: `DemandDeposit`,
`TermDeposit`, `FirmLoan`, `ConsumerLoan`, `MortgageLoan`, `GovBondAFS`,
`GovBondHTM`, `QuasiFiscalBond`, `CorpBond`, `Reserve`, `StandingFacility`,
`InterbankLoan`, `Equity`, `LifeReserve`, `NonLifeReserve`, `TfiUnit`,
`NbfiLoan`, `Cash`, `Capital`, and `ForeignAsset`. Some identifiers are grouped
into one semantic family above; this coverage statement does not imply that
each current identifier is already a correct or independently persisted target
contract.

## Real Assets and Non-financial Stocks

| Family | Current representation | Target semantic contract | First-target resolution | Resolution |
| --- | --- | --- | --- | --- |
| Physical capital | Monetary-valued `capitalStock` on each firm. | Enterprise-owned productive-capital stock; asset rows only when vintage/unit heterogeneity is causal. | Enterprise column | ADR-aligned detail |
| Inventory | Monetary-valued enterprise stock plus derived aggregate cache. | Enterprise-owned inventory stock; aggregate is observation only. | Enterprise column | ADR-aligned detail |
| Green capital | Enterprise stock plus aggregate cache. | Enterprise productive-capital subtype or tagged stock. | Enterprise column | ADR-aligned detail |
| Technology state | `TechState` plus readiness/routineness and transition diagnostics. | Enterprise production regime and technology attributes, not automatically a financial asset. | Enterprise columns/classification | ADR-aligned detail |
| Housing | Aggregate national/regional value, price, wealth, and mortgage projections. | Regional housing cohorts linked to household tenure and collateral. | Regional housing cohorts; no individual dwellings | Owner accepted |
| Public capital | Aggregate central-government policy stock. | General-government non-financial asset stock with declared consolidation. | Aggregate singleton stock | ADR-aligned detail |
| Foreign productive assets | Not separately resolved; NFA/FDI are macro positions and flows. | Optional foreign/direct-investment asset or ownership positions. | Aggregate external position | Deferred |

## Market, Policy, and Aggregate State

| Family | Ontology classification | Current examples | Target rule | Resolution |
| --- | --- | --- | --- | --- |
| Prices and indices | Persistent aggregate market state | CPI price level, equity index, housing price indices, exchange rate, commodity and import-cost indices. | Named typed market measures; never asset owners. | ADR-aligned detail |
| Yield and rate curves | Persistent market/policy state | NBP reference rate, interbank rate, WIBOR curve, bond yields, mortgage rate. | Typed rate surfaces with timing and source semantics. | ADR-aligned detail |
| Expectations | Persistent behavioral/macro state | Expected inflation/rate, credibility, forecast error, guidance rate. | Aggregate expectation state with explicit lag timing. | ADR-aligned detail |
| Macroprudential policy | Persistent policy/risk state | CCyB, credit-to-GDP gap/trend, O-SII/P2R configuration. | Policy state and bank-specific requirements, not financial positions. | ADR-aligned detail |
| Fiscal budget | Institutional accounting plus monthly flows | Revenue, spending, deficit, fiscal-rule debt, public capital. | Separate persistent financial/policy state from period flows and holder-resolved instruments. | ADR-aligned detail |
| Balance of payments | Aggregate external accounting state and period flows | NFA, current/capital accounts, FDI, portfolio flows, reserves, trade. | Explicit stock/flow classification and reconciliation to resolved NBP/foreign positions where applicable. | ADR-aligned detail |
| GVC | Foreign partner-cohort state and trade flows | Partner-sector demand, supply, prices, disruption. | Cohort model; not a foreign-enterprise registry. | ADR-aligned detail |
| Labor, goods, housing, securities markets | Clearing mechanisms plus limited persistent state | Wage results, demand signals, price/yield memory, regional clearing. | A market becomes an entity only if it owns assets or contracts; otherwise state is price/rule/memory. | Target invariant |
| Household and regional wage surfaces | Persistent aggregate market state | Market wage, reservation wage, and `regionalWages`. | Typed labor-market measures with declared month timing; not independent population state. | ADR-aligned detail |
| Sector production signals | Persistent mechanism/aggregate state | `currentSigmas`, productivity index, automation/hybrid ratios, demand and hiring signals. | Separate structural parameters, lagged decision signals, and derived observations by lifetime. | ADR-aligned detail |
| Sectoral mobility | Current-month observation stored in `RealState` | Cross-sector hires, quits, mobility rate. | Observation unless an explicit lagged behavioral variable is declared. | ADR-aligned detail |
| Informal-economy memory | Smoothed shares under `MechanismsState`. | Explicit lagged behavioral/policy signal with provenance and timing. | Persistent aggregate signal | ADR-aligned detail |
| Monetary plumbing | Monthly reserve interest, standing-facility, interbank-interest, and flow-of-funds values under `World`. | Executed-flow evidence and reconciliation observations unless an amount survives closing. | Monthly evidence, not independent financial stocks | ADR-aligned detail |

## Classifications and Reference Catalogs

| Catalog family | Current encoding | Target contract | Resolution |
| --- | --- | --- | --- |
| Entity and relationship kinds | Implied by Scala types, packages, and ledger sectors. | Versioned ontology catalog with stable external identifiers. | ADR-aligned detail |
| Production sector | `SectorIdx` plus ordered `SimParams.sectorDefs`. | Versioned industry-to-model-sector catalog and crosswalk; row order is not semantic identity. | ADR-aligned detail |
| Geography and residency | `Region` enum, housing-region enum, partner IDs, and Boolean immigration flag. | Versioned geography, territorial-coverage, usual-residence, and partner catalogs. | ADR-aligned detail |
| Labor activity and employment contract | `HhStatus`, `ContractType`, and integer education levels. | Separate versioned activity, contract, education, and program-status catalogs. | ADR-aligned detail |
| Enterprise size, legal form, and ownership | `initialSize`, size configuration, and ownership Booleans. | Stable size, legal-form, listing, ownership, and control classifications. | Owner accepted for ownership/control; remaining catalog encoding is physical design |
| Account, credit, security, insurance, and collateral products | Enums and mechanism-specific configuration distributed across packages. | Versioned product catalogs referenced by contracts and instruments. | ADR-aligned detail |
| Financial accounting attributes | Asset enum plus mechanism-specific AFS/HTM, maturity, stage, and recovery fields. | Versioned currency, valuation, maturity, seniority, staging, and accounting catalogs. | ADR-aligned detail |
| Representation and consolidation | Not encoded as one reference catalog. | Versioned representation mode, weight unit, aggregation, and institutional-consolidation catalog. | ADR-aligned detail |

## Flows, Events, and Evidence

| Family | Current encoding | Target semantics | Retention rule | Resolution |
| --- | --- | --- | --- | --- |
| Monetary flows | `BatchedFlow` execution plus semantic SFC projection. | Executed period activity with mechanism, parties, asset, amount, and month. | Accounting evidence retained according to the run evidence policy; canonical aggregates and hashes are mandatory. | Target invariant |
| Origination, repayment, amortization, default, write-off | Mostly aggregate fields and per-entity result values. | Contract lifecycle events that update contract state and ledger principal atomically. | Typed month events; detailed persistence is evidence-policy controlled. | ADR-aligned detail |
| Birth, death, entry, exit, migration, employment transition | Direct row replacement, counts, and diagnostics. | Entity/relationship lifecycle events applied at declared boundaries. | Boundary changes are mandatory state; event detail may be sampled or requested. | ADR-aligned detail |
| Bank failure and resolution | Bank status changes plus diagnostics and monetary flows. | Institutional lifecycle event with loss allocation, resolution actions, and counterparty effects. | Mandatory auditable event summary and reconciliation evidence. | ADR-aligned detail |
| `FlowState` and aggregate step totals | Persisted current-month caches under `World`. | Observations or compatibility projections unless explicitly promoted to lagged state. | Derived on demand or retained at declared cadence. | ADR-aligned detail |
| Firm decisions and household snapshots | Optional traces/snapshot payloads in `StepOutput`. | Bounded observations of authoritative state and decisions. | Opt-in selectors and cadence; never unconditional full-history retention. | ADR-aligned detail |
| `MonthTrace` | Boundary, timing, random contract, flows, validations. | Core replay and causal-timing evidence. | Mandatory compact core; extensible envelopes follow evidence policy. | ADR-aligned detail |

## Current Semantic Conflations

The audit found the following model-level conflations that the target ontology
must not preserve as public concepts:

1. `Household.State` is simultaneously a worker/person, a household financial
   decision-maker, a social-network node, and a dependent-child container.
2. Employment is an activity-state enum payload instead of an independently
   identifiable relationship with terms and lifecycle.
3. `bankId` combines a service relationship, routing choice, and vector index.
4. Firm ownership is reduced to independently assigned `foreignOwned` and
   `stateOwned` flags that may both be true without shares or a control rule,
   while listed equity balances are a different, only partly reconciled
   surface.
5. Household mortgages are partly resolved by household, settled through an
   aggregate housing model, and mirrored back to banks by allocation rather
   than lender-specific contracts.
6. Consumer and firm credit have borrower balances and bank aggregates but no
   persistent contract identity or complete bilateral terms.
7. The interbank matrix is reconstructed from net bank positions rather than
   carried as a persistent bilateral exposure relation.
8. Retirees, working-age population, and immigrant stock can evolve as
   aggregates alongside household rows instead of being derived from persons.
9. BFG fund capital is a persistent monetary balance under mechanism state
   rather than an explicitly owned institutional ledger balance.
10. `FlowState` mixes observations, diagnostics, compatibility caches, and
   stock-like aggregate projections under the persistent `World` root.
11. Names such as `ForeignFirm`, `Insurance.State`, and `Nbfi.State` can suggest
    individual economic units where the implementation is an aggregate cohort
    or monthly diagnostic surface.

## Decision Register

These decisions are the smallest set needed to close the semantic gate. Storage
encoding and notebook ergonomics are deliberately excluded.

| ID | Decision | Recommended first-target choice | RFC-0003 open decision | State |
| --- | --- | --- | --- | --- |
| O-01 | Party identity | Table-specific stable typed IDs; a tagged `PartyRef` only at genuinely heterogeneous relationship/contract boundaries. Dense row indices remain private runtime locators, not identities. | 1 | RFC resolved; allocator encoding deferred |
| O-02 | Shared runtime infrastructure | Shared allocator/column primitives with domain-specific schemas; no generic ECS entity/component API. | 2 | Covered by ADR-0006; API detail deferred |
| O-03 | First resolved financial families | Demand-deposit accounts, mortgages, consumer credit, firm credit, bank reserve accounts, and bilateral interbank exposure. Term deposits remain an aggregate residual; security positions start as cohorts. | 3 | Owner accepted, 2026-07-20 |
| O-04 | Ordinary enterprise ownership | Mixed policy: explicit public/foreign owner cohorts where causal, declared domestic-private ownership class otherwise; no private cap table. Mixed stakes are allowed, but the derived control class is singular and validated. | 4 | Owner accepted, 2026-07-20 |
| O-05 | Security granularity | Instrument cohorts by issuer or issuer class, currency, coupon/indexation, maturity, seniority, and risk where relevant, with separate holder positions and position-level accounting/valuation class; no security-by-security ISIN book initially. Systemic listed issuers may be preserved at weight one. | 5 | Owner accepted, 2026-07-20 |
| O-06 | Credit granularity | One weighted represented facility per borrower-lender-product where borrower identity matters; aggregate cohorts only for unresolved NBFI/residual books. | 6 | Owner accepted, 2026-07-20 |
| O-07 | Housing resolution | Household-to-regional-housing-cohort tenure/collateral links; no dwelling population in the first target. | 7 | Owner accepted, 2026-07-20 |
| O-08 | Aggregate institutions | Named banks, NBP, central government, consolidated JST, ZUS/FUS, NFZ, PPK, FP, PFRON, FGSP, and BFG remain named or aggregate singletons. Insurance remains one aggregate sector; TFI and credit NBFI are distinct aggregate institutions; BGK and PFR are distinct named units. | 8 | Owner accepted, 2026-07-20 |
| O-09 | Event persistence | Always apply typed lifecycle events atomically and retain mandatory audit aggregates; persist detailed event rows only when requested by evidence policy. | 9 | RFC resolved |
| O-10 | Mutation strategy | Decide double buffering, change sets, and validated in-place updates after semantic schemas and access patterns are accepted. | 10 | Deferred physical design |
| O-11 | Snapshot/query cadence | Research API decision after the semantic vocabulary and authoritative sources are fixed. | 11 | Deferred to RFC-0002 |
| O-12 | Resolution promotion | Require a research question, empirical controls, reconciliation, lifecycle semantics, and validation evidence before promoting an aggregate family. | 12 | RFC resolved |
| O-13 | Domestic firm statistical unit | A row represents a Eurostat-aligned statistical enterprise and consolidated synthetic financial counterparty. Legal units, enterprise groups, establishments, and local units are not separate first-target units. | 13 | Owner accepted, 2026-07-20 |
| O-14 | Synthetic workplace | `Workplace` is a synthetic model unit. In v1 it is one-to-one with an enterprise, has the same represented quantity, and exposes modelled location with entity-seat-proxy provenance; it is not an empirical establishment or worksite. | 14 | Owner accepted, 2026-07-24 |

### Accepted Resolution for O-03 and O-06

The first target uses an explicit bilateral relationship for each resolved
holder/borrower, bank, and product combination. The relationship carries a
represented quantity where its coverage differs from the entity's own weight.
Per-unit values may drive underwriting and behavior, while the authoritative
ledger balance is the checked fixed-point extensive amount represented by the
relationship.

This is an economic facility or account cohort, not a reconstruction of every
empirical agreement, tranche, or payment schedule. Economically material
heterogeneity must be preserved by population stratification, a distinct
product/cohort classification, or an additional represented relationship; it
must not be hidden in an unsupported average. The contract owns counterparties
and terms and references the ledger balance key, while the ledger remains the
only owner of monetary principal.

The first resolved families are demand-deposit accounts, household mortgages,
household consumer credit, enterprise bank credit, named-bank reserve accounts
at NBP, and bilateral interbank exposure. Term deposits, unresolved NBFI loan
books, and security positions remain declared aggregate residuals or cohorts
until evidence justifies greater resolution.

### Accepted Resolution for O-04

Ordinary domestic-private enterprises carry a declared ownership/control class
without resolving their private owners or cap tables. Publicly controlled
enterprises link to a declared public owner or owner cohort;
foreign-controlled enterprises link to a rest-of-world owner cohort. These
relationships carry shares and effective intervals where represented.

Multiple ownership stakes may coexist, but a declared control rule must derive
one validated control classification for behavior and reporting. Listed equity
instruments and holder positions remain separate and must reconcile with the
control classification at their declared resolution; their first-target
granularity remains Decision O-05.

### Accepted Resolution for O-05

The first target uses security instrument cohorts rather than reconstructing
every empirical ISIN. Cohorts preserve issuer or issuer class, currency, coupon
or indexation, maturity, seniority, and risk where these affect behavior.
Holder positions separately carry quantity or represented holding, valuation,
and accounting classification; AFS and HTM are position attributes.

Government, corporate, and quasi-fiscal issuance remains distinguishable at the
accepted cohort resolution, including separate BGK and PFR issuers. Listed
equity uses preserved systemic issuers where declared by the baseline and
issuer cohorts otherwise. Holder positions reconcile to issued amounts and
market valuation, but no individual shareholder register is created.

### Accepted Resolution for O-07

The first target creates regional housing cohorts and explicit household tenure
and collateral relations. A relation carries the represented quantity or value
needed to connect housing status, wealth, costs, and mortgage collateral. It
does not identify an individual dwelling. Dwelling identities require later
promotion under O-12 when transaction, construction, mobility, or collateral
evidence makes them behaviorally necessary.

### Accepted Resolution for O-08

Commercial banks remain named units with weight one. NBP, central government,
ZUS/FUS, NFZ, PPK, FP, PFRON, FGSP, and BFG are named aggregate singletons;
JST remains one consolidated local-government sector. Insurance remains one
aggregate sector, while TFI/investment funds and credit NBFIs are two distinct
aggregate institutions. BGK and PFR are distinct named low-cardinality units
with an explicit consolidation policy for each accounting view.

These choices define institutional identity, not a requirement that every
institution already has a complete standalone behavioral implementation.
Greater resolution requires promotion under O-12.

### Accepted Resolution for O-13

`Enterprise` follows the Eurostat statistical-unit concept: an autonomous
producer may comprise one or more legal units and operate at more than one
location. In Amor Fati the same synthetic unit is the first-target consolidated
financial counterparty for ownership of productive assets, financial positions,
banking contracts, securities issuance, distress, entry, and exit.

An ordinary row may represent multiple empirical enterprises in a controlled
stratum; explicitly preserved systemic enterprises may use weight one. Legal
units, enterprise groups, establishments, and local units are distinct ontology
levels but are not separately represented in the first target. The reference
economy must declare its statistical-universe crosswalk and consolidation rule.
Regional production, site-level employment, or capacity questions may later
promote establishments or local units under O-12.

### RFC Resolutions for O-01, O-09, and O-12

Every persistent unit, relationship, contract, instrument, and position uses a
stable family-specific typed ID. Dense row indices are private runtime locators.
A tagged `PartyRef` is used only where one column must intentionally address
several party families; exact allocator and slot-reuse encoding belongs to the
physical design.

Lifecycle events are applied atomically whether or not their detailed rows are
retained. Mandatory audit aggregates and reconciliation evidence always
survive; detailed events follow the run evidence policy. An aggregate family is
promoted to resolved units or contracts only when a research question,
empirical controls, reconciliation, lifecycle semantics, and validation
evidence justify the additional resolution.

### Accepted Resolution for O-14

`Workplace` is a first-target synthetic labor and production locus, not a
legal establishment, local unit, registered seat, or observed physical
worksite. It is created and retired with its operating enterprise in v1, and
the one-to-one link carries the same represented quantity. A person or labor
cohort's primary employment relation targets this workplace; firm-level
financial and ownership effects continue to target the enterprise. The
baseline records how the workplace location was initialized, including the
`entity-seat proxy` used by `PL-2025-Q4-v1`.

Multiple workplaces per enterprise, actual-workplace evidence, commuting, and
site-level capacity require resolution promotion under O-12.

## Semantic-Ready Gate

Decision status: complete and canonicalized by
[ADR-0011](../adr/0011-first-target-model-ontology-and-resolution-boundaries.md)
and [ADR-0012](../adr/0012-synthetic-workplace-for-first-target-employment.md).

The ontology is ready for a Research API decision when:

1. O-01 through O-09 and O-12 through O-14 are owner-accepted, RFC-resolved,
   covered by an accepted ADR, or explicitly revised as appropriate.
2. Every first-target unit and relationship has a representation mode and stable
   identity policy.
3. Every persistent monetary stock has one authoritative owner and any
   aggregate mirror is labeled as a projection.
4. Every current root-state family maps to persistent state, lagged signal,
   workspace, command/change set, flow/event, or observation/evidence.
5. Aggregate institutions and omitted resolution are explicit rather than
   implied by class names.
6. The accepted semantic decisions are captured in a durable ADR before the
   Research API publishes selectors and result concepts.

O-10 and O-11 do not block this semantic gate. They are downstream physical
storage and research-access decisions.

## Code Coverage

The audit is anchored in these current implementation surfaces:

- [`FlowSimulation.SimState`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/FlowSimulation.scala),
  [`World`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/World.scala),
  and [`WorldStateSegments`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/WorldStateSegments.scala);
- [`Household.State`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala),
  [`FirmState`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/firm/FirmModel.scala),
  and [`Banking.BankState`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala);
- the aggregate institutional states under
  [`agents`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/README.md);
- [`LedgerFinancialState`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/ledger/LedgerFinancialState.scala)
  and [`AssetOwnershipContract`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/ledger/AssetOwnershipContract.scala);
- market and mechanism state under
  [`engine/markets`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/)
  and
  [`engine/mechanisms`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/);
- [`MonthExecution`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthExecution.scala),
  [`MonthlyCalculus`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/MonthlyCalculus.scala),
  [`FlowState`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/FlowState.scala),
  and [`MonthTrace`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthTrace.scala).

This is a family-level semantic audit. Field-level physical column schemas are a
later deliverable after the semantic-ready gate.
