# Autonomous Economic Agents

The agents package contains every autonomous agent in the SFC-ABM model.
Each agent is an `object` with a nested `case class State` and pure functions
that transform state. No mutable fields — state transitions produce new immutable instances.

All agents that modify monetary flows participate in the SFC accounting check.
Ledger-contracted financial stocks live in `LedgerFinancialState`; agent states
carry behavioral state, operational diagnostics, and legacy unsupported metrics.

## Agents

| File | Agent | State | Key SFC identities |
|------|-------|-------|-------------------|
| `Banking.scala` | 7 Polish banks (KNF 2024) | Per-bank behavior, capital, NPL/risk buckets, LCR/NSFR inputs; deposits, loans, bonds, reserves, interbank positions are ledger projections | BankCapital, BankDeposits, BondClearing, InterbankNetting |
| `Firm.scala` | Heterogeneous firms (6 sectors) | Technology state (Traditional/Hybrid/Automated), capital stock, inventory, green capital, labor and production state; cash/debt/equity are ledger projections | BankCapital (NPL), FlowOfFunds, CorpBondStock |
| `Household.scala` | Individual households | Skill, health, MPC, employment status, housing and demographic state; savings, debt, consumer credit, and equity wealth are ledger projections | BankDeposits, ConsumerCredit |
| `Immigration.scala` | Immigrant workers | Stock, monthly inflow/outflow, remittance outflow | BankDeposits (remittance → deposit outflow), Nfa |
| `Insurance.scala` | Life + non-life sector | Premium, claim, and investment-income diagnostics; reserves and securities holdings are ledger projections | BankDeposits (premium/claims), BondClearing |
| `Jst.scala` | Local government (JST) | Revenue, spending, deficit, unsupported debt metric; cash is ledger-owned | BankDeposits (JST deposits), JstDebt |
| `Nbfi.scala` | TFI funds + NBFI credit | Origination, default, and deposit-drain diagnostics; AUM, bond/equity holdings, cash, and loan stock are ledger projections | BankDeposits (deposit drain), BondClearing (TFI bonds), NbfiCredit |
| `Nbp.scala` | National Bank of Poland | Reference rate, QE policy metrics, monthly FX operations; gov bond holdings and FX reserves are ledger-owned | BankCapital (reserve interest), Nfa (FX intervention), BondClearing (QE bonds) |
| `DepositMobility.scala` | Deposit flight (Diamond-Dybvig) | Per-bank deposit flows, health-based flight, panic contagion | BankDeposits (redistribution) |
| `EarmarkedFunds.scala` | FP, PFRON, FGŚP | Payroll-funded statutory funds, bankruptcy payouts, ALMP | GovDebt (gov subvention) |
| `EclStaging.scala` | IFRS 9 ECL provisioning | S1/S2/S3 staging, macro-driven migration, forward-looking provisions | BankCapital (provision) |
| `InterbankContagion.scala` | Interbank contagion (Lehman channel) | 7×7 bilateral exposure matrix, counterparty losses, liquidity hoarding | InterbankNetting |
| `QuasiFiscal.scala` | BGK + PFR (consolidated) | Monthly issuance and lending diagnostics; bonds, holder split, and loan portfolio are ledger projections | BondClearing (quasi-fiscal bonds) |
| `SocialSecurity.scala` | ZUS, NFZ, PPK, demographics | Contribution/pension/health flows, retirees, working-age pop; fund cash and PPK bond holdings are ledger-owned | BondClearing (PPK bonds), FusBalance, NfzBalance |

## Supporting types

| File | Kind | Description |
|------|------|-------------|
| `ContractType.scala` | Enum | `Permanent`, `Zlecenie`, `B2B` — contract-specific ZUS employer rates, FP rates, firing priority, AI vulnerability, sector mix |
| `Region.scala` | Enum | 6 NUTS-1 regions (Central, South, East, Northwest, Southwest, North) — wage multipliers, base unemployment, housing cost, population share, friction matrix, migration probabilities |
| `RegionalMigration.scala` | Module | Inter-regional household relocation: wage-gap–driven migration probability, friction-weighted target selection |
| `StateOwned.scala` | Module | SOE behavioral modifiers: dividend multiplier, firing reduction, investment multiplier, energy passthrough, per-sector SOE share (GUS) |

## How to extend

**Adding a new agent** (e.g., pension fund, development bank):
1. Create `agents/NewAgent.scala` with `object NewAgent` + `case class State`.
2. Add `zero` and `initial` factory methods.
3. Add `step(prev: State, ...)(using p: SimParams): State` for monthly logic.
4. Add state field to `World.scala`.
5. Wire `step` call into the appropriate economics stage.
6. If the agent emits monetary flows — add `FlowMechanism` entries and a `*Flows.scala`.
7. If flows affect monetary stocks — add to the SFC semantic flow projection and verify
   the relevant SFC identity passes.
