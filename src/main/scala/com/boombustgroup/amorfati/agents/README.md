# Autonomous Economic Agents

The agents package contains every autonomous agent in the SFC-ABM model.
Each agent is an `object` with a nested `case class State`and pure functions 
that transform state. No mutable fields — state transitions produce new immutable instances.

All agents that modify monetary stocks participate in the 14-identity
SFC accounting check (see `com.boombustgroup.amorfati.accounting.Sfc`).

## Agents

| File | Agent | State | Key flows (SFC) |
|------|-------|-------|-----------------|
| `Banking.scala` | 7 Polish banks (KNF 2024) | Per-bank: deposits, loans, capital, NPL, gov bonds, interbank, LCR/NSFR | Identity 1 (bank capital), 2 (deposits), 5 (bond clearing), 6 (interbank netting) |
| `Firm.scala` | Heterogeneous firms (6 sectors) | Cash, debt, tech state (Traditional/Hybrid/Automated), capital stock, inventory, green capital | Identity 1 (NPL → bank capital), 10 (flow-of-funds), 12 (corp bonds) |
| `Household.scala` | Individual households | Savings, debt, skill, health, MPC, employment status, consumer credit, equity wealth | Identity 2 (deposits), 11 (consumer credit) |
| `Immigration.scala` | Immigrant workers | Stock, monthly inflow/outflow, remittance outflow | Identity 2 (remittance → deposit outflow), 4 (NFA) |
| `Insurance.scala` | Life + non-life sector | Reserves, gov/corp bond holdings, equity holdings | Identity 2 (premium/claims → deposits), 5 (bond clearing) |
| `Jst.scala` | Local government (JST) | Deposits, debt, revenue, spending | Identity 2 (JST deposits), 7 (JST debt) |
| `Nbfi.scala` | TFI funds + NBFI credit | AUM, bond/equity holdings, loan stock | Identity 2 (deposit drain), 5 (TFI bonds), 13 (NBFI credit) |
| `Nbp.scala` | National Bank of Poland | Reference rate, gov bond holdings, QE, FX reserves | Identity 1 (reserve interest), 4 (FX intervention → NFA), 5 (QE bonds) |
| `SocialSecurity.scala` | ZUS, PPK, demographics | FUS balance, PPK bond holdings, retirees, working-age pop | Identity 5 (PPK bonds), 8 (FUS balance) |

## How to extend

**Adding a new agent** (e.g., pension fund, development bank):
1. Create `agents/NewAgent.scala` with `object NewAgent` + `case class State`.
2. Add `zero` and `initial` factory methods.
3. Add `step(prev: State, ...)(using p: SimParams): State` for monthly logic.
4. Add state field to `World.scala`.
5. Wire `step` call into the appropriate pipeline step (usually 2, 6, or 8).
6. If flows affect monetary stocks — add to `Sfc.MonthlyFlows` and verify
   the relevant SFC identity passes.
