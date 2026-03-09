package sfc.engine

import sfc.accounting.*
import sfc.agents.*
import sfc.config.*
import sfc.types.*

/** Top-level orchestrator of the SFC-ABM simulation.
  *
  * Each call to `step` transforms the current state into the next state by
  * executing a fixed 10-stage pipeline that mirrors the real-world sequence of
  * economic decisions. The caller (Main.runSingle) invokes `step` in a monthly
  * loop, but `step` itself is time-agnostic — it only sees the current state:
  *
  *   - s1 FiscalConstraintStep — fiscal rules, minimum wage, lending base rate
  *   - s2 LaborDemographicsStep — labor market clearing, wages, demographics,
  *     ZUS/PPK
  *   - s3 HouseholdIncomeStep — HH income, consumption, PIT, sectoral mobility
  *   - s4 DemandStep — per-sector demand multipliers, aggregate demand
  *   - s5 FirmProcessingStep — production, I-O, technology adoption, loans, NPL
  *   - s6 HouseholdFinancialStep — mortgages, consumer credit, remittances,
  *     tourism
  *   - s7 PriceEquityStep — inflation, price level, GPW equity market, sigma
  *     dynamics
  *   - s8 OpenEconomyStep — BoP, forex, GVC trade, monetary policy, bonds, QE,
  *     NBFI
  *   - s9 BankUpdateStep — bank P&L, provisioning, CAR, interbank, deposit
  *     rates
  *   - s10 WorldAssemblyStep — assemble new World + SFC validation (14
  *     identities)
  *
  * The pipeline is strictly sequential: each step's Input case class carries
  * typed references to all prior step Outputs it needs (e.g. s7 receives
  * s1–s5). This makes data dependencies explicit at the type level and
  * eliminates the field-unpacking boilerplate that would otherwise dominate the
  * orchestrator.
  *
  * The final step (s10) assembles the updated World, reassigns households to
  * firms, and runs the SFC accounting check (see Sfc.validate). If any of the
  * 14 balance-sheet identities is violated, the check returns Left with
  * detailed error information — the simulation halts immediately in Main.
  *
  * No business logic lives here — every calculation is delegated to a Step
  * object in the `steps` package. This file is pure wiring.
  *
  * When to modify this file:
  *   - Adding a new pipeline stage: insert a new sN call and thread its Output
  *     to downstream steps that need it.
  *   - Adding a new field to an existing Step's Input: no change here — the
  *     Step's Input already receives the full Output objects.
  *   - Changing step ordering: reorder the sN calls (rare — ordering reflects
  *     real-world causality).
  *
  * @see
  *   [[sfc.accounting.Sfc]] — the 14 SFC identities and MonthlyFlows
  * @see
  *   [[steps.WorldAssemblyStep]] — final state assembly + SFC check
  * @see
  *   [[sfc.Main]] — simulation loop that calls step() each month
  */
object Simulation:

  /** Bundles the three mutable components of the simulation: the World state,
    * the firm vector, and the household vector. These always travel together
    * between simulation steps and the Main loop.
    */
  case class SimState(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
  )

  /** Result of a single monthly simulation step.
    *
    * Bundles the updated simulation state together with the outcome of the SFC
    * accounting check. The caller (Main) should inspect `sfcCheck` and halt on
    * `Left` — a failed identity means a monetary flow was mis-routed or
    * omitted.
    */
  case class StepResult(
      state: SimState,        // updated simulation state (World + firms + households)
      sfcCheck: Sfc.SfcResult, // Right(()) if all 13 identities hold
  )

  /** Transform current state into next state via the 10-stage pipeline.
    *
    * Executes stages s1–s10 in causal order. Each stage receives typed Output
    * references from all prior stages it depends on — no intermediate unpacking
    * is needed. The dependency DAG is:
    *
    * {{{
    *   s1 ──┬──────────────────────────────────────────────────────────────→ s9, s10
    *        ├→ s2 ──┬──────────────────────────────────────────────────────→ s9, s10
    *        │       ├→ s3 ──┬──────────────────────────────────────────────→ s9, s10
    *        │       │       ├→ s4 ──┬──────────────────────────────────────→ s9, s10
    *        │       │       │       ├→ s5 ──┬──────────────────────────────→ s9, s10
    *        │       │       │       │       ├→ s7 ─→ s8 ─→ s9 ─→ s10
    *        │       │       ├→ s6 ──┼───────┼──────→ s8
    *        │       │       │       │       │
    *        └───────┴───────┴───────┴───────┴──────────────────────────────→ s10 (World assembly)
    * }}}
    *
    * '''Ordering invariant:''' Scala val bindings are strictly sequential in
    * method bodies (forward references are compile errors), and each step's
    * Input requires specific prior Output types — reordering would not compile.
    *
    * @param state
    *   current simulation state (World + firms + households)
    * @param rc
    *   run configuration (currency regime, time horizon)
    * @return
    *   StepResult with updated state and SFC check outcome
    */
  def step(state: SimState, rc: RunConfig)(using SimParams): StepResult =
    import steps.{
      FiscalConstraintStep as S1,
      LaborDemographicsStep as S2,
      HouseholdIncomeStep as S3,
      DemandStep as S4,
      FirmProcessingStep as S5,
      HouseholdFinancialStep as S6,
      PriceEquityStep as S7,
      OpenEconomyStep as S8,
      BankUpdateStep as S9,
      WorldAssemblyStep as S10,
    }
    val SimState(w, firms, households) = state
    val s1                             = S1.run(S1.Input(w, rc))
    val s2                             = S2.run(S2.Input(w, rc, firms, households, s1))
    val s3                             = S3.run(S3.Input(w, rc, firms, households, s1, s2))
    val s4                             = S4.run(S4.Input(w, s2, s3))
    val s5                             = S5.run(S5.Input(w, rc, firms, households, s1, s2, s3, s4))
    val s6                             = S6.run(S6.Input(w, s1, s2, s3))
    val s7                             = S7.run(S7.Input(w, rc, s1, s2, s3, s4, s5))
    val s8                             = S8.run(S8.Input(w, rc, s1, s2, s3, s4, s5, s6, s7))
    val s9                             = S9.run(S9.Input(w, rc, s1, s2, s3, s4, s5, s6, s7, s8))
    val s10                            = S10.run(S10.Input(w, rc, firms, households, s1, s2, s3, s4, s5, s6, s7, s8, s9))
    StepResult(SimState(s10.newWorld, s10.finalFirms, s10.reassignedHouseholds), s10.sfcResult)
