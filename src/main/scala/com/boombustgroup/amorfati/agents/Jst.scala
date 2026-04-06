package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Local government (JST / samorządy). JST receives PIT/CIT shares, property
  * tax, subventions/dotacje. JST deposits sit in commercial banks and are
  * ledger-coverable cash balances; JST debt remains a cumulative fiscal metric
  * in the current model.
  */
object Jst:

  // Fallback effective PIT rate when PIT mechanism disabled
  private val FallbackPitRate = 0.12

  case class State(
      deposits: PLN, // JST deposits in commercial banks (ledger-coverable cash balance)
      debt: PLN,     // cumulative JST debt metric (not yet represented as a holder-tracked instrument)
      revenue: PLN,  // this month's revenue
      spending: PLN, // this month's spending
      deficit: PLN,  // spending − revenue (positive = deficit)
  )

  object State:
    val zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Result of monthly JST step. */
  case class StepResult(
      state: State,      // updated JST state
      depositChange: PLN, // effect on bank deposits (SFC Identity 2)
  )

  /** Monthly JST step: revenue (PIT/CIT shares, property tax, subventions,
    * dotacje) → spending (revenue × mult) → deficit → deposit change.
    */
  def step(
      prev: State,
      govTaxRevenue: PLN,   // central government total tax revenue (CIT + VAT)
      totalWageIncome: PLN, // total wage income (for PIT proxy)
      gdp: PLN,             // GDP proxy for subvention/dotacje
      nFirms: Int,          // number of living firms (for property tax)
      pitRevenue: PLN,
  )(using p: SimParams): StepResult =
    // Revenue sources:
    // 1. PIT share: JST gets ~38.46% of PIT collected
    val jstPitIncome =
      if pitRevenue > PLN.Zero then pitRevenue * p.fiscal.jstPitShare
      else totalWageIncome * (Share(FallbackPitRate) * p.fiscal.jstPitShare)
    // 2. CIT share: JST gets ~6.71% of CIT
    val citRevenue   = govTaxRevenue * p.fiscal.jstCitShare
    // 3. Property tax: fixed per firm per year
    val propertyTax  = nFirms * p.fiscal.jstPropertyTax / 12L
    // 4. Subwencja oświatowa (education subvention): ~3% of GDP annually
    val subvention   = gdp * p.fiscal.jstSubventionShare / 12L
    // 5. Dotacje celowe (targeted grants): ~1% of GDP annually
    val dotacje      = gdp * p.fiscal.jstDotacjeShare / 12L

    val totalRevenue  = jstPitIncome + citRevenue + propertyTax + subvention + dotacje
    // JST spending: revenue × spending multiplier (slightly > 1 → deficit bias)
    val totalSpending = totalRevenue * p.fiscal.jstSpendingMult
    val deficit       = totalSpending - totalRevenue
    val depositChange = totalRevenue - totalSpending // negative when deficit
    val newDeposits   = prev.deposits + depositChange
    val newDebt       = prev.debt + deficit

    StepResult(State(newDeposits, newDebt, totalRevenue, totalSpending, deficit), depositChange)
