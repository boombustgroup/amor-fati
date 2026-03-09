package sfc.agents

import sfc.config.SimParams
import sfc.types.*

/** Local government (JST / samorządy). JST receives PIT/CIT shares, property
  * tax, subventions/dotacje. JST deposits sit in commercial banks.
  */
object Jst:
  /** Compute JST monthly step.
    *
    * @param prev
    *   previous JST state
    * @param govTaxRevenue
    *   central government total tax revenue (CIT + VAT)
    * @param totalWageIncome
    *   total wage income (for PIT proxy)
    * @param gdp
    *   GDP proxy for subvention/dotacje
    * @param nFirms
    *   number of living firms (for property tax)
    * @return
    *   (newJstState, depositChange) where depositChange affects bank deposits
    *   (SFC Identity 2)
    */
  def step(
      prev: State,
      govTaxRevenue: Double,
      totalWageIncome: Double,
      gdp: Double,
      nFirms: Int,
      pitRevenue: Double = 0.0,
  )(using p: SimParams): (State, Double) =
    if !p.flags.jst then (prev, 0.0)
    else
      // Revenue sources:
      // 1. PIT share: JST gets ~38.46% of PIT collected
      //    When PIT mechanism enabled, use actual pitRevenue; otherwise proxy from wage income
      val jstPitIncome =
        if p.flags.pit && pitRevenue > 0 then pitRevenue * p.fiscal.jstPitShare.toDouble
        else totalWageIncome * 0.12 * p.fiscal.jstPitShare.toDouble // fallback proxy
      // 2. CIT share: JST gets ~6.71% of CIT
      val citRevenue  = govTaxRevenue * p.fiscal.jstCitShare.toDouble // govTaxRevenue includes CIT
      // 3. Property tax: fixed per firm per year
      val propertyTax = nFirms.toDouble * p.fiscal.jstPropertyTax.toDouble / 12.0
      // 4. Subwencja oświatowa (education subvention): ~3% of GDP annually
      val subvention  = gdp * p.fiscal.jstSubventionShare.toDouble / 12.0
      // 5. Dotacje celowe (targeted grants): ~1% of GDP annually
      val dotacje     = gdp * p.fiscal.jstDotacjeShare.toDouble / 12.0

      val totalRevenue  = jstPitIncome + citRevenue + propertyTax + subvention + dotacje
      // JST spending: revenue × spending multiplier (slightly > 1 → deficit bias)
      val totalSpending = totalRevenue * p.fiscal.jstSpendingMult
      val deficit       = totalSpending - totalRevenue
      val newDebt       = prev.debt + PLN(deficit)
      val depositChange = totalRevenue - totalSpending // negative when deficit (JST draws down deposits)
      val newDeposits   = prev.deposits + PLN(depositChange)

      (State(newDeposits, newDebt, PLN(totalRevenue), PLN(totalSpending), PLN(deficit)), depositChange)

  case class State(
      deposits: PLN, // JST deposits in commercial banks
      debt: PLN,     // cumulative JST debt
      revenue: PLN,  // this month's revenue
      spending: PLN, // this month's spending
      deficit: PLN,  // spending - revenue (positive = deficit)
  )

  object State:
    val zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
