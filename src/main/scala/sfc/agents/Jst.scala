package sfc.agents

import sfc.config.Config

/** State of local government (JST / samorządy). Phase 7B.
  * JST receives PIT/CIT shares, property tax, subventions/dotacje.
  * JST deposits sit in commercial banks and modify SFC Identity 2. */
case class JstState(
  deposits: Double,   // JST deposits in commercial banks
  debt: Double,       // cumulative JST debt
  revenue: Double,    // this month's revenue
  spending: Double,   // this month's spending
  deficit: Double     // spending - revenue (positive = deficit)
)

object JstState:
  val zero: JstState = JstState(0.0, 0.0, 0.0, 0.0, 0.0)

object JstLogic:
  /** Compute JST monthly step.
    * @param prev previous JST state
    * @param govTaxRevenue central government total tax revenue (CIT + VAT)
    * @param totalWageIncome total wage income (for PIT proxy)
    * @param gdp GDP proxy for subvention/dotacje
    * @param nFirms number of living firms (for property tax)
    * @return (newJstState, depositChange) where depositChange affects bank deposits (SFC Identity 2) */
  def step(prev: JstState, govTaxRevenue: Double, totalWageIncome: Double,
           gdp: Double, nFirms: Int): (JstState, Double) =
    if !Config.JstEnabled then (prev, 0.0)
    else
      // Revenue sources:
      // 1. PIT share: JST gets ~38.46% of PIT collected (simplified as % of wage income × effective PIT rate)
      val effectivePitRate = 0.12  // progressive 12%/32% + kwota wolna → effective ~12%
      val pitRevenue = totalWageIncome * effectivePitRate * Config.JstPitShare
      // 2. CIT share: JST gets ~6.71% of CIT
      val citRevenue = govTaxRevenue * Config.JstCitShare  // govTaxRevenue includes CIT
      // 3. Property tax: fixed per firm per year
      val propertyTax = nFirms.toDouble * Config.JstPropertyTax / 12.0
      // 4. Subwencja oświatowa (education subvention): ~3% of GDP annually
      val subvention = gdp * Config.JstSubventionShare / 12.0
      // 5. Dotacje celowe (targeted grants): ~1% of GDP annually
      val dotacje = gdp * Config.JstDotacjeShare / 12.0

      val totalRevenue = pitRevenue + citRevenue + propertyTax + subvention + dotacje
      // JST spending: revenue × spending multiplier (slightly > 1 → deficit bias)
      val totalSpending = totalRevenue * Config.JstSpendingMult
      val deficit = totalSpending - totalRevenue
      val newDebt = prev.debt + deficit
      val depositChange = totalRevenue - totalSpending  // negative when deficit (JST draws down deposits)
      val newDeposits = prev.deposits + depositChange

      (JstState(newDeposits, newDebt, totalRevenue, totalSpending, deficit), depositChange)
