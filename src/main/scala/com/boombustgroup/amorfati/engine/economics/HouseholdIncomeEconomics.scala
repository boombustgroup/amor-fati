package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.LaborMarket
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Pure economic logic for household income determination — no state mutation,
  * no flows.
  *
  * Computes individual household income, consumption, saving, and portfolio
  * decisions. Integrates labor market separations, wage updates, bank-specific
  * lending/deposit rates, equity returns, and sectoral mobility signals into
  * household-level state updates.
  *
  * Extracted from HouseholdIncomeStep (Calculus vs Accounting split).
  */
object HouseholdIncomeEconomics:

  // ---- Calibration constants ----
  private val ImportErElasticity = 0.5 // exchange rate elasticity of import propensity

  case class Output(
      totalIncome: PLN,                               // aggregate household income (wages + benefits + transfers)
      consumption: PLN,                               // aggregate household consumption spending
      importCons: PLN,                                // import component of household consumption (forex demand)
      domesticCons: PLN,                              // domestic component of household consumption
      updatedHouseholds: Vector[Household.State],     // post-income household population
      hhAgg: Household.Aggregates,                    // household-level aggregates (employment, savings, etc.)
      perBankHhFlowsOpt: Option[Vector[PerBankFlow]], // per-bank household flow breakdown (multi-bank mode)
      pitRevenue: PLN,                                // personal income tax collected from households
      importAdj: Double,                              // ER-adjusted import propensity (base * ER elasticity)
      aggUnempBenefit: PLN,                           // aggregate unemployment benefit payments
  )

  @boundaryEscape
  def compute(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      lendingBaseRate: Rate,
      resWage: PLN,
      newWage: PLN,
      rng: Random,
  )(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val importAdj = toDouble(p.forex.importPropensity) *
      Math.pow(p.forex.baseExRate / w.forex.exchangeRate, ImportErElasticity)

    val afterSep           = LaborMarket.separations(households, firms, firms)
    val afterWages         = LaborMarket.updateWages(afterSep, firms, newWage)
    val bsec               = w.bankingSector
    val nBanksHh           = banks.length
    val hhBankRates        = Some(
      BankRates(
        lendingRates = banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, lendingBaseRate, w.gov.bondYield)),
        depositRates = banks.map(_ => Banking.hhDepositRate(w.nbp.referenceRate)),
      ),
    )
    val eqReturn           = w.financial.equity.monthlyReturn
    val secWages           = if p.flags.sectoralMobility then Some(SectoralMobility.sectorWages(afterWages)) else None
    val secVacancies       =
      if p.flags.sectoralMobility then Some(SectoralMobility.sectorVacancies(afterWages, firms)) else None
    val (newHhs, agg, pbf) = Household.step(
      afterWages,
      w,
      newWage,
      resWage,
      importAdj,
      rng,
      nBanksHh,
      hhBankRates,
      eqReturn,
      secWages,
      secVacancies,
    )

    val pitRevenue =
      if p.flags.pit then toDouble(agg.totalPit)
      else 0.0

    Output(
      agg.totalIncome,
      agg.consumption,
      agg.importConsumption,
      agg.domesticConsumption,
      newHhs,
      agg,
      pbf,
      PLN(pitRevenue),
      importAdj,
      aggUnempBenefit = PLN.Zero,
    )
