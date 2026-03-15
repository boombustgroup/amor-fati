package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.LaborMarket
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Household income determination: computes individual household income,
  * consumption, saving, and portfolio decisions. Integrates labor market
  * separations, wage updates, bank-specific lending/deposit rates, equity
  * returns, and sectoral mobility signals into household-level state updates.
  */
object HouseholdIncomeStep:

  // ---- Calibration constants ----
  private val ImportErElasticity = 0.5 // exchange rate elasticity of import propensity

  case class Input(
      w: World,                            // current world state
      firms: Vector[Firm.State],           // pre-step firm population
      households: Vector[Household.State], // pre-step household population
      s1: FiscalConstraintStep.Output,     // fiscal constraint (reservation wage, lending base rate)
      s2: LaborDemographicsStep.Output,    // labor/demographics (new wage)
  )

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

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    val importAdj = p.forex.importPropensity.toDouble *
      Math.pow(p.forex.baseExRate / in.w.forex.exchangeRate, ImportErElasticity)

    val afterSep           = LaborMarket.separations(in.households, in.firms, in.firms)
    val afterWages         = LaborMarket.updateWages(afterSep, in.s2.newWage)
    val bsec               = in.w.bankingSector
    val nBanksHh           = bsec.banks.length
    val hhBankRates        = Some(
      BankRates(
        lendingRates = bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.s1.lendingBaseRate, in.w.gov.bondYield)),
        depositRates = bsec.banks.map(_ => Banking.hhDepositRate(in.w.nbp.referenceRate)),
      ),
    )
    val eqReturn           = in.w.financial.equity.monthlyReturn.toDouble
    val secWages           = if p.flags.sectoralMobility then Some(SectoralMobility.sectorWages(afterWages)) else None
    val secVacancies       =
      if p.flags.sectoralMobility then Some(SectoralMobility.sectorVacancies(afterWages, in.firms)) else None
    val (newHhs, agg, pbf) = Household.step(
      afterWages,
      in.w,
      in.s2.newWage,
      in.s1.resWage,
      importAdj,
      rng,
      nBanksHh,
      hhBankRates,
      eqReturn,
      secWages,
      secVacancies,
    )

    val pitRevenue =
      if p.flags.pit then agg.totalPit.toDouble
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
