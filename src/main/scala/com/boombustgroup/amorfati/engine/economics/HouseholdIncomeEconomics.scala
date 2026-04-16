package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.LaborMarket
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

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
  private val ImportErElasticity                          = 0.5 // exchange rate elasticity of import propensity
  private def exchangeRateValue(er: ExchangeRate): Double =
    er.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD

  case class Output(
      totalIncome: PLN,                               // aggregate household income (wages + benefits + transfers)
      consumption: PLN,                               // aggregate household consumption spending
      importCons: PLN,                                // import component of household consumption (forex demand)
      domesticCons: PLN,                              // domestic component of household consumption
      updatedHouseholds: Vector[Household.State],     // post-income household population
      hhAgg: Household.Aggregates,                    // household-level aggregates (employment, savings, etc.)
      perBankHhFlowsOpt: Option[Vector[PerBankFlow]], // per-bank household flow breakdown (multi-bank mode)
      pitRevenue: PLN,                                // personal income tax collected from households
      importAdj: Share,                               // ER-adjusted import propensity (base * ER elasticity)
      aggUnempBenefit: PLN,                           // aggregate unemployment benefit payments
  )

  @boundaryEscape
  def compute(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
      lendingBaseRate: Rate,
      resWage: PLN,
      newWage: PLN,
      rng: RandomStream,
  )(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val importAdj = Share(
      toDouble(p.forex.importPropensity) *
        Math.pow(toDouble(p.forex.baseExRate) / exchangeRateValue(w.forex.exchangeRate), ImportErElasticity),
    )

    val afterSep           = LaborMarket.separations(households, firms, firms)
    val afterWages         = LaborMarket.updateWages(afterSep, firms, newWage)
    val bsec               = w.bankingSector
    val nBanksHh           = banks.length
    val hhBankRates        = Some(
      BankRates(
        lendingRates = banks
          .zip(bsec.configs)
          .map((b, cfg) => Banking.lendingRate(b, cfg, lendingBaseRate, w.gov.bondYield, CorporateBondOwnership.bankHolderFor(ledgerFinancialState, b.id))),
        depositRates = banks.map(_ => Banking.hhDepositRate(w.nbp.referenceRate)),
      ),
    )
    val eqReturn           = w.financialMarkets.equity.monthlyReturn
    val secWages           = Some(SectoralMobility.sectorWages(afterWages))
    val secVacancies       = Some(SectoralMobility.sectorVacancies(afterWages, firms))
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

    Output(
      agg.totalIncome,
      agg.consumption,
      agg.importConsumption,
      agg.domesticConsumption,
      newHhs,
      agg,
      pbf,
      agg.totalPit,
      importAdj,
      aggUnempBenefit = PLN.Zero,
    )
