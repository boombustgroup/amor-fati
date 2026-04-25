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
  private val ImportErElasticity = Coefficient.decimal(5, 1) // exchange rate elasticity of import propensity

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
      ledgerFinancialState: LedgerFinancialState,     // ledger after household financial stock updates
  )

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
    val importAdj =
      (p.forex.importPropensity * p.forex.baseExRate.ratioTo(w.forex.exchangeRate).pow(ImportErElasticity.toScalar)).toShare.clamp(
        Share.Zero,
        Share.One,
      )

    val afterSep      = LaborMarket.separations(households, firms, firms)
    val afterWages    = LaborMarket.updateWages(afterSep, firms, newWage)
    val bsec          = w.bankingSector
    val nBanksHh      = banks.length
    val bankStocks    = ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks)
    val hhBankRates   = Some(
      BankRates(
        lendingRates = banks
          .zip(bankStocks)
          .zip(bsec.configs)
          .map { case ((b, stocks), cfg) =>
            Banking.lendingRate(b, stocks, cfg, lendingBaseRate, w.gov.bondYield, CorporateBondOwnership.bankHolderFor(ledgerFinancialState, b.id))
          },
        depositRates = banks.map(_ => Banking.hhDepositRate(w.nbp.referenceRate)),
      ),
    )
    val eqReturn      = w.financialMarkets.equity.monthlyReturn
    val secWages      = Some(SectoralMobility.sectorWages(afterWages))
    val secVacancies  = Some(SectoralMobility.sectorVacancies(afterWages, firms))
    val openingStocks = ledgerFinancialState.households.map(LedgerFinancialState.projectHouseholdFinancialStocks)
    val householdStep = Household.step(
      afterWages,
      openingStocks,
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
    val agg           = householdStep.aggregates

    Output(
      agg.totalIncome,
      agg.consumption,
      agg.importConsumption,
      agg.domesticConsumption,
      householdStep.households,
      agg,
      householdStep.perBankFlows,
      agg.totalPit,
      importAdj,
      aggUnempBenefit = PLN.Zero,
      ledgerFinancialState = ledgerFinancialState.copy(households = householdStep.financialStocks.map(LedgerFinancialState.householdBalances)),
    )
