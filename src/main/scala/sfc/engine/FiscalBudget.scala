package sfc.engine

import sfc.accounting.*
import sfc.config.*
import sfc.types.*

object FiscalBudget:

  def update(
    prev: GovState,
    citPaid: Double,
    vat: Double,
    bdpActive: Boolean,
    bdpAmount: Double,
    priceLevel: Double,
    unempBenefitSpend: Double,
    debtService: Double = 0.0,
    nbpRemittance: Double = 0.0,
    zusGovSubvention: Double = 0.0,
    socialTransferSpend: Double = 0.0,
    euCofinancing: Double = 0.0,
    euProjectCapital: Double = 0.0,
    exciseRevenue: Double = 0.0,
    customsDutyRevenue: Double = 0.0,
    govPurchasesActual: Double = 0.0,
  ): GovState =
    val bdpSpend = if bdpActive then Config.TotalPopulation.toDouble * bdpAmount else 0.0
    val govBaseRaw =
      if govPurchasesActual > 0 then govPurchasesActual
      else Config.GovBaseSpending * priceLevel
    val (govCurrent, govCapital) =
      if Config.GovInvestEnabled then (govBaseRaw * (1.0 - Config.GovInvestShare), govBaseRaw * Config.GovInvestShare)
      else (govBaseRaw, 0.0)
    val totalSpend =
      bdpSpend + unempBenefitSpend + socialTransferSpend + govCurrent + govCapital + debtService + zusGovSubvention + euCofinancing
    val totalRev = citPaid + vat + nbpRemittance + exciseRevenue + customsDutyRevenue
    val deficit = totalSpend - totalRev
    val newBondsOutstanding =
      if Config.GovBondMarket then Math.max(0.0, prev.bondsOutstanding.toDouble + deficit)
      else prev.bondsOutstanding.toDouble
    val newCapitalStock =
      if Config.GovInvestEnabled then
        prev.publicCapitalStock.toDouble * (1.0 - Config.GovDepreciationRate / 12.0) + govCapital + euProjectCapital
      else 0.0
    GovState(
      bdpActive,
      PLN(totalRev),
      PLN(bdpSpend),
      PLN(deficit),
      PLN(prev.cumulativeDebt.toDouble + deficit),
      PLN(unempBenefitSpend),
      PLN(newBondsOutstanding),
      prev.bondYield,
      PLN(debtService),
      PLN(socialTransferSpend),
      PLN(newCapitalStock),
      PLN(govCurrent),
      PLN(govCapital + euProjectCapital),
      PLN(euCofinancing),
      PLN(exciseRevenue),
      PLN(customsDutyRevenue),
    )
