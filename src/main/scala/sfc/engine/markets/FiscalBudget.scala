package sfc.engine.markets

import sfc.accounting.*
import sfc.config.*
import sfc.types.*

object FiscalBudget:

  def update(
      prev: GovState,
      citPaid: Double,
      vat: Double,
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
  )(using p: SimParams): GovState =
    val govBaseRaw               =
      if govPurchasesActual > 0 then govPurchasesActual
      else p.fiscal.govBaseSpending.toDouble * priceLevel
    val (govCurrent, govCapital) =
      if p.flags.govInvest then (govBaseRaw * (1.0 - p.fiscal.govInvestShare.toDouble), govBaseRaw * p.fiscal.govInvestShare.toDouble)
      else (govBaseRaw, 0.0)
    val totalSpend               =
      unempBenefitSpend + socialTransferSpend + govCurrent + govCapital + debtService + zusGovSubvention + euCofinancing
    val totalRev                 = citPaid + vat + nbpRemittance + exciseRevenue + customsDutyRevenue
    val deficit                  = totalSpend - totalRev
    val newBondsOutstanding      =
      if p.flags.govBondMarket then Math.max(0.0, prev.bondsOutstanding.toDouble + deficit)
      else prev.bondsOutstanding.toDouble
    val newCapitalStock          =
      if p.flags.govInvest then prev.publicCapitalStock.toDouble * (1.0 - p.fiscal.govDepreciationRate.toDouble / 12.0) + govCapital + euProjectCapital
      else 0.0
    GovState(
      PLN(totalRev),
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
