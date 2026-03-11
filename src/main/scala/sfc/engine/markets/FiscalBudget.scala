package sfc.engine.markets

import sfc.accounting.*
import sfc.config.*
import sfc.types.*

/** Government budget reconciliation: monthly revenue, spending, deficit, debt.
  *
  * Revenue: CIT + dividend tax (PIT Belka 19%), VAT, excise, customs, NBP
  * remittance. Spending: current government purchases, capital investment,
  * unemployment benefits, social transfers (800+), ZUS subvention, EU
  * co-financing, debt service.
  *
  * When GOV_INVEST is enabled, base spending splits into current (1 − share)
  * and capital (share) components; public capital stock depreciates monthly.
  * Bond-financed deficit accumulates when GOV_BOND_MARKET is enabled.
  *
  * Calibration: MF budgetary law (ustawa budżetowa) structure. All flows in
  * nominal PLN, price-adjusted via priceLevel.
  */
object FiscalBudget:

  /** Monthly fiscal inputs — all monetary fields in PLN. */
  case class Input(
      prev: GovState,
      priceLevel: Double,
      // Revenue
      citPaid: PLN = PLN.Zero,
      vat: PLN = PLN.Zero,
      nbpRemittance: PLN = PLN.Zero,
      exciseRevenue: PLN = PLN.Zero,
      customsDutyRevenue: PLN = PLN.Zero,
      // Spending
      unempBenefitSpend: PLN = PLN.Zero,
      debtService: PLN = PLN.Zero,
      zusGovSubvention: PLN = PLN.Zero,
      socialTransferSpend: PLN = PLN.Zero,
      euCofinancing: PLN = PLN.Zero,
      euProjectCapital: PLN = PLN.Zero,
      govPurchasesActual: PLN = PLN.Zero,
  )

  /** Monthly budget update → new GovState. */
  def update(in: Input)(using p: SimParams): GovState =
    val govBaseRaw: PLN =
      if in.govPurchasesActual > PLN.Zero then in.govPurchasesActual
      else p.fiscal.govBaseSpending * in.priceLevel

    val (govCurrent, govCapital): (PLN, PLN) =
      if p.flags.govInvest then
        val capShare = p.fiscal.govInvestShare.toDouble
        (govBaseRaw * (1.0 - capShare), govBaseRaw * capShare)
      else (govBaseRaw, PLN.Zero)

    val totalSpend = in.unempBenefitSpend + in.socialTransferSpend +
      govCurrent + govCapital + in.debtService + in.zusGovSubvention + in.euCofinancing
    val totalRev   = in.citPaid + in.vat + in.nbpRemittance +
      in.exciseRevenue + in.customsDutyRevenue
    val deficit    = totalSpend - totalRev

    val newBondsOutstanding =
      if p.flags.govBondMarket then (in.prev.bondsOutstanding + deficit).max(PLN.Zero)
      else in.prev.bondsOutstanding

    val newCapitalStock =
      if p.flags.govInvest then
        val monthlyDepreciation = p.fiscal.govDepreciationRate.toDouble / 12.0
        in.prev.publicCapitalStock * (1.0 - monthlyDepreciation) + govCapital + in.euProjectCapital
      else PLN.Zero

    GovState(
      taxRevenue = totalRev,
      deficit = deficit,
      cumulativeDebt = in.prev.cumulativeDebt + deficit,
      unempBenefitSpend = in.unempBenefitSpend,
      bondsOutstanding = newBondsOutstanding,
      bondYield = in.prev.bondYield,
      debtServiceSpend = in.debtService,
      socialTransferSpend = in.socialTransferSpend,
      publicCapitalStock = newCapitalStock,
      govCurrentSpend = govCurrent,
      govCapitalSpend = govCapital + in.euProjectCapital,
      euCofinancing = in.euCofinancing,
      exciseRevenue = in.exciseRevenue,
      customsDutyRevenue = in.customsDutyRevenue,
    )
