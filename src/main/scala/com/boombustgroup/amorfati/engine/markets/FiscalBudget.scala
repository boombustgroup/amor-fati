package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.types.*

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

  // ---------------------------------------------------------------------------
  // State type
  // ---------------------------------------------------------------------------

  /** Government sector balance sheet and flow snapshot.
    *
    * Updated monthly by [[update]]. Revenue fields (`taxRevenue`,
    * `exciseRevenue`, `customsDutyRevenue`) and spending fields
    * (`unempBenefitSpend`, `debtServiceSpend`, `socialTransferSpend`,
    * `govCurrentSpend`, `govCapitalSpend`, `euCofinancing`) are single-month
    * flows. Stock fields (`cumulativeDebt`, `bondsOutstanding`,
    * `publicCapitalStock`) accumulate across months.
    *
    * `deficit` = totalSpend − totalRevenue (positive = deficit, negative =
    * surplus). `cumulativeDebt` += deficit each month.
    *
    * `weightedCoupon` tracks the portfolio-weighted average coupon rate. This
    * is the rolling-portfolio WAM (weighted average maturity) model: each
    * month, a fraction `1/govAvgMaturityMonths` of the portfolio matures and is
    * refinanced at the current market yield. New deficit issuance also enters
    * at current yield. The weighted coupon converges gradually to market yield
    * — a +5pp yield shock at 54-month average maturity takes ~4.5 years to
    * fully pass through to debt service, matching the actual MF flat redemption
    * profile (Strategia zarządzania długiem sektora finansów publicznych 2024).
    *
    * Calibration: MF budgetary law structure 2024, GUS public finance
    * statistics, NBP government securities data.
    */
  case class GovState(
      taxRevenue: PLN,                     // total tax revenue this month (CIT + VAT + excise + customs + NBP remittance)
      deficit: PLN,                        // monthly budget deficit (positive) or surplus (negative)
      cumulativeDebt: PLN,                 // cumulative public debt stock (Σ deficits since t = 0)
      unempBenefitSpend: PLN,              // unemployment benefit payments this month
      bondsOutstanding: PLN = PLN.Zero,    // government bond stock (skarbowe papiery wartościowe)
      foreignBondHoldings: PLN = PLN.Zero, // non-resident SPW holdings (~35%, NBP)
      bondYield: Rate = Rate.Zero,         // lagged market yield (for lending rates)
      weightedCoupon: Rate = Rate.Zero,    // portfolio-weighted average coupon (WAM rolling model)
      debtServiceSpend: PLN = PLN.Zero,    // interest payments on public debt this month
      socialTransferSpend: PLN = PLN.Zero, // social transfers (800+, family benefits) this month
      publicCapitalStock: PLN = PLN.Zero,  // public capital stock (roads, infrastructure) — GOV_INVEST
      govCurrentSpend: PLN = PLN.Zero,     // current government purchases (1 − investShare) × base
      govCapitalSpend: PLN = PLN.Zero,     // capital government investment (investShare × base + EU capital)
      euCofinancing: PLN = PLN.Zero,       // EU co-financing expenditure this month
      exciseRevenue: PLN = PLN.Zero,       // excise duty revenue this month (akcyza)
      customsDutyRevenue: PLN = PLN.Zero,  // customs duty revenue this month (cło)
      minWageLevel: PLN = PLN(4666.0),     // statutory minimum wage (PLN/month, GUS 2024)
      minWagePriceLevel: Double = 1.0,     // price level at last minimum wage adjustment
  )

  // ---------------------------------------------------------------------------
  // Budget update
  // ---------------------------------------------------------------------------

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
      nfzGovSubvention: PLN = PLN.Zero,
      earmarkedGovSubvention: PLN = PLN.Zero,
      socialTransferSpend: PLN = PLN.Zero,
      euCofinancing: PLN = PLN.Zero,
      euProjectCapital: PLN = PLN.Zero,
      govPurchasesActual: PLN = PLN.Zero,
  )

  /** Monthly budget update → new GovState. */
  def update(in: Input)(using p: SimParams): GovState =
    val govBaseRaw: PLN =
      if in.govPurchasesActual > PLN.Zero then in.govPurchasesActual
      else p.fiscal.govBaseSpending * Multiplier(in.priceLevel)

    val (govCurrent, govCapital): (PLN, PLN) =
      if p.flags.govInvest then
        val capShare = p.fiscal.govInvestShare
        (govBaseRaw * (Share.One - capShare), govBaseRaw * capShare)
      else (govBaseRaw, PLN.Zero)

    val totalSpend = in.unempBenefitSpend + in.socialTransferSpend +
      govCurrent + govCapital + in.debtService + in.zusGovSubvention + in.nfzGovSubvention + in.earmarkedGovSubvention + in.euCofinancing
    val totalRev   = in.citPaid + in.vat + in.nbpRemittance +
      in.exciseRevenue + in.customsDutyRevenue
    val deficit    = totalSpend - totalRev

    val newBondsOutstanding =
      if p.flags.govBondMarket then (in.prev.bondsOutstanding + deficit).max(PLN.Zero)
      else in.prev.bondsOutstanding

    val newCapitalStock =
      if p.flags.govInvest then
        val monthlyDepreciation = p.fiscal.govDepreciationRate.toDouble / 12.0
        in.prev.publicCapitalStock * Share(1.0 - monthlyDepreciation) + govCapital + in.euProjectCapital
      else PLN.Zero

    GovState(
      taxRevenue = totalRev,
      deficit = deficit,
      cumulativeDebt = in.prev.cumulativeDebt + deficit,
      unempBenefitSpend = in.unempBenefitSpend,
      bondsOutstanding = newBondsOutstanding,
      bondYield = in.prev.bondYield,           // lagged market yield — updated by OpenEconomyStep
      weightedCoupon = in.prev.weightedCoupon, // WAM coupon — updated by OpenEconomyStep
      debtServiceSpend = in.debtService,
      socialTransferSpend = in.socialTransferSpend,
      publicCapitalStock = newCapitalStock,
      govCurrentSpend = govCurrent,
      govCapitalSpend = govCapital + in.euProjectCapital,
      euCofinancing = in.euCofinancing,
      exciseRevenue = in.exciseRevenue,
      customsDutyRevenue = in.customsDutyRevenue,
    )
