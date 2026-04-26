package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.types.*

/** Government budget reconciliation: monthly revenue, spending, deficit, debt.
  *
  * Revenue: CIT + dividend tax (PIT Belka 19%), VAT, excise, customs, NBP
  * remittance, plus separately tracked SOE dividend extraction. Spending:
  * current government purchases, capital investment, unemployment benefits,
  * social transfers (800+), ZUS subvention, EU co-financing, debt service.
  *
  * When GOV_INVEST is enabled, base spending splits into current (1 − share)
  * and capital (share) components; public capital stock depreciates monthly.
  * Government-bond stock is settled downstream into `LedgerFinancialState`.
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
    * `govCurrentSpend`, `govCapitalSpend`, `euProjectCapital`, `euCofinancing`)
    * are single-month flows. `govCapitalSpend` is only the domestic
    * budget-financed capital outlay. `euProjectCapital` is the capital portion
    * of the total EU project envelope (EU transfer + domestic co-financing).
    * Stock fields (`cumulativeDebt`, `publicCapitalStock`) accumulate across
    * months. Government-bond ownership and issuer-side outstanding stock live
    * in `LedgerFinancialState`; this state keeps only the broader fiscal debt
    * metric (Σ deficits), not a separate tradable instrument.
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
  case class GovFinancialState(
      cumulativeDebt: PLN, // fiscal debt metric (Σ deficits since t = 0), not a separate holder-tracked instrument
  )

  case class GovPolicyState(
      bondYield: Rate = Rate.Zero,                    // lagged market yield (for lending rates)
      weightedCoupon: Rate = Rate.Zero,               // portfolio-weighted average coupon (WAM rolling model)
      publicCapitalStock: PLN = PLN.Zero,             // public capital stock accumulated from domestic capex and EU project capital
      minWageLevel: PLN = PLN(4666),                  // statutory minimum wage (PLN/month, GUS 2024)
      minWagePriceLevel: PriceIndex = PriceIndex.Base, // price level at last minimum wage adjustment
  )

  case class GovFlowState(
      taxRevenue: PLN,                     // budget revenue this month excluding direct SOE dividend extraction
      deficit: PLN,                        // monthly budget deficit (positive) or surplus (negative)
      unempBenefitSpend: PLN,              // unemployment benefit payments this month
      debtServiceSpend: PLN = PLN.Zero,    // interest payments on public debt this month
      socialTransferSpend: PLN = PLN.Zero, // social transfers (800+, family benefits) this month
      govCurrentSpend: PLN = PLN.Zero,     // domestic current government purchases (1 − investShare) × base
      govCapitalSpend: PLN = PLN.Zero,     // domestic budget-financed capital government investment only
      euProjectCapital: PLN = PLN.Zero,    // capital share of the total EU project envelope (EU funds + domestic co-financing)
      euCofinancing: PLN = PLN.Zero,       // domestic co-financing outlay booked separately from project-envelope value
      exciseRevenue: PLN = PLN.Zero,       // excise duty revenue this month (akcyza)
      customsDutyRevenue: PLN = PLN.Zero,  // customs duty revenue this month (cło)
      govDividendRevenue: PLN = PLN.Zero,  // direct SOE dividend extraction booked separately from aggregate budget revenue
  )

  case class GovState(
      financial: GovFinancialState,
      policy: GovPolicyState,
      monthly: GovFlowState,
  ):
    def taxRevenue: PLN               = monthly.taxRevenue
    def govDividendRevenue: PLN       = monthly.govDividendRevenue
    def totalRevenue: PLN             = monthly.taxRevenue + monthly.govDividendRevenue
    def deficit: PLN                  = monthly.deficit
    def cumulativeDebt: PLN           = financial.cumulativeDebt
    def unempBenefitSpend: PLN        = monthly.unempBenefitSpend
    def bondYield: Rate               = policy.bondYield
    def weightedCoupon: Rate          = policy.weightedCoupon
    def debtServiceSpend: PLN         = monthly.debtServiceSpend
    def socialTransferSpend: PLN      = monthly.socialTransferSpend
    def publicCapitalStock: PLN       = policy.publicCapitalStock
    def govCurrentSpend: PLN          = monthly.govCurrentSpend
    def govCapitalSpend: PLN          = monthly.govCapitalSpend
    def euProjectCapital: PLN         = monthly.euProjectCapital
    def euCofinancing: PLN            = monthly.euCofinancing
    def exciseRevenue: PLN            = monthly.exciseRevenue
    def customsDutyRevenue: PLN       = monthly.customsDutyRevenue
    def minWageLevel: PLN             = policy.minWageLevel
    def minWagePriceLevel: PriceIndex = policy.minWagePriceLevel

    /** Domestic budget-financed demand anchor used by fiscal-demand rules. */
    def domesticBudgetDemand: PLN = govCurrentSpend + govCapitalSpend

    /** Central-budget outlays before separate social-fund subventions are added
      * in SFC/world assembly. Excludes the total EU project envelope.
      */
    def domesticBudgetOutlays: PLN =
      unempBenefitSpend + socialTransferSpend + govCurrentSpend + govCapitalSpend + debtServiceSpend + euCofinancing

  object GovState:
    def apply(
        taxRevenue: PLN,
        deficit: PLN,
        cumulativeDebt: PLN,
        unempBenefitSpend: PLN,
        bondYield: Rate = Rate.Zero,
        weightedCoupon: Rate = Rate.Zero,
        debtServiceSpend: PLN = PLN.Zero,
        socialTransferSpend: PLN = PLN.Zero,
        publicCapitalStock: PLN = PLN.Zero,
        govCurrentSpend: PLN = PLN.Zero,
        govCapitalSpend: PLN = PLN.Zero,
        euProjectCapital: PLN = PLN.Zero,
        euCofinancing: PLN = PLN.Zero,
        exciseRevenue: PLN = PLN.Zero,
        customsDutyRevenue: PLN = PLN.Zero,
        govDividendRevenue: PLN = PLN.Zero,
        minWageLevel: PLN = PLN(4666),
        minWagePriceLevel: PriceIndex = PriceIndex.Base,
    ): GovState =
      GovState(
        financial = GovFinancialState(cumulativeDebt),
        policy = GovPolicyState(bondYield, weightedCoupon, publicCapitalStock, minWageLevel, minWagePriceLevel),
        monthly = GovFlowState(
          taxRevenue,
          deficit,
          unempBenefitSpend,
          debtServiceSpend,
          socialTransferSpend,
          govCurrentSpend,
          govCapitalSpend,
          euProjectCapital,
          euCofinancing,
          exciseRevenue,
          customsDutyRevenue,
          govDividendRevenue,
        ),
      )

  // ---------------------------------------------------------------------------
  // Budget update
  // ---------------------------------------------------------------------------

  /** Monthly fiscal inputs — all monetary fields in PLN. */
  case class Input(
      prev: GovState,
      priceLevel: PriceIndex,
      // Revenue
      citPaid: PLN = PLN.Zero,
      govDividendRevenue: PLN,
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
      else p.fiscal.govBaseSpending * in.priceLevel.toMultiplier

    val (govCurrent, govCapital): (PLN, PLN) =
      val capShare = p.fiscal.govInvestShare
      (govBaseRaw * (Share.One - capShare), govBaseRaw * capShare)

    val totalSpend = in.unempBenefitSpend + in.socialTransferSpend +
      govCurrent + govCapital + in.debtService + in.zusGovSubvention + in.nfzGovSubvention + in.earmarkedGovSubvention + in.euCofinancing
    val taxRev     = in.citPaid + in.vat + in.nbpRemittance + in.exciseRevenue + in.customsDutyRevenue
    val totalRev   = taxRev + in.govDividendRevenue
    val deficit    = totalSpend - totalRev

    val newCapitalStock =
      val monthlyDeprecShare = p.fiscal.govDepreciationRate.monthly.toMultiplier.toShare
      in.prev.publicCapitalStock * (Share.One - monthlyDeprecShare) + govCapital + in.euProjectCapital

    GovState(
      taxRevenue = taxRev,
      deficit = deficit,
      cumulativeDebt = in.prev.cumulativeDebt + deficit,
      unempBenefitSpend = in.unempBenefitSpend,
      bondYield = in.prev.bondYield,           // lagged market yield — updated by OpenEconomyStep
      weightedCoupon = in.prev.weightedCoupon, // WAM coupon — updated by OpenEconomyStep
      debtServiceSpend = in.debtService,
      socialTransferSpend = in.socialTransferSpend,
      publicCapitalStock = newCapitalStock,
      govCurrentSpend = govCurrent,
      govCapitalSpend = govCapital,
      euProjectCapital = in.euProjectCapital,
      euCofinancing = in.euCofinancing,
      exciseRevenue = in.exciseRevenue,
      customsDutyRevenue = in.customsDutyRevenue,
      govDividendRevenue = in.govDividendRevenue,
    )

  def nextGovBondOutstanding(prevGovBondOutstanding: PLN, deficit: PLN): PLN =
    (prevGovBondOutstanding + deficit).max(PLN.Zero)
