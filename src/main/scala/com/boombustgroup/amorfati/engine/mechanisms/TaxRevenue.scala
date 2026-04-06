package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Tax revenue computation: VAT, excise, customs, and informal-economy evasion
  * adjustments.
  *
  * Extracted from BankingEconomics to separate fiscal revenue logic from
  * banking balance-sheet updates. All flows are monthly, in PLN.
  */
object TaxRevenue:

  case class Input(
      consumption: PLN,           // aggregate household consumption (VAT/excise tax base)
      pitRevenue: PLN,            // gross PIT revenue before informal evasion
      totalImports: PLN,          // total imports (customs duty tax base)
      informalCyclicalAdj: Double, // lagged cyclical adjustment for shadow economy share
  )

  case class Output(
      vat: PLN,                     // gross VAT revenue (sector-weighted effective rates)
      vatAfterEvasion: PLN,         // net VAT revenue after informal economy evasion
      pitAfterEvasion: PLN,         // net PIT revenue after informal economy evasion
      exciseRevenue: PLN,           // gross excise revenue (sector-weighted rates)
      exciseAfterEvasion: PLN,      // net excise revenue after informal economy evasion
      customsDutyRevenue: PLN,      // customs duty on non-EU imports
      realizedTaxShadowShare: Share, // current-period realized aggregate tax-side shadow share
  )

  def compute(in: Input)(using p: SimParams): Output =
    val realizedTaxShadowShare = InformalEconomy.aggregateTaxShadowShare(Share(in.informalCyclicalAdj))
    val effectiveVatRate       = p.fiscal.fofConsWeights.zip(p.fiscal.vatRates).map((w, r) => r * w).foldLeft(Rate.Zero)(_ + _)
    val effectiveExciseRate    = p.fiscal.fofConsWeights.zip(p.fiscal.exciseRates).map((w, r) => r * w).foldLeft(Rate.Zero)(_ + _)
    val vat                    = in.consumption * effectiveVatRate
    val exciseRevenue          = in.consumption * effectiveExciseRate
    val customsDutyRevenue     = in.totalImports * p.fiscal.customsNonEuShare * p.fiscal.customsDutyRate
    val vatRetention           = Share.One - (realizedTaxShadowShare * p.informal.vatEvasion)
    val exciseRetention        = Share.One - (realizedTaxShadowShare * p.informal.exciseEvasion)
    val pitRetention           = Share.One - (realizedTaxShadowShare * p.informal.pitEvasion)
    val vatAfterEvasion        = vat * vatRetention
    val exciseAfterEvasion     = exciseRevenue * exciseRetention
    val pitAfterEvasion        = in.pitRevenue * pitRetention

    Output(
      vat = vat,
      vatAfterEvasion = vatAfterEvasion,
      pitAfterEvasion = pitAfterEvasion,
      exciseRevenue = exciseRevenue,
      exciseAfterEvasion = exciseAfterEvasion,
      customsDutyRevenue = customsDutyRevenue,
      realizedTaxShadowShare = realizedTaxShadowShare,
    )
