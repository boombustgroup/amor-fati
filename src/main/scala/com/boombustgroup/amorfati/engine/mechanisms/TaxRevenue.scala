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
      consumption: Double,        // aggregate household consumption (VAT/excise tax base)
      pitRevenue: Double,         // gross PIT revenue before informal evasion
      totalImports: Double,       // total imports (customs duty tax base)
      informalCyclicalAdj: Double, // lagged cyclical adjustment for shadow economy share
  )

  case class Output(
      vat: Double,                 // gross VAT revenue (sector-weighted effective rates)
      vatAfterEvasion: Double,     // net VAT revenue after informal economy evasion
      pitAfterEvasion: Double,     // net PIT revenue after informal economy evasion
      exciseRevenue: Double,       // gross excise tax revenue (sector-weighted rates)
      exciseAfterEvasion: Double,  // net excise revenue after informal economy evasion
      customsDutyRevenue: Double,  // customs duty on non-EU imports
      effectiveShadowShare: Double, // consumption-weighted aggregate shadow economy share
  )

  @boundaryEscape
  def compute(in: Input)(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val weights              = p.fiscal.fofConsWeights.map(toDouble(_))
    val vatRates             = p.fiscal.vatRates.map(toDouble(_))
    val excRates             = p.fiscal.exciseRates.map(toDouble(_))
    val effectiveShadowShare = InformalEconomy.aggregateTaxShadowShare(Share(in.informalCyclicalAdj))
    val effectiveShadowFrac  = toDouble(effectiveShadowShare)

    val vat = in.consumption * weights.zip(vatRates).map((w, r) => w * r).sum

    val exciseRevenue = in.consumption * weights.zip(excRates).map((w, r) => w * r).sum

    val customsDutyRevenue =
      in.totalImports * toDouble(p.fiscal.customsNonEuShare) * toDouble(p.fiscal.customsDutyRate)

    val vatAfterEvasion =
      if p.flags.informal then vat * (1.0 - effectiveShadowFrac * toDouble(p.informal.vatEvasion)) else vat

    val exciseAfterEvasion =
      if p.flags.informal then exciseRevenue * (1.0 - effectiveShadowFrac * toDouble(p.informal.exciseEvasion))
      else exciseRevenue

    val pitAfterEvasion =
      if p.flags.informal then in.pitRevenue * (1.0 - effectiveShadowFrac * toDouble(p.informal.pitEvasion))
      else in.pitRevenue

    Output(
      vat = vat,
      vatAfterEvasion = vatAfterEvasion,
      pitAfterEvasion = pitAfterEvasion,
      exciseRevenue = exciseRevenue,
      exciseAfterEvasion = exciseAfterEvasion,
      customsDutyRevenue = customsDutyRevenue,
      effectiveShadowShare = effectiveShadowFrac,
    )
