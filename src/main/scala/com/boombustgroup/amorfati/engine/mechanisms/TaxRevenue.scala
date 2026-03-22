package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams

/** Tax revenue computation: VAT, excise, customs, and informal-economy evasion
  * adjustments.
  *
  * Extracted from BankUpdateStep to separate fiscal revenue logic from banking
  * balance-sheet updates. All flows are monthly, in PLN.
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

  def compute(in: Input)(using p: SimParams): Output =
    val vat = in.consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.vatRates.map(_.toDouble))
      .map((w, r) => w * r)
      .kahanSum

    val exciseRevenue = in.consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.exciseRates.map(_.toDouble))
      .map((w, r) => w * r)
      .kahanSum

    val customsDutyRevenue =
      if p.flags.openEcon then in.totalImports * p.fiscal.customsNonEuShare.toDouble * p.fiscal.customsDutyRate.toDouble
      else 0.0

    // Informal economy: aggregate tax evasion
    val effectiveShadowShare =
      if p.flags.informal then
        p.fiscal.fofConsWeights
          .map(_.toDouble)
          .zip(p.informal.sectorShares.map(_.toDouble))
          .map((cw, ss) => cw * Math.min(1.0, ss + in.informalCyclicalAdj))
          .kahanSum
      else 0.0

    val vatAfterEvasion =
      if p.flags.informal then vat * (1.0 - effectiveShadowShare * p.informal.vatEvasion.toDouble) else vat

    val exciseAfterEvasion =
      if p.flags.informal then exciseRevenue * (1.0 - effectiveShadowShare * p.informal.exciseEvasion.toDouble)
      else exciseRevenue

    val pitAfterEvasion =
      if p.flags.informal then in.pitRevenue * (1.0 - effectiveShadowShare * p.informal.pitEvasion.toDouble)
      else in.pitRevenue

    Output(
      vat = vat,
      vatAfterEvasion = vatAfterEvasion,
      pitAfterEvasion = pitAfterEvasion,
      exciseRevenue = exciseRevenue,
      exciseAfterEvasion = exciseAfterEvasion,
      customsDutyRevenue = customsDutyRevenue,
      effectiveShadowShare = effectiveShadowShare,
    )
