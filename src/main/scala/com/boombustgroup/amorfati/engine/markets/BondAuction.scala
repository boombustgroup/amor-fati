package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Government bond auction: domestic + foreign demand, absorption constraint.
  *
  * Models the primary market for Polish government bonds (skarbowe papiery
  * wartościowe, SPW) with two demand channels:
  *
  *   1. '''Foreign demand''' (~35% of SPW stock, NBP 2024) is a function of the
  *      yield spread vs German Bund and PLN exchange rate expectations. Higher
  *      spread attracts carry trade; PLN depreciation deters (risk-off). This
  *      is a natural stabilizer: yield shock → higher spread → more foreign
  *      demand → absorption → lower yield pressure.
  *   2. '''Absorption constraint''' — total demand (domestic + foreign) vs new
  *      issuance. When bid-to-cover ratio < 1, the auction is undersubscribed:
  *      not all bonds are placed, unfunded deficit accumulates as floating
  *      debt. This is the fiscal crisis trigger (cf. Italy 2011, Greece 2010).
  *
  * Pure function — no state, no side effects. Called from BankUpdateStep
  * waterfall to determine how new bond issuance is distributed between domestic
  * holders (banks acting as DSPW primary dealers and final holders) and
  * non-resident investors.
  *
  * Calibration: MF auction data (przetargi SPW), NBP SPW holder structure
  * (rezydenci vs nierezydenci), ECB Bund yields.
  */
object BondAuction:

  /** Auction result: how much was absorbed and by whom. */
  case class Result(
      domesticAbsorbed: PLN, // bonds absorbed by domestic banks
      foreignAbsorbed: PLN,  // bonds absorbed by foreign holders
      bidToCover: Multiplier, // total demand / issuance (< 1 = undersubscribed)
  )

  /** Compute foreign demand share as f(yield spread vs Bund, ER change).
    *
    * foreignShare = baseShare × (1 + yieldSensitivity × spread − erSensitivity
    * × depreciation)
    *
    * Spread = govBondYield − bundYield. Positive spread attracts foreign
    * capital. Depreciation (positive = PLN weakening) deters via currency risk.
    * Clamped to [0, maxForeignShare] to prevent unrealistic extremes.
    */
  @computationBoundary
  private[amorfati] def foreignDemandShare(
      marketYield: Rate,
      erChange: Coefficient,
  )(using p: SimParams): Share =
    import ComputationBoundary.toDouble
    val spread      = marketYield - p.fiscal.bundYield
    val yieldEffect = toDouble(p.fiscal.foreignYieldSensitivity) * toDouble(spread)
    val erEffect    = toDouble(p.fiscal.foreignErSensitivity) * toDouble(erChange)
    val raw         = toDouble(p.fiscal.baseForeignShare) * (1.0 + yieldEffect - erEffect)
    Share(raw.max(0.0).min(toDouble(p.fiscal.maxForeignShare)))

  /** Run the bond auction for this month's issuance.
    *
    * @param newIssuance
    *   total new bonds to place (deficit + rollover, from FiscalBudget)
    * @param bankBondCapacity
    *   maximum additional bonds banks can absorb (available reserves/capital)
    * @param marketYield
    *   current market yield on gov bonds
    * @param erChange
    *   month-on-month PLN exchange rate change (positive = depreciation)
    * @return
    *   AuctionResult with domestic/foreign split and bid-to-cover ratio
    */
  def auction(
      newIssuance: PLN,
      bankBondCapacity: PLN,
      marketYield: Rate,
      erChange: Coefficient,
  )(using p: SimParams): Result =
    if newIssuance <= PLN.Zero then Result(PLN.Zero, PLN.Zero, Multiplier(1.0))
    else
      val foreignShare   = foreignDemandShare(marketYield, erChange)
      val foreignDemand  = newIssuance * foreignShare
      val domesticCap    = bankBondCapacity.max(PLN.Zero)
      val domesticDemand = (newIssuance - foreignDemand).min(domesticCap).max(PLN.Zero)
      val totalDemand    = domesticDemand + foreignDemand
      val coverRatio     = Multiplier((totalDemand / newIssuance).min(10.0))
      val absorbed       = totalDemand.min(newIssuance)
      val foreignActual  = foreignDemand.min(absorbed)
      val domesticActual = absorbed - foreignActual
      Result(domesticActual, foreignActual, coverRatio)
