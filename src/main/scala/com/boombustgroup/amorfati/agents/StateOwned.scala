package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** State-owned enterprise (SOE) behavior: Skarb Państwa ownership effects.
  *
  * ~30% of GPW market cap is state-controlled (PGE, PKN Orlen, KGHM, PKO BP,
  * PZU, JSW). SOEs behave differently from private firms in four channels:
  *
  *   1. '''Dividend policy''' — driven by budget needs, not shareholder value.
  *      SOE dividend payout = max(normalPayout, govDemand). Government extracts
  *      higher dividends when fiscal deficit is large.
  *   2. '''Political employment buffer''' — SOEs resist layoffs during
  *      downturns (political cost of unemployment). Reduced firing rate
  *      relative to private firms.
  *   3. '''Directed investment''' — SOEs invest in government priority sectors
  *      (energy transition, defense, infrastructure) regardless of ROI. Higher
  *      investment rate than profit-maximizing firms.
  *   4. '''Energy price subsidies''' — energy SOEs (PGE, Tauron) absorb price
  *      shocks rather than passing to consumers. Reduces measured inflation but
  *      erodes SOE profitability.
  *
  * Pure functions — no mutable state. Applied as modifiers to standard firm
  * behavior in FirmProcessingStep.
  *
  * Calibration: MF dividend revenue data, GUS employment by ownership, NIK SOE
  * investment reports.
  */
object StateOwned:

  /** SOE dividend extraction: government takes higher dividends when deficit is
    * large. Returns dividend multiplier (>= 1.0 for SOEs).
    */
  def dividendMultiplier(deficitToGdp: Ratio)(using p: SimParams): Double =
    val baseMult     = p.soe.baseDividendMultiplier
    val fiscalDemand = (deficitToGdp - p.soe.dividendFiscalThreshold).max(Ratio.Zero).toDouble * p.soe.dividendFiscalSensitivity
    baseMult + fiscalDemand

  /** SOE employment buffer: reduced firing rate during downturns. Returns
    * fraction of normal firing that SOE actually executes.
    */
  def firingReduction(using p: SimParams): Ratio = p.soe.firingReduction

  /** SOE directed investment: higher investment rate than private sector.
    * Returns investment multiplier (>= 1.0).
    */
  def investmentMultiplier(using p: SimParams): Double = p.soe.investmentMultiplier

  /** SOE energy price absorption: energy SOEs absorb price shocks. Returns
    * fraction of commodity price shock passed to consumers.
    */
  def energyPassthrough(using p: SimParams): Ratio = p.soe.energyPassthrough

  /** Probability that a firm in given sector is state-owned at initialization.
    * Based on actual SOE presence per sector (GUS/GPW 2024).
    */
  def sectorSoeShare(sectorIdx: Int): Ratio = sectorIdx match
    case 0 => Ratio(0.05) // BPO — minimal SOE
    case 1 => Ratio(0.20) // Manufacturing — Orlen, KGHM, JSW
    case 2 => Ratio(0.05) // Retail — minimal
    case 3 => Ratio(0.30) // Healthcare — public hospitals
    case 4 => Ratio(0.80) // Public — by definition
    case 5 => Ratio(0.15) // Agriculture — KOWR, ANR
    case _ => Ratio(0.10)
