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

  private val ManufacturingSector = 1
  private val HealthcareSector    = 3
  private val PublicSector        = 4
  private val AgricultureSector   = 5

  private case class SectorSemantics(
      investmentPriority: Share = Share.Zero,
      energyBufferExposure: Share = Share.Zero,
  )

  private val DefaultSectorSemantics = SectorSemantics()

  private val semanticsBySector: Map[Int, SectorSemantics] = Map(
    ManufacturingSector -> SectorSemantics(investmentPriority = Share.One, energyBufferExposure = Share.One),
    HealthcareSector    -> SectorSemantics(investmentPriority = Share(0.35)),
    PublicSector        -> SectorSemantics(investmentPriority = Share.One, energyBufferExposure = Share(0.35)),
    AgricultureSector   -> SectorSemantics(investmentPriority = Share(0.45)),
  )

  private def sectorSemantics(sectorIdx: Int): SectorSemantics =
    semanticsBySector.getOrElse(sectorIdx, DefaultSectorSemantics)

  /** SOE dividend extraction: government takes higher dividends when deficit is
    * large. Returns dividend multiplier (>= 1.0 for SOEs).
    */
  def dividendMultiplier(deficitToGdp: Share)(using p: SimParams): Multiplier =
    val baseMult     = p.soe.baseDividendMultiplier
    val fiscalDemand = (deficitToGdp - p.soe.dividendFiscalThreshold).max(Share.Zero) * p.soe.dividendFiscalSensitivity
    baseMult + fiscalDemand.toMultiplier

  /** SOE employment buffer: reduced firing rate during downturns. Returns
    * fraction of normal firing that SOE actually executes.
    */
  def firingReduction(using p: SimParams): Share = p.soe.firingReduction

  /** SOE directed investment: higher investment rate than private sector.
    * Returns investment multiplier (>= 1.0).
    */
  def investmentMultiplier(using p: SimParams): Multiplier = p.soe.investmentMultiplier

  /** Strategic-investment priority of an SOE in a given sector.
    *
    * The current 6-sector schema does not have a dedicated utilities bucket, so
    * directed SOE investment is modeled as strongest in Manufacturing and
    * Public, with smaller spillovers to Healthcare and Agriculture.
    */
  def directedInvestmentPriority(sectorIdx: Int): Share =
    sectorSemantics(sectorIdx).investmentPriority

  /** Effective investment multiplier after applying current sector semantics.
    * Non-strategic SOEs fall back to neutral investment behavior.
    */
  def directedInvestmentMultiplier(sectorIdx: Int)(using p: SimParams): Multiplier =
    Multiplier.One + (investmentMultiplier.deviationFromOne * directedInvestmentPriority(sectorIdx)).toMultiplier

  /** SOE energy price absorption: energy SOEs absorb price shocks. Returns
    * fraction of commodity price shock passed to consumers.
    */
  def energyPassthrough(using p: SimParams): Share = p.soe.energyPassthrough

  /** Sector exposure to administered energy-price buffering.
    *
    * With no explicit Energy/Utilities sector in the schema, the behavior is
    * attached to the parts of Manufacturing and Public where the state today
    * plausibly acts as the price-buffering supplier.
    */
  def energyBufferExposure(sectorIdx: Int): Share =
    sectorSemantics(sectorIdx).energyBufferExposure

  /** Effective consumer pass-through after sector semantics are applied.
    * Sectors with no energy-buffer exposure fall back to full pass-through.
    */
  def effectiveEnergyPassthrough(sectorIdx: Int)(using p: SimParams): Share =
    Share.One - (energyBufferExposure(sectorIdx) * (Share.One - energyPassthrough))

  /** Probability that a firm in given sector is state-owned at initialization.
    * Based on actual SOE presence per sector (GUS/GPW 2024).
    */
  def sectorSoeShare(sectorIdx: Int): Share = sectorIdx match
    case 0 => Share(0.05) // BPO — minimal SOE
    case 1 => Share(0.20) // Manufacturing — Orlen, KGHM, JSW
    case 2 => Share(0.05) // Retail — minimal
    case 3 => Share(0.30) // Healthcare — public hospitals
    case 4 => Share(0.80) // Public — by definition
    case 5 => Share(0.15) // Agriculture — KOWR, ANR
    case _ => Share(0.10)
