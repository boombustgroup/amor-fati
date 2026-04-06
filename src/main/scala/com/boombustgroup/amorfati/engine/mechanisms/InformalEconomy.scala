package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Shared helpers for aggregate tax-side informal-economy semantics. */
object InformalEconomy:

  /** Consumption-weighted aggregate shadow-economy share used by the current
    * aggregate tax channels (VAT, PIT, excise).
    */
  def aggregateTaxShadowShare(cyclicalAdj: Share)(using p: SimParams): Share =
    p.fiscal.fofConsWeights
      .zip(p.informal.sectorShares)
      .map((cw, ss) => cw * (ss + cyclicalAdj).min(Share.One))
      .foldLeft(Share.Zero)(_ + _)
