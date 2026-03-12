package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Shadow economy and tax evasion: counter-cyclical informal sector dynamics.
  *
  * Models the Polish informal economy (Schneider 2023: 20-25% of GDP) with
  * 4-channel tax evasion: firm-level CIT evasion, and aggregate VAT/PIT/excise
  * evasion scaled by per-sector shadow shares. The informal share is
  * counter-cyclical — rises with unemployment (lagged, smoothed). Affects SFC
  * Identity 3 (government budget).
  *
  * @param sectorShares
  *   per-sector shadow economy share of output (6 sectors, Schneider 2023: Agri
  *   35%, Retail 30%, etc.)
  * @param citEvasion
  *   fraction of shadow output evading CIT (firm-level channel)
  * @param vatEvasion
  *   fraction of shadow output evading VAT (aggregate channel)
  * @param pitEvasion
  *   fraction of shadow employment evading PIT
  * @param exciseEvasion
  *   fraction of shadow output evading excise duties
  * @param unempThreshold
  *   unemployment rate threshold for counter-cyclical activation
  * @param cyclicalSens
  *   sensitivity of shadow share to excess unemployment above threshold
  * @param smoothing
  *   exponential smoothing parameter for lagged unemployment (higher = more
  *   inertia)
  */
case class InformalConfig(
    sectorShares: Vector[Ratio] = Vector(Ratio(0.05), Ratio(0.15), Ratio(0.30), Ratio(0.20), Ratio(0.02), Ratio(0.35)),
    citEvasion: Ratio = Ratio(0.80),
    vatEvasion: Ratio = Ratio(0.90),
    pitEvasion: Ratio = Ratio(0.85),
    exciseEvasion: Ratio = Ratio(0.70),
    unempThreshold: Rate = Rate(0.05),
    cyclicalSens: Ratio = Ratio(0.50),
    smoothing: Ratio = Ratio(0.92),
):
  require(sectorShares.length == 6, s"sectorShares must have 6 sectors: ${sectorShares.length}")
