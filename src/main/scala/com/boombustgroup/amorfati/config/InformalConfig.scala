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
  * The defaults below place the baseline aggregate tax-side shadow share near
  * the middle of the stated 20-25% target band, while keeping the cyclical
  * uplift moderate in the default runtime path.
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
    sectorShares: Vector[Share] =
      Vector(Share.decimal(5, 2), Share.decimal(15, 2), Share.decimal(30, 2), Share.decimal(20, 2), Share.decimal(2, 2), Share.decimal(35, 2)),
    citEvasion: Share = Share.decimal(80, 2),
    vatEvasion: Share = Share.decimal(90, 2),
    pitEvasion: Share = Share.decimal(85, 2),
    exciseEvasion: Share = Share.decimal(70, 2),
    unempThreshold: Rate = Rate.decimal(5, 2),
    cyclicalSens: Coefficient = Coefficient.decimal(50, 2),
    smoothing: Coefficient = Coefficient.decimal(92, 2),
):
  require(sectorShares.length == 6, s"sectorShares must have 6 sectors: ${sectorShares.length}")
