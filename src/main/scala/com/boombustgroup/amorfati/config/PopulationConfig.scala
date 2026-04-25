package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Firm size distribution strategy used to compute average workers per firm and
  * gdpRatio.
  *   - `Uniform`: all firms have `workersPerFirm` employees (analytical
  *     baseline)
  *   - `Gus`: realistic micro/small/medium/large split calibrated to GUS
  *     CEIDG/KRS 2024
  */
enum FirmSizeDist:
  case Uniform, Gus

/** Agent population and firm size distribution.
  *
  * Controls the number of heterogeneous firms and the mapping from firm count
  * to total employment, which in turn determines `gdpRatio` — the scaling
  * factor that maps agent-level flows to real Polish GDP (~3.5 bln PLN, GUS
  * 2024).
  *
  * @param firmsCount
  *   number of firm agents in the simulation
  * @param workersPerFirm
  *   workers per firm under `Uniform` distribution (also used as normalizing
  *   base for `Gus`)
  * @param firmSizeDist
  *   size distribution strategy: `Uniform` (equal sizes) or `Gus` (realistic
  *   micro/small/medium/large split)
  * @param firmSizeMicroShare
  *   share of micro-enterprises (1-9 employees) under `Gus` distribution (GUS
  *   CEIDG 2024: 96.2%)
  * @param firmSizeSmallShare
  *   share of small firms (10-49 employees) under `Gus` (GUS: 2.8%)
  * @param firmSizeMediumShare
  *   share of medium firms (50-249 employees) — derived as residual (GUS: 0.8%)
  * @param firmSizeLargeShare
  *   share of large firms (250+ employees) under `Gus` (GUS: 0.2%)
  * @param firmSizeLargeMax
  *   upper bound on headcount draw for large firms
  * @param realGdp
  *   Polish real GDP in PLN used to compute `gdpRatio` (GUS 2024: ~3.5 bln PLN)
  */
case class PopulationConfig(
    firmsCount: Int = 10000,
    workersPerFirm: Int = 10,
    firmSizeDist: FirmSizeDist = FirmSizeDist.Gus,
    firmSizeMicroShare: Share = Share("0.962"),
    firmSizeSmallShare: Share = Share("0.028"),
    firmSizeMediumShare: Share = Share("0.008"),
    firmSizeLargeShare: Share = Share("0.002"),
    firmSizeLargeMax: Int = 1000,
    realGdp: PLN = PLN("3500e9"),
):
  require(firmsCount > 0, s"firmsCount must be positive: $firmsCount")
  require(workersPerFirm > 0, s"workersPerFirm must be positive: $workersPerFirm")
  require(realGdp > PLN.Zero, s"realGdp must be positive: $realGdp")
  require(firmSizeLargeMax >= 250, s"firmSizeLargeMax must be >= 250: $firmSizeLargeMax")
