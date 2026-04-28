package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Production-side GDP diagnostics.
  *
  * `MonthlyGdpProxy` is intentionally anchored to realized output, not desired
  * expenditure. Demand pressure can exceed capacity, but GDP should not count
  * unmet household or government demand as production.
  */
object GdpAccounting:

  def realizedSectorOutputs(
      priceLevel: PriceIndex,
      sectorCount: Int,
      firms: Vector[Firm.State],
      sectorMultiplier: Int => Multiplier,
  )(using p: SimParams): Vector[PLN] =
    val livingBySector = firms.iterator.filter(Firm.isAlive).toVector.groupBy(_.sector.toInt)
    Vector.tabulate(sectorCount): s =>
      livingBySector
        .getOrElse(s, Vector.empty)
        .foldLeft(PLN.Zero): (acc, f) =>
          acc + (priceLevel * (Firm.computeCapacity(f) * sectorMultiplier(f.sector.toInt)))

  def outputBasedMonthlyGdp(sectorOutputs: Vector[PLN], inventoryChange: PLN): PLN =
    (sectorOutputs.sumPln + inventoryChange).max(PLN.Zero)
