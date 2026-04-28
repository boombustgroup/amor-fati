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
    val outputs = Array.fill(sectorCount)(PLN.Zero)
    var i       = 0
    while i < firms.length do
      val firm = firms(i)
      if Firm.isAlive(firm) then
        val idx = firm.sector.toInt
        if idx >= 0 && idx < sectorCount then outputs(idx) = outputs(idx) + Firm.computeCapacity(firm) * sectorMultiplier(idx)
        else
          throw IllegalArgumentException(
            s"Invalid sector id ${firm.sector.toInt} for firm ${firm.id.toInt}; expected 0 until $sectorCount",
          )
      i += 1
    outputs.iterator.map(output => priceLevel * output).toVector

  def outputBasedMonthlyGdp(sectorOutputs: Vector[PLN], inventoryChange: PLN): PLN =
    (sectorOutputs.sumPln + inventoryChange).max(PLN.Zero)
