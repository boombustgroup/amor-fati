package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.types.*

/** Factory for equity market state initialization. */
object EquityInit:

  @boundaryEscape
  def create(totalPop: Int)(using p: SimParams): EquityMarket.State =
    import ComputationBoundary.toDouble
    val initHhEq = PLN(totalPop.toDouble * Math.exp(toDouble(p.household.savingsMu)) * 0.05) * p.equity.hhEquityFrac
    EquityMarket.initial.copy(hhEquityWealth = initHhEq)
