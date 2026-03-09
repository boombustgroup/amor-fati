package sfc.init

import sfc.config.SimParams
import sfc.engine.markets.EquityMarket
import sfc.types.*

/** Factory for equity market state initialization. */
object EquityInit:

  def create(totalPop: Int)(using p: SimParams): EquityMarket.State =
    if p.flags.gpw then
      val initEq   = EquityMarket.initial
      val initHhEq =
        if p.flags.gpwHhEquity then PLN(totalPop * p.equity.hhEquityFrac.toDouble * Math.exp(p.household.savingsMu) * 0.05)
        else PLN.Zero
      initEq.copy(hhEquityWealth = initHhEq)
    else EquityMarket.zero
