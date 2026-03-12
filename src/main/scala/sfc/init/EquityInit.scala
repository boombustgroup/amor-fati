package sfc.init

import sfc.config.SimParams
import sfc.engine.markets.EquityMarket
import sfc.types.*

/** Factory for equity market state initialization. */
object EquityInit:

  def create(totalPop: Int)(using p: SimParams): EquityMarket.State =
    if p.flags.gpw then
      val initHhEq =
        if p.flags.gpwHhEquity
        then PLN(totalPop.toDouble * Math.exp(p.household.savingsMu) * 0.05) * p.equity.hhEquityFrac
        else PLN.Zero
      EquityMarket.initial.copy(hhEquityWealth = initHhEq)
    else EquityMarket.zero
