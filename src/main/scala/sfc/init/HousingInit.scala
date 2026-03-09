package sfc.init

import sfc.config.SimParams
import sfc.engine.markets.HousingMarket

/** Factory for housing market state initialization. */
object HousingInit:

  def create()(using p: SimParams): HousingMarket.State =
    if p.flags.re then HousingMarket.initial
    else HousingMarket.zero
