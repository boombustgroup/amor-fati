package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.HousingMarket

/** Factory for housing market state initialization. */
object HousingInit:

  def create()(using p: SimParams): HousingMarket.State =
    HousingMarket.initial
