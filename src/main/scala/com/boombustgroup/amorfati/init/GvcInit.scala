package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.GvcTrade

/** Factory for GVC trade state initialization. */
object GvcInit:

  def create()(using p: SimParams): GvcTrade.State =
    if true then GvcTrade.initial
    else GvcTrade.zero
