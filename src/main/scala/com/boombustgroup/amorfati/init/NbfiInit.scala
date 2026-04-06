package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.Nbfi
import com.boombustgroup.amorfati.config.SimParams

/** Factory for NBFI (shadow banking) state initialization. */
object NbfiInit:

  def create()(using p: SimParams): Nbfi.State =
    if true then Nbfi.initial
    else Nbfi.State.zero
