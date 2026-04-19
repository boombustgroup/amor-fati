package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.Nbfi

/** Factory for NBFI (shadow banking) state initialization. */
object NbfiInit:

  def create(): Nbfi.State =
    Nbfi.initial
