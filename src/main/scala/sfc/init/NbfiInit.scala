package sfc.init

import sfc.agents.Nbfi
import sfc.config.SimParams

/** Factory for NBFI (shadow banking) state initialization. */
object NbfiInit:

  def create()(using p: SimParams): Nbfi.State =
    if p.flags.nbfi then Nbfi.initial
    else Nbfi.State.zero
