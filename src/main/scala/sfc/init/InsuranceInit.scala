package sfc.init

import sfc.agents.Insurance
import sfc.config.SimParams

/** Factory for insurance sector state initialization. */
object InsuranceInit:

  def create()(using p: SimParams): Insurance.State =
    if p.flags.insurance then Insurance.initial
    else Insurance.zero
