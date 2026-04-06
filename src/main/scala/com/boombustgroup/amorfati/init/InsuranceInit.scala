package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.Insurance
import com.boombustgroup.amorfati.config.SimParams

/** Factory for insurance sector state initialization. */
object InsuranceInit:

  def create()(using p: SimParams): Insurance.State =
    Insurance.initial
