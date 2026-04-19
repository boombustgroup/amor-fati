package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.Insurance

/** Factory for insurance sector state initialization. */
object InsuranceInit:

  def create(): Insurance.State =
    Insurance.initial
