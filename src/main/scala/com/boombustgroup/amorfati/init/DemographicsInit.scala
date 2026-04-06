package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.SocialSecurity
import com.boombustgroup.amorfati.config.SimParams

/** Factory for demographics state initialization. */
object DemographicsInit:

  def create(totalPop: Int)(using p: SimParams): SocialSecurity.DemographicsState =
    SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
