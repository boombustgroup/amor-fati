package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.SocialSecurity
import com.boombustgroup.amorfati.config.SimParams

/** Factory for demographics state initialization. */
object DemographicsInit:

  def create(totalPop: Int)(using p: SimParams): SocialSecurity.DemographicsState =
    if true then SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
    else if true && p.social.demInitialRetirees > 0 then SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
    else SocialSecurity.DemographicsState.zero
