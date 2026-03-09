package sfc.init

import sfc.agents.SocialSecurity
import sfc.config.SimParams

/** Factory for demographics state initialization. */
object DemographicsInit:

  def create(totalPop: Int)(using p: SimParams): SocialSecurity.DemographicsState =
    if p.flags.demographics then SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
    else if p.flags.zus && p.social.demInitialRetirees > 0 then SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
    else SocialSecurity.DemographicsState.zero
