package com.boombustgroup.amorfati.config

/** Mechanism toggles for 49 model features.
  *
  * Each flag enables/disables an independently switchable mechanism in the
  * SFC-ABM engine. Defaults reflect the full Polish economy baseline. Flags are
  * used exclusively for counterfactual experiments — not for reproducing legacy
  * behavior.
  *
  * Grouped by domain:
  *
  * '''Firm & production:''' `firmEntry`, `physCap`, `inventory`, `informal`,
  * `energy`
  *
  * '''Government:''' `govInvest`, `euFunds`, `minWage`, `govBondMarket`,
  * `govUnempBenefit`, `pit`, `social800`, `social800ImmigEligible`
  *
  * '''Central bank (NBP):''' `nbpSymmetric`, `nbpForwardGuidance`, `nbpQe`,
  * `nbpFxIntervention`
  *
  * '''Banking:''' `bankFailure`, `bankLcr`, `interbankTermStructure`,
  * `creditDiagnostics`, `bailIn`, `macropru`, `jst`
  *
  * '''Social:''' `zus`, `ppk`, `demographics`
  *
  * '''Markets:''' `io`, `gpw`, `gpwEquityIssuance`, `gpwHhEquity`,
  * `gpwDividends`
  *
  * '''External:''' `openEcon`, `gvc`, `immigration`, `immigEndogenous`, `fdi`,
  * `remittance`, `tourism`
  *
  * '''Financial:''' `insurance`, `nbfi`, `re`, `reMortgage`, `reHhHousing`,
  * `reRegional`
  *
  * '''Labor:''' `sectoralMobility`, `unions`, `expectations`
  */
case class FeatureFlags(
    // Firm & production
    firmEntry: Boolean = false,
    physCap: Boolean = true,
    inventory: Boolean = false,
    informal: Boolean = false,
    energy: Boolean = false,
    // Government
    govInvest: Boolean = false,
    euFunds: Boolean = false,
    minWage: Boolean = false,
    govBondMarket: Boolean = true,
    govUnempBenefit: Boolean = true,
    pit: Boolean = false,
    social800: Boolean = false,
    social800ImmigEligible: Boolean = true,
    // Central bank (NBP)
    nbpSymmetric: Boolean = true,
    nbpForwardGuidance: Boolean = false,
    nbpQe: Boolean = true,
    nbpFxIntervention: Boolean = false,
    // Banking
    bankFailure: Boolean = false,
    bankLcr: Boolean = false,
    interbankTermStructure: Boolean = false,
    creditDiagnostics: Boolean = false,
    bailIn: Boolean = false,
    macropru: Boolean = false,
    jst: Boolean = false,
    // Social
    zus: Boolean = false,
    ppk: Boolean = false,
    demographics: Boolean = false,
    // Markets
    io: Boolean = false,
    gpw: Boolean = false,
    gpwEquityIssuance: Boolean = false,
    gpwHhEquity: Boolean = false,
    gpwDividends: Boolean = false,
    // External
    openEcon: Boolean = false,
    gvc: Boolean = false,
    immigration: Boolean = false,
    immigEndogenous: Boolean = false,
    fdi: Boolean = false,
    remittance: Boolean = false,
    tourism: Boolean = false,
    // Financial
    insurance: Boolean = false,
    nbfi: Boolean = false,
    re: Boolean = false,
    reMortgage: Boolean = true,
    reHhHousing: Boolean = true,
    reRegional: Boolean = false,
    // Labor
    sectoralMobility: Boolean = false,
    unions: Boolean = false,
    expectations: Boolean = false,
    sbtc: Boolean = true,
)
