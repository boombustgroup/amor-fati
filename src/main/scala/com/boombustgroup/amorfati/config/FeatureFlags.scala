package com.boombustgroup.amorfati.config

/** Mechanism toggles for 49 model features.
  *
  * Each flag enables/disables an independently switchable mechanism in the
  * SFC-ABM engine. Defaults reflect the full Polish economy baseline — all
  * mechanisms enabled. Flags are used exclusively for counterfactual
  * experiments — toggle OFF to isolate a channel's contribution.
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
  * '''Social:''' `nfz`, `zus`, `ppk`, `demographics`
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
  * '''Labor:''' `sectoralMobility`, `unions`, `expectations`, `sbtc`
  */
case class FeatureFlags(
    // Firm & production
    firmEntry: Boolean = true,
    physCap: Boolean = true,
    inventory: Boolean = true,
    informal: Boolean = true,
    energy: Boolean = true,
    // Government
    govInvest: Boolean = true,
    euFunds: Boolean = true,
    minWage: Boolean = true,
    govBondMarket: Boolean = true,
    quasiFiscal: Boolean = true,
    govUnempBenefit: Boolean = true,
    pit: Boolean = true,
    social800: Boolean = true,
    social800ImmigEligible: Boolean = true,
    // Central bank (NBP)
    nbpSymmetric: Boolean = true,
    nbpForwardGuidance: Boolean = true,
    nbpQe: Boolean = true,
    nbpFxIntervention: Boolean = true,
    // Banking
    bankFailure: Boolean = true,
    bankLcr: Boolean = true,
    interbankTermStructure: Boolean = true,
    creditDiagnostics: Boolean = true,
    bailIn: Boolean = true,
    macropru: Boolean = true,
    jst: Boolean = true,
    // Social
    nfz: Boolean = true,
    zus: Boolean = true,
    ppk: Boolean = true,
    demographics: Boolean = true,
    // Markets
    io: Boolean = true,
    gpw: Boolean = true,
    gpwEquityIssuance: Boolean = true,
    gpwHhEquity: Boolean = true,
    gpwDividends: Boolean = true,
    // External
    openEcon: Boolean = true,
    gvc: Boolean = true,
    immigration: Boolean = true,
    immigEndogenous: Boolean = true,
    fdi: Boolean = true,
    remittance: Boolean = true,
    tourism: Boolean = true,
    // Financial
    insurance: Boolean = true,
    nbfi: Boolean = true,
    re: Boolean = true,
    reMortgage: Boolean = true,
    reHhHousing: Boolean = true,
    reRegional: Boolean = true,
    // Labor
    sectoralMobility: Boolean = true,
    unions: Boolean = true,
    expectations: Boolean = true,
    sbtc: Boolean = true,
)
