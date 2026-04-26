package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Quasi-fiscal entities: BGK + PFR consolidated configuration.
  *
  * BGK (Bank Gospodarstwa Krajowego) and PFR (Polski Fundusz Rozwoju) issue
  * state-guaranteed bonds off the MF balance sheet to finance infrastructure,
  * crisis programs, and subsidized lending. ~200 mld PLN outstanding (NIK
  * 2024).
  *
  * @param issuanceShare
  *   fraction of gov capital spending routed through BGK/PFR (NIK 2024: ~40%)
  * @param avgMaturityMonths
  *   average maturity of BGK/PFR bonds (longer than SPW, ~72 months / 6 years)
  * @param nbpAbsorptionShare
  *   share of new issuance bought by NBP when quasi-QE active (COVID: ~70%)
  * @param lendingShare
  *   fraction of issuance directed to subsidized lending (BGK kredyty, ~50%)
  * @param loanMaturityMonths
  *   average maturity of BGK subsidized loans (~120 months / 10 years)
  */
case class QuasiFiscalConfig(
    issuanceShare: Share = Share.decimal(40, 2),
    avgMaturityMonths: Int = 72,
    nbpAbsorptionShare: Share = Share.decimal(70, 2),
    lendingShare: Share = Share.decimal(50, 2),
    loanMaturityMonths: Int = 120,
)
