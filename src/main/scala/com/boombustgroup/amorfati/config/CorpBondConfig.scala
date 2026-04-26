package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Corporate bond market (Catalyst): issuance, buyer composition, and default
  * recovery.
  *
  * Models the Polish corporate bond market with demand-side absorption
  * constraint: issuance by qualifying firms (50+ employees), buyer composition
  * split across banks, PPK funds, insurance, NBFI/TFI, and residual other
  * investors. Calibrated to KNF 2024 (Catalyst + non-public), with BBB Polish
  * corporate spread (RRRF 2024). Affects SFC Identity 12.
  *
  * `initStock` is in raw PLN — scaled by `gdpRatio` in `SimParams.defaults`.
  *
  * @param spread
  *   credit spread over policy rate for corporate bonds (RRRF 2024 BBB avg:
  *   ~2.5%)
  * @param initStock
  *   initial outstanding corporate bond stock in raw PLN (KNF 2024: ~90 mld
  *   PLN, scaled by gdpRatio)
  * @param minSize
  *   minimum firm size (employees) for bond issuance eligibility
  * @param issuanceFrac
  *   annual issuance as fraction of outstanding stock
  * @param bankShare
  *   share of corporate bonds held by commercial banks
  * @param ppkShare
  *   share of corporate bonds held by PPK/OFE pension funds
  * @param recovery
  *   recovery rate on defaulted corporate bonds
  * @param maturity
  *   average bond maturity in months
  */
case class CorpBondConfig(
    spread: Rate = Rate.decimal(25, 3),
    initStock: PLN = PLN(90000000000L), // raw — scaled by gdpRatio
    minSize: Int = 50,
    issuanceFrac: Share = Share.decimal(15, 2),
    bankShare: Share = Share.decimal(30, 2),
    ppkShare: Share = Share.decimal(15, 2),
    recovery: Share = Share.decimal(30, 2),
    maturity: Int = 60,
)
