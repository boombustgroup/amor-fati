package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Commercial banking system: balance sheets, credit risk, LCR/NSFR,
  * macroprudential, and KNF/BFG supervision.
  *
  * Models a multi-bank system (7 banks by default, calibrated to KNF 2024) with
  * heterogeneous balance sheets, credit spreads, NPL dynamics, capital adequacy
  * (Basel III CRR), liquidity coverage (LCR/NSFR), macroprudential buffers
  * (CCyB, O-SII), KNF BION/SREP P2R add-ons, BFG resolution levy and bail-in,
  * and interbank market.
  *
  * Stock values (`initCapital`, `initDeposits`, etc.) are in raw PLN — scaled
  * by `gdpRatio` in `SimParams.defaults`.
  *
  * @param initCapital
  *   initial aggregate bank equity (KNF 2024: ~270 mld PLN)
  * @param initDeposits
  *   initial aggregate deposits (NBP M3 2024: ~1,900 mld PLN)
  * @param initLoans
  *   initial aggregate corporate loans (NBP 2024: ~700 mld PLN)
  * @param initGovBonds
  *   initial commercial bank government bond holdings (NBP 2024: ~400 mld PLN)
  * @param initNbpGovBonds
  *   initial NBP government bond holdings (NBP 2024: ~300 mld PLN)
  * @param initConsumerLoans
  *   initial consumer loan stock (BIK 2024: ~200 mld PLN)
  * @param baseSpread
  *   base lending spread over policy rate
  * @param nplSpreadFactor
  *   spread increase per unit NPL ratio
  * @param minCar
  *   minimum capital adequacy ratio (Basel III CRR: 8%)
  * @param loanRecovery
  *   loss-given-default recovery rate on corporate loans
  * @param profitRetention
  *   fraction of bank profits retained as capital
  * @param reserveReq
  *   required reserve ratio (NBP 2024: 3.5%)
  * @param stressThreshold
  *   CAR threshold below which bank enters stress mode
  * @param lcrMin
  *   minimum Liquidity Coverage Ratio (Basel III: 100%)
  * @param nsfrMin
  *   minimum Net Stable Funding Ratio (Basel III: 100%)
  * @param demandDepositRunoff
  *   LCR assumption: fraction of demand deposits that may run off in 30 days
  * @param termDepositFrac
  *   fraction of deposits that are term (stable for NSFR purposes)
  * @param p2rAddons
  *   per-bank BION/SREP P2R capital add-ons (KNF 2024, 7 banks)
  * @param bfgLevyRate
  *   annual BFG resolution fund levy as fraction of deposits (BFG 2024)
  * @param bailInDepositHaircut
  *   fraction of uninsured deposits bailed-in during resolution
  * @param bfgDepositGuarantee
  *   BFG deposit guarantee limit per depositor (PLN, BFG: 400,000)
  * @param ccybMax
  *   maximum countercyclical capital buffer (KNF 2024: 2.5%)
  * @param ccybActivationGap
  *   credit/GDP gap threshold to activate CCyB
  * @param ccybReleaseGap
  *   credit/GDP gap threshold to release CCyB
  * @param osiiPkoBp
  *   O-SII buffer for PKO BP (KNF 2024: 1.0%)
  * @param osiiPekao
  *   O-SII buffer for Pekao (KNF 2024: 0.5%)
  * @param concentrationLimit
  *   single-name concentration limit as fraction of capital (Art. 395 CRR: 25%)
  * @param htmShare
  *   fraction of gov bond portfolio classified Held-to-Maturity (NBP 2024:
  *   ~60%)
  * @param htmForcedSaleThreshold
  *   LCR threshold (as fraction of lcrMin) below which HTM bonds are forcibly
  *   reclassified to AFS, realizing hidden mark-to-market losses (interest rate
  *   risk channel)
  * @param htmForcedSaleRate
  *   fraction of HTM portfolio reclassified to AFS per month under LCR stress
  * @param initHtmBookYield
  *   weighted-average acquisition yield on initial HTM portfolio (Polish 10Y at
  *   model start, MF 2024)
  */
case class BankingConfig(
    // Initial balance sheet (raw — scaled by gdpRatio in SimParams.defaults)
    initCapital: PLN = PLN(270e9),
    initDeposits: PLN = PLN(1900e9),
    initLoans: PLN = PLN(700e9),
    initGovBonds: PLN = PLN(400e9),
    initNbpGovBonds: PLN = PLN(300e9),
    initConsumerLoans: PLN = PLN(200e9),
    // Spreads & risk
    baseSpread: Rate = Rate(0.015),
    nplSpreadFactor: Double = 5.0,
    minCar: Ratio = Ratio(0.08),
    loanRecovery: Ratio = Ratio(0.30),
    firmLoanAmortRate: Rate = Rate(1.0 / 60), // monthly: 1/60 ≈ 5-year avg maturity (NBP 2024)
    profitRetention: Ratio = Ratio(0.30),
    govBondDuration: Double = 4.5,            // avg modified duration of Polish gov bond portfolio (years, MF 2024)
    reserveReq: Rate = Rate(0.035),
    stressThreshold: Ratio = Ratio(0.05),
    // LCR/NSFR (Basel III)
    lcrMin: Double = 1.0,
    nsfrMin: Double = 1.0,
    demandDepositRunoff: Ratio = Ratio(0.10),
    termDepositFrac: Ratio = Ratio(0.40),
    // KNF/BFG
    p2rAddons: Vector[Rate] = Vector(Rate(0.015), Rate(0.010), Rate(0.030), Rate(0.015), Rate(0.020), Rate(0.025), Rate(0.020)),
    bfgLevyRate: Rate = Rate(0.0024),
    bailInDepositHaircut: Ratio = Ratio(0.08),
    bfgDepositGuarantee: PLN = PLN(400000.0),
    // Macroprudential (KNF 2024)
    ccybMax: Rate = Rate(0.025),
    ccybActivationGap: Ratio = Ratio(0.02),
    ccybReleaseGap: Double = -0.02,
    osiiPkoBp: Rate = Rate(0.01),
    osiiPekao: Rate = Rate(0.005),
    concentrationLimit: Ratio = Ratio(0.25),
    // AFS/HTM bond portfolio split (interest rate risk channel)
    htmShare: Ratio = Ratio(0.60),
    htmForcedSaleThreshold: Double = 0.75,
    htmForcedSaleRate: Ratio = Ratio(0.10),
    initHtmBookYield: Rate = Rate(0.055),
):
  require(minCar > Ratio.Zero && minCar < Ratio.One, s"minCar must be in (0,1): $minCar")
  require(initCapital >= PLN.Zero, s"initCapital must be non-negative: $initCapital")
  require(initDeposits >= PLN.Zero, s"initDeposits must be non-negative: $initDeposits")
  require(lcrMin > 0, s"lcrMin must be positive: $lcrMin")
  require(nsfrMin > 0, s"nsfrMin must be positive: $nsfrMin")
