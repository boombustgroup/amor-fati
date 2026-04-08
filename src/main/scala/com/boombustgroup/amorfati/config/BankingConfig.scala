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
  * @param depositFlightSensitivity
  *   sensitivity of deposit switching to CAR shortfall below threshold
  * @param depositFlightCarThreshold
  *   CAR level below which depositors start leaving (KNF stress: ~10%)
  * @param depositPanicRate
  *   fraction of depositors who panic-switch when any bank fails (Diamond &
  *   Dybvig 1983)
  * @param maxDepositSwitchRate
  *   maximum fraction of HH that can switch banks per month (structural cap)
  * @param interbankRecoveryRate
  *   recovery rate on interbank exposures when counterparty fails (NBP FSR:
  *   ~40%, secured/unsecured mix)
  * @param hoardingNplThreshold
  *   system NPL ratio above which banks start hoarding liquidity (reducing
  *   interbank lending)
  * @param hoardingSensitivity
  *   speed of hoarding onset: factor = 1 − sensitivity × (NPL − threshold). At
  *   10.0, a 10pp NPL overshoot → full freeze.
  * @param eclRate1
  *   Stage 1 (performing) ECL provision rate (12-month ECL, KNF: ~1%)
  * @param eclRate2
  *   Stage 2 (watch) ECL provision rate (lifetime ECL, KNF: ~8%)
  * @param eclRate3
  *   Stage 3 (default) ECL provision rate (1 − recovery, KNF: ~50%)
  * @param eclMigrationSensitivity
  *   sensitivity of S1→S2 migration to unemployment excess above NAIRU
  * @param eclGdpSensitivity
  *   sensitivity of S1→S2 migration to GDP contraction
  * @param eclMaxMigration
  *   maximum monthly S1→S2 migration rate (structural cap)
  * @param eclCureRate
  *   monthly cure rate: fraction of S3 loans returning to S2 (restructuring)
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
    nplSpreadFactor: Multiplier = Multiplier(5.0),
    minCar: Multiplier = Multiplier(0.08),
    loanRecovery: Share = Share(0.30),
    firmLoanAmortRate: Rate = Rate(1.0 / 60),      // monthly: 1/60 ≈ 5-year avg maturity (NBP 2024)
    profitRetention: Share = Share(0.30),
    govBondDuration: Multiplier = Multiplier(4.5), // avg modified duration of Polish gov bond portfolio (years, MF 2024)
    reserveReq: Share = Share(0.035),
    stressThreshold: Share = Share(0.05),
    // LCR/NSFR (Basel III)
    lcrMin: Multiplier = Multiplier(1.0),
    nsfrMin: Multiplier = Multiplier(1.0),
    demandDepositRunoff: Share = Share(0.10),
    termDepositFrac: Share = Share(0.40),
    // KNF/BFG
    p2rAddons: Vector[Multiplier] =
      Vector(Multiplier(0.015), Multiplier(0.010), Multiplier(0.030), Multiplier(0.015), Multiplier(0.020), Multiplier(0.025), Multiplier(0.020)),
    bfgLevyRate: Rate = Rate(0.0024),
    bailInDepositHaircut: Share = Share(0.08),
    bfgDepositGuarantee: PLN = PLN(400000.0),
    // Macroprudential (KNF 2024)
    ccybMax: Multiplier = Multiplier(0.025),
    ccybActivationGap: Coefficient = Coefficient(0.02),
    ccybReleaseGap: Coefficient = Coefficient(-0.02),
    osiiPkoBp: Multiplier = Multiplier(0.01),
    osiiPekao: Multiplier = Multiplier(0.005),
    concentrationLimit: Share = Share(0.25),
    // AFS/HTM bond portfolio split (interest rate risk channel)
    htmShare: Share = Share(0.60),
    htmForcedSaleThreshold: Share = Share(0.75),
    htmForcedSaleRate: Share = Share(0.10),
    initHtmBookYield: Rate = Rate(0.055),
    // Deposit mobility (Diamond & Dybvig 1983)
    depositFlightSensitivity: Coefficient = Coefficient(5.0),
    depositFlightCarThreshold: Multiplier = Multiplier(0.10),
    depositPanicRate: Share = Share(0.03),
    maxDepositSwitchRate: Share = Share(0.10),
    // Interbank contagion
    interbankRecoveryRate: Share = Share(0.40),
    hoardingNplThreshold: Share = Share(0.05),
    hoardingSensitivity: Multiplier = Multiplier(10.0),
    // IFRS 9 ECL staging
    eclRate1: Share = Share(0.01),
    eclRate2: Share = Share(0.08),
    eclRate3: Share = Share(0.50),
    eclMigrationSensitivity: Coefficient = Coefficient(3.0),
    eclGdpSensitivity: Coefficient = Coefficient(5.0),
    eclMaxMigration: Share = Share(0.20),
    eclCureRate: Share = Share(0.02),
):
  require(minCar > Multiplier.Zero && minCar < Multiplier.One, s"minCar must be in (0,1): $minCar")
  require(initCapital >= PLN.Zero, s"initCapital must be non-negative: $initCapital")
  require(initDeposits >= PLN.Zero, s"initDeposits must be non-negative: $initDeposits")
  require(lcrMin > Multiplier.Zero, s"lcrMin must be positive: $lcrMin")
  require(nsfrMin > Multiplier.Zero, s"nsfrMin must be positive: $nsfrMin")
