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
    initCapital: PLN = PLN(270000000000L),
    initDeposits: PLN = PLN(1900000000000L),
    initLoans: PLN = PLN(700000000000L),
    initGovBonds: PLN = PLN(400000000000L),
    initNbpGovBonds: PLN = PLN(300000000000L),
    initConsumerLoans: PLN = PLN(200000000000L),
    // Spreads & risk
    baseSpread: Rate = Rate.decimal(15, 3),
    nplSpreadFactor: Multiplier = Multiplier(5),
    minCar: Multiplier = Multiplier.decimal(8, 2),
    loanRecovery: Share = Share.decimal(30, 2),
    firmLoanAmortRate: Rate = Rate.fraction(1, 60),          // monthly: 1/60 ≈ 5-year avg maturity (NBP 2024)
    profitRetention: Share = Share.decimal(30, 2),
    govBondDuration: Multiplier = Multiplier.decimal(45, 1), // avg modified duration of Polish gov bond portfolio (years, MF 2024)
    reserveReq: Share = Share.decimal(35, 3),
    stressThreshold: Share = Share.decimal(5, 2),
    // LCR/NSFR (Basel III)
    lcrMin: Multiplier = Multiplier(1),
    nsfrMin: Multiplier = Multiplier(1),
    demandDepositRunoff: Share = Share.decimal(10, 2),
    termDepositFrac: Share = Share.decimal(40, 2),
    // KNF/BFG
    p2rAddons: Vector[Multiplier] = Vector(
      Multiplier.decimal(15, 3),
      Multiplier.decimal(10, 3),
      Multiplier.decimal(30, 3),
      Multiplier.decimal(15, 3),
      Multiplier.decimal(20, 3),
      Multiplier.decimal(25, 3),
      Multiplier.decimal(20, 3),
    ),
    bfgLevyRate: Rate = Rate.decimal(24, 4),
    bailInDepositHaircut: Share = Share.decimal(8, 2),
    bfgDepositGuarantee: PLN = PLN(400000),
    // Macroprudential (KNF 2024)
    ccybMax: Multiplier = Multiplier.decimal(25, 3),
    ccybActivationGap: Coefficient = Coefficient.decimal(2, 2),
    ccybReleaseGap: Coefficient = Coefficient.decimal(-2, 2),
    osiiPkoBp: Multiplier = Multiplier.decimal(1, 2),
    osiiPekao: Multiplier = Multiplier.decimal(5, 3),
    concentrationLimit: Share = Share.decimal(25, 2),
    // AFS/HTM bond portfolio split (interest rate risk channel)
    htmShare: Share = Share.decimal(60, 2),
    htmForcedSaleThreshold: Share = Share.decimal(75, 2),
    htmForcedSaleRate: Share = Share.decimal(10, 2),
    initHtmBookYield: Rate = Rate.decimal(55, 3),
    // Deposit mobility (Diamond & Dybvig 1983)
    depositFlightSensitivity: Coefficient = Coefficient(5),
    depositFlightCarThreshold: Multiplier = Multiplier.decimal(10, 2),
    depositPanicRate: Share = Share.decimal(3, 2),
    maxDepositSwitchRate: Share = Share.decimal(10, 2),
    // Interbank contagion
    interbankRecoveryRate: Share = Share.decimal(40, 2),
    hoardingNplThreshold: Share = Share.decimal(5, 2),
    hoardingSensitivity: Multiplier = Multiplier(10),
    // IFRS 9 ECL staging
    eclRate1: Share = Share.decimal(1, 2),
    eclRate2: Share = Share.decimal(8, 2),
    eclRate3: Share = Share.decimal(50, 2),
    eclMigrationSensitivity: Coefficient = Coefficient(3),
    eclGdpSensitivity: Coefficient = Coefficient(5),
    eclMaxMigration: Share = Share.decimal(20, 2),
    eclCureRate: Share = Share.decimal(2, 2),
):
  require(minCar > Multiplier.Zero && minCar < Multiplier.One, s"minCar must be in (0,1): $minCar")
  require(initCapital >= PLN.Zero, s"initCapital must be non-negative: $initCapital")
  require(initDeposits >= PLN.Zero, s"initDeposits must be non-negative: $initDeposits")
  require(lcrMin > Multiplier.Zero, s"lcrMin must be positive: $lcrMin")
  require(nsfrMin > Multiplier.Zero, s"nsfrMin must be positive: $nsfrMin")
