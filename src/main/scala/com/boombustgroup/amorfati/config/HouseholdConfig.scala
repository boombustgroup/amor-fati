package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Household agent parameters: wages, consumption, savings, debt, and consumer
  * credit.
  *
  * Each household is an individual agent with heterogeneous MPC
  * (Beta-distributed), log-normal savings and debt, skill level subject to
  * decay and scarring, and access to consumer credit. Rent is drawn from a
  * truncated normal.
  *
  * @param baseWage
  *   mean monthly gross wage (PLN, GUS 2024: ~8,266 PLN)
  * @param baseReservationWage
  *   minimum acceptable wage — also the 2025 minimum wage level (Dz.U. 2024)
  * @param mpc
  *   mean marginal propensity to consume (aggregate target)
  * @param laborSupplySteepness
  *   slope of labor supply response to wage gap
  * @param wageAdjSpeed
  *   monthly wage Phillips-curve adjustment speed
  * @param count
  *   number of household agents (set to totalPopulation in SimParams.defaults)
  * @param savingsMu
  *   log-normal mean of initial savings distribution (ln PLN)
  * @param savingsSigma
  *   log-normal std dev of initial savings distribution
  * @param debtFraction
  *   fraction of households initialized with positive debt (BIK 2024: ~40%)
  * @param debtMu
  *   log-normal mean of initial debt distribution (ln PLN)
  * @param debtSigma
  *   log-normal std dev of initial debt distribution
  * @param rentMean
  *   mean monthly rent (PLN, Otodom/NBP 2024)
  * @param rentStd
  *   std dev of rent (PLN)
  * @param rentFloor
  *   minimum rent (PLN)
  * @param mpcAlpha
  *   Beta distribution alpha parameter for heterogeneous MPC
  * @param mpcBeta
  *   Beta distribution beta parameter for heterogeneous MPC
  * @param skillDecayRate
  *   monthly skill depreciation rate while unemployed
  * @param scarringRate
  *   additional monthly skill loss after `scarringOnset` months of unemployment
  * @param scarringCap
  *   maximum cumulative scarring penalty
  * @param scarringOnset
  *   months of unemployment before scarring begins
  * @param retrainingCost
  *   cost of retraining program (PLN)
  * @param retrainingDuration
  *   duration of retraining in months
  * @param retrainingBaseSuccess
  *   base probability of successful retraining (education-adjusted)
  * @param retrainingProb
  *   monthly probability of enrolling in retraining while unemployed
  * @param retrainingEnabled
  *   whether retraining mechanism is active
  * @param bankruptcyThreshold
  *   savings threshold (in multiples of monthly wage) below which household
  *   defaults
  * @param socialK
  *   Watts-Strogatz degree for household social network
  * @param socialP
  *   Watts-Strogatz rewiring probability for household network
  * @param debtServiceRate
  *   monthly debt service as fraction of outstanding debt
  * @param baseAmortRate
  *   monthly amortization rate on household debt
  * @param depositSpread
  *   spread below policy rate for household deposit remuneration
  * @param ccSpread
  *   consumer credit spread over policy rate (NBP MIR 2024)
  * @param ccMaxDti
  *   maximum debt-to-income ratio for consumer credit eligibility (KNF
  *   Recommendation T)
  * @param ccMaxLoan
  *   maximum consumer loan size (PLN)
  * @param ccAmortRate
  *   monthly amortization rate on consumer loans
  * @param ccNplRecovery
  *   recovery rate on defaulted consumer loans (BIK 2024)
  * @param ccEligRate
  *   fraction of employed households eligible for consumer credit each month
  */
case class HouseholdConfig(
    baseWage: PLN = PLN(8266),
    baseReservationWage: PLN = PLN(4666),
    mpc: Share = Share.decimal(82, 2),
    laborSupplySteepness: Coefficient = Coefficient(8),
    wageAdjSpeed: Coefficient = Coefficient.decimal(12, 2),
    // Household count (defaults to totalPopulation — set in SimParams.defaults)
    count: Int = 100000,
    // Savings distribution
    savingsMu: Coefficient = Coefficient.decimal(96, 1),
    savingsSigma: Coefficient = Coefficient.decimal(12, 1),
    // Debt
    debtFraction: Share = Share.decimal(40, 2),
    debtMu: Coefficient = Coefficient.decimal(105, 1),
    debtSigma: Coefficient = Coefficient.decimal(15, 1),
    // Rent
    rentMean: PLN = PLN(1800),
    rentStd: PLN = PLN(400),
    rentFloor: PLN = PLN(800),
    // MPC distribution
    mpcAlpha: Coefficient = Coefficient.decimal(82, 1),
    mpcBeta: Coefficient = Coefficient.decimal(18, 1),
    // State-dependent MPC (Carroll 1997 buffer-stock)
    bufferTargetMonths: Multiplier = Multiplier(6),             // target savings = 6 months of income
    bufferSensitivity: Coefficient = Coefficient.decimal(4, 1), // MPC adjustment strength (0 = static, 1 = fully responsive)
    mpcUnemployedBoost: Share = Share.decimal(10, 2),           // MPC uplift when unemployed (desperate spending)
    bufferProtectedShare: Share = Share.decimal(50, 2),         // protected share of target buffer under stress
    bufferExcessDrawdownRate: Share = Share.decimal(20, 2),     // monthly drawdown rate for savings above target buffer
    bufferStressDrawdownRate: Share = Share.decimal(35, 2),     // monthly drawdown rate for savings above protected buffer under stress
    // Skill decay & scarring
    skillDecayRate: Share = Share.decimal(2, 2),
    scarringRate: Share = Share.decimal(2, 2),
    scarringCap: Share = Share.decimal(50, 2),
    scarringOnset: Int = 3,
    // Post-reemployment wage scarring (Jacobson, LaLonde & Sullivan 1993)
    wageScarRate: Share = Share.decimal(25, 3),                 // monthly wage scar accumulation during long-term unemployment
    wageScarCap: Share = Share.decimal(30, 2),                  // max 30% permanent wage loss
    wageScarDecay: Share = Share.decimal(5, 3),                 // monthly recovery once reemployed (~0.5%/mo → ~10 year half-life)
    // Retraining
    retrainingCost: PLN = PLN(5000),
    retrainingDuration: Int = 6,
    retrainingBaseSuccess: Share = Share.decimal(60, 2),
    retrainingProb: Share = Share.decimal(15, 2),
    retrainingEnabled: Boolean = true,
    // Bankruptcy
    bankruptcyThreshold: Coefficient = Coefficient(-3),
    bankruptcyDistressMonths: Int = 3,
    // Social network
    socialK: Int = 10,
    socialP: Share = Share.decimal(15, 2),
    // Debt service
    debtServiceRate: Share = Share.decimal(2, 2),
    baseAmortRate: Share = Share.decimal(15, 3),
    depositSpread: Rate = Rate.decimal(2, 2),
    // Consumer credit
    ccSpread: Rate = Rate.decimal(4, 2),
    ccMaxDti: Share = Share.decimal(40, 2),
    ccMaxLoan: PLN = PLN(50000),
    ccAmortRate: Rate = Rate.decimal(25, 3),
    ccNplRecovery: Share = Share.decimal(15, 2),
    ccEligRate: Share = Share.decimal(30, 2),
)
