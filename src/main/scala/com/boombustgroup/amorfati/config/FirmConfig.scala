package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Firm-level production, automation, entry, and digitalization parameters.
  *
  * Covers the supply side of the agent-based model: revenue generation, cost
  * structure, AI/hybrid automation adoption with CAPEX/OPEX, endogenous firm
  * entry, staged digitalization, and Watts-Strogatz network topology for
  * demonstration effects.
  *
  * @param baseRevenue
  *   monthly revenue per worker before demand shocks (PLN, calibrated to GUS
  *   F-01 2024)
  * @param otherCosts
  *   fixed non-wage monthly cost per worker (materials, rent, utilities)
  * @param aiCapex
  *   one-time capital expenditure for full AI automation per firm (PLN)
  * @param hybridCapex
  *   one-time CAPEX for hybrid (partial) automation per firm (PLN)
  * @param aiOpex
  *   monthly operating cost of full AI automation per firm (PLN)
  * @param hybridOpex
  *   monthly OPEX for hybrid automation per firm (PLN)
  * @param autoSkeletonCrew
  *   minimum workers retained after full AI automation
  * @param hybridReadinessMin
  *   minimum digitalization readiness (DR) for hybrid adoption
  * @param fullAiReadinessMin
  *   minimum DR for full AI adoption
  * @param demandPassthrough
  *   fraction of aggregate demand shock passed to firm revenue
  * @param entryRate
  *   base monthly probability of new firm entry per vacant slot (GUS CEIDG
  *   2024)
  * @param entryProfitSens
  *   sensitivity of entry probability to sector profitability
  * @param entrySectorBarriers
  *   per-sector entry barrier multiplier (6 sectors, GUS CEIDG/KRS 2024)
  * @param entryAiThreshold
  *   sector average DR above which new entrants may be AI-native
  * @param entryAiProb
  *   probability that an entrant in a high-DR sector is AI-native (hybrid)
  * @param entryStartupCash
  *   initial cash endowment for new entrants (PLN)
  * @param digiDrift
  *   monthly exogenous DR drift for all firms
  * @param digiInvestCost
  *   one-time cost of a discretionary DR investment (PLN)
  * @param digiInvestBoost
  *   DR jump from a discretionary investment
  * @param digiCapexDiscount
  *   CAPEX discount for firms above median DR
  * @param digiInvestBaseProb
  *   base monthly probability of discretionary DR investment
  * @param networkK
  *   Watts-Strogatz degree for firm interaction network
  * @param networkRewireP
  *   Watts-Strogatz rewiring probability
  * @param demoEffectThresh
  *   fraction of neighbors automated before demonstration effect triggers
  * @param demoEffectBoost
  *   adoption probability boost from demonstration effect
  * @param sigmaLambda
  *   Poisson arrival rate for aggregate technology shocks (0 = off)
  * @param sigmaCapMult
  *   capacity multiplier applied during a technology shock
  * @param rewireRho
  *   monthly probability of network rewiring (0 = static network)
  */
case class FirmConfig(
    // Production & costs
    baseRevenue: PLN = PLN(180000.0),
    otherCosts: PLN = PLN(16667.0),
    aiCapex: PLN = PLN(1200000.0),
    hybridCapex: PLN = PLN(350000.0),
    aiOpex: PLN = PLN(30000.0),
    hybridOpex: PLN = PLN(12000.0),
    autoSkeletonCrew: Int = 2,
    hybridReadinessMin: Share = Share(0.20),
    fullAiReadinessMin: Share = Share(0.35),
    demandPassthrough: Share = Share(0.40),
    // Entry
    entryRate: Share = Share(0.02),
    entryProfitSens: Coefficient = Coefficient(2.0),
    entrySectorBarriers: Vector[Coefficient] =
      Vector(Coefficient(0.8), Coefficient(0.6), Coefficient(1.2), Coefficient(0.5), Coefficient(0.1), Coefficient(0.7)),
    entryAiThreshold: Share = Share(0.15),
    entryAiProb: Share = Share(0.20),
    entryStartupCash: PLN = PLN(50000.0),
    // Digitalization
    digiDrift: Share = Share(0.001),
    digiInvestCost: PLN = PLN(50000.0),
    digiInvestBoost: Share = Share(0.05),
    digiCapexDiscount: Share = Share(0.30),
    digiInvestBaseProb: Share = Share(0.08),
    // Labor adjustment (smooth hiring/firing)
    laborAdjustSpeed: Coefficient = Coefficient(0.15), // monthly partial adjustment toward target (λ)
    severanceMonths: Double = 2.0,                     // months of wage per fired worker (Kodeks Pracy)
    minWorkersRetained: Int = 3,                       // hard floor on workforce
    // Network / demonstration effects
    networkK: Int = 6,
    networkRewireP: Share = Share(0.10),
    demoEffectThresh: Share = Share(0.40),
    demoEffectBoost: Share = Share(0.15),
    sigmaLambda: Double = 0.0,
    sigmaCapMult: Multiplier = Multiplier(3.0),
    rewireRho: Share = Share(0.0),
):
  require(entrySectorBarriers.length == 6, s"entrySectorBarriers must have 6 sectors: ${entrySectorBarriers.length}")
