package sfc.engine

import sfc.accounting.{ForexState, GovState, MonetaryAggregates, SfcCheck}
import sfc.config.{Config, HH_MODE, HhMode, RunConfig, SECTORS}
import sfc.agents.*
import sfc.agents.{ImmigrationLogic, ImmigrationState}
import sfc.accounting.*
import sfc.networks.Network
import sfc.dynamics.{DynamicNetwork, SigmaDynamics}
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object Sectors:
  private def laborSupplyRatio(wage: Double, resWage: Double): Double =
    val x = Config.LaborSupplySteepness * (wage / resWage - 1.0)
    1.0 / (1.0 + Math.exp(-x))

  def updateLaborMarket(prevWage: Double, resWage: Double, laborDemand: Int): (Double, Int) =
    val supplyAtPrev = (Config.TotalPopulation * laborSupplyRatio(prevWage, resWage)).toInt
    val excessDemand = (laborDemand - supplyAtPrev).toDouble / Config.TotalPopulation
    val wageGrowth   = excessDemand * Config.WageAdjSpeed
    val newWage      = Math.max(resWage, prevWage * (1.0 + wageGrowth))
    val newSupply    = (Config.TotalPopulation * laborSupplyRatio(newWage, resWage)).toInt
    val employed     = Math.min(laborDemand, newSupply)
    (newWage, employed)

  def updateInflation(prevInflation: Double, prevPrice: Double, demandMult: Double,
    wageGrowth: Double, exRateDeviation: Double,
    autoRatio: Double, hybridRatio: Double, rc: RunConfig): (Double, Double) =
    val demandPull    = (demandMult - 1.0) * 0.15
    val costPush      = wageGrowth * 0.25
    // EUR: no exchange rate pass-through (single currency area)
    val rawImportPush = if rc.isEurozone then 0.0
                        else Math.max(0.0, exRateDeviation) * Config.ImportPropensity * 0.25
    val importPush    = if Config.OeEnabled then Math.min(rawImportPush, Config.OeImportPushCap)
                        else rawImportPush
    val techDeflation = autoRatio * 0.060 + hybridRatio * 0.018
    // Soft floor: beyond -1.5%/month, deflation passes through at 30% rate
    // (models downward price stickiness -- Bewley 1999, Schmitt-Grohe & Uribe 2016)
    val rawMonthly    = demandPull + costPush + importPush - techDeflation
    val monthly       = if rawMonthly >= -0.015 then rawMonthly
                        else -0.015 + (rawMonthly + 0.015) * 0.3
    val annualized    = monthly * 12.0
    val smoothed      = prevInflation * 0.7 + annualized * 0.3
    val newPrice      = Math.max(0.30, prevPrice * (1.0 + monthly))
    (smoothed, newPrice)

  def updateCbRate(prevRate: Double, inflation: Double, exRateChange: Double,
    employed: Int, rc: RunConfig): Double =
    if rc.isEurozone then
      // ECB Taylor rule reacting to Eurozone-wide inflation (exogenous to Poland)
      val infGap = Config.EuroInflation - Config.EcbTargetInfl
      val taylor = Config.EcbNeutralRate +
        Config.EcbAlpha * Math.max(0.0, infGap)
      val smoothed = prevRate * Config.EcbInertia + taylor * (1.0 - Config.EcbInertia)
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, smoothed))
    else if Config.NbpSymmetric then
      // v2.0: Symmetric Taylor + output gap (dual mandate)
      val infGap = inflation - Config.NbpTargetInfl
      val unempRate = 1.0 - (employed.toDouble / Config.TotalPopulation)
      val rawOutputGap = (unempRate - Config.NbpNairu) / Config.NbpNairu
      // Cap output gap at ±0.30 — prevents extreme Taylor responses from initial tight labor
      // market while preserving dual-mandate stabilization (Svensson 2003)
      val outputGap = Math.max(-0.30, Math.min(0.30, rawOutputGap))
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * infGap -
        Config.TaylorDelta * outputGap +
        Config.TaylorBeta  * exRateChange
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective = if Config.NbpMaxRateChange > 0 then
        prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
      else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))
    else
      // v1.0 legacy: asymmetric Taylor (inflation-only)
      val infGap  = inflation - Config.NbpTargetInfl
      val taylor  = Config.NbpNeutralRate +
        Config.TaylorAlpha * Math.max(0.0, infGap) +
        Config.TaylorBeta  * Math.max(0.0, exRateChange)
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective = if Config.NbpMaxRateChange > 0 then
        prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
      else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))

  def updateForeign(prev: ForexState, importConsumption: Double, techImports: Double,
    autoRatio: Double, domesticRate: Double, gdp: Double, rc: RunConfig): ForexState =
    val techComp = 1.0 + autoRatio * Config.ExportAutoBoost
    val totalImp = importConsumption + techImports
    if rc.isEurozone then
      // EUR: fixed exchange rate, exports depend on relative price competitiveness
      // No capital account arbitrage (single monetary zone)
      val exports  = Config.ExportBase * techComp
      val tradeBal = exports - totalImp
      ForexState(Config.BaseExRate, totalImp, exports, tradeBal, techImports)
    else
      // PLN: floating exchange rate, BoP-driven adjustment
      val exComp   = prev.exchangeRate / Config.BaseExRate
      val exports  = Config.ExportBase * exComp * techComp
      val tradeBal = exports - totalImp
      val rateDiff = domesticRate - Config.ForeignRate
      val capAcct  = rateDiff * Config.IrpSensitivity * gdp
      val bop      = tradeBal + capAcct
      val bopRatio = if gdp > 0 then bop / gdp else 0.0
      val exRateChg = -Config.ExRateAdjSpeed * bopRatio
      val newRate  = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
      ForexState(newRate, totalImp, exports, tradeBal, techImports)

  def updateGov(prev: GovState, citPaid: Double, vat: Double,
    bdpActive: Boolean, bdpAmount: Double, priceLevel: Double,
    unempBenefitSpend: Double,
    debtService: Double = 0.0,
    nbpRemittance: Double = 0.0,
    zusGovSubvention: Double = 0.0,
    socialTransferSpend: Double = 0.0,
    euCofinancing: Double = 0.0,
    euProjectCapital: Double = 0.0,
    exciseRevenue: Double = 0.0,
    customsDutyRevenue: Double = 0.0,
    govPurchasesActual: Double = 0.0): GovState =
    val bdpSpend   = if bdpActive then Config.TotalPopulation.toDouble * bdpAmount else 0.0
    val govBaseRaw = if govPurchasesActual > 0 then govPurchasesActual
                     else Config.GovBaseSpending * priceLevel
    val (govCurrent, govCapital) = if Config.GovInvestEnabled then
      (govBaseRaw * (1.0 - Config.GovInvestShare), govBaseRaw * Config.GovInvestShare)
    else (govBaseRaw, 0.0)
    val totalSpend = bdpSpend + unempBenefitSpend + socialTransferSpend + govCurrent + govCapital + debtService + zusGovSubvention + euCofinancing
    val totalRev   = citPaid + vat + nbpRemittance + exciseRevenue + customsDutyRevenue
    val deficit    = totalSpend - totalRev
    val newBondsOutstanding = if Config.GovBondMarket then Math.max(0.0, prev.bondsOutstanding + deficit)
                              else prev.bondsOutstanding
    val newCapitalStock = if Config.GovInvestEnabled then
      prev.publicCapitalStock * (1.0 - Config.GovDepreciationRate / 12.0) + govCapital + euProjectCapital
    else 0.0
    GovState(bdpActive, totalRev, bdpSpend, deficit, prev.cumulativeDebt + deficit,
      unempBenefitSpend, newBondsOutstanding, prev.bondYield, debtService, socialTransferSpend,
      newCapitalStock, govCurrent, govCapital + euProjectCapital, euCofinancing,
      exciseRevenue, customsDutyRevenue)

object Simulation:
  /** Step with optional individual households.
    * When households = None (aggregate mode), behavior is identical to Papers 1–5.
    * When households = Some(...) (individual mode), uses HouseholdLogic + LaborMarket. */
  def step(w: World, firms: Array[Firm], rc: RunConfig,
           households: Option[Vector[Household]] = None): (World, Array[Firm], Option[Vector[Household]]) =
    val m = w.month + 1
    val bdpActive = m >= Config.ShockMonth

    val bdpUnconstrained = if bdpActive then rc.bdpAmount else 0.0
    // EUR regime: SGP fiscal constraint caps BDP spending
    val bdp = if rc.isEurozone && bdpActive && bdpUnconstrained > 0 then
      val annualGdp = w.gdpProxy * 12.0
      // Flow constraint: monthly deficit ≤ 3%/12 of annual GDP
      val maxMonthlyDeficit = Config.SgpDeficitLimit * annualGdp / 12.0
      val baseGovSpend = Config.GovBaseSpending * w.priceLevel
      val estRevenue = w.gov.taxRevenue  // previous period's revenue
      val maxBdpSpend = Math.max(0.0, maxMonthlyDeficit + estRevenue - baseGovSpend)
      val maxBdpPerCapita = maxBdpSpend / Config.TotalPopulation.toDouble
      // Stock constraint: debt brake — if debt > 60% of GDP, progressive austerity
      val debtRatio = if annualGdp > 0 then w.gov.cumulativeDebt / annualGdp else 0.0
      val austerityMult = if debtRatio > Config.SgpDebtLimit then
        Math.max(0.0, 1.0 - (debtRatio - Config.SgpDebtLimit) * Config.SgpAusterityRate)
      else 1.0
      Math.max(0.0, Math.min(bdpUnconstrained, maxBdpPerCapita * austerityMult))
    else bdpUnconstrained
    val (baseMinWage, updatedMinWagePriceLevel) = if Config.MinWageEnabled then
      val isAdjustMonth = m > 0 && m % Config.MinWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl = if Config.MinWageInflationIndex && w.hh.minWagePriceLevel > 0 then
          w.priceLevel / w.hh.minWagePriceLevel - 1.0 else 0.0
        val inflIndexed = w.hh.minWageLevel * (1.0 + Math.max(0.0, cumInfl))
        val target = w.hh.marketWage * Config.MinWageTargetRatio
        val gap = target - inflIndexed
        val adjusted = if gap > 0 then inflIndexed + gap * Config.MinWageConvergenceSpeed
                       else inflIndexed
        (Math.max(w.hh.minWageLevel, adjusted), w.priceLevel)
      else (w.hh.minWageLevel, w.hh.minWagePriceLevel)
    else (Config.BaseReservationWage, w.hh.minWagePriceLevel)

    val resWage = baseMinWage + bdp * Config.ReservationBdpMult

    // When term structure is enabled, use WIBOR 3M as lending base rate
    // instead of refRate. Uses LAGGED interbankRate (standard SFC approach).
    val rawLendingBaseRate = if Config.InterbankTermStructure then
      w.bankingSector.map { bs =>
        YieldCurve.compute(bs.interbankRate).wibor3m
      }.getOrElse(w.nbp.referenceRate)
    else w.nbp.referenceRate

    // Channel 2: Firm CAPEX via expected rate — firms partially factor expected future rate
    val lendingBaseRate = if Config.ExpEnabled then
      0.5 * rawLendingBaseRate + 0.5 * w.expectations.expectedRate
    else rawLendingBaseRate

    val living = firms.filter(FirmOps.isAlive)
    val laborDemand = living.kahanSumBy(f => FirmOps.workers(f).toDouble).toInt
    val (rawWage, rawEmployed) = Sectors.updateLaborMarket(w.hh.marketWage, resWage, laborDemand)

    // Channel 1: Expectations-augmented wage Phillips curve
    // Asymmetric: only upward pressure (Bewley 1999 — workers don't demand wage cuts)
    val wageAfterExp = if Config.ExpEnabled then
      val target = if rc.isEurozone then Config.EcbTargetInfl else Config.NbpTargetInfl
      val expWagePressure = Config.ExpWagePassthrough *
        Math.max(0.0, w.expectations.expectedInflation - target) / 12.0
      Math.max(resWage, rawWage * (1.0 + expWagePressure))
    else rawWage

    // Union downward wage rigidity (#44): dampen wage declines in proportion to aggregate union density
    val newWage = if Config.UnionEnabled && wageAfterExp < w.hh.marketWage then
      val aggDensity = SECTORS.zipWithIndex.map((s, i) => s.share * Config.UnionDensity(i)).sum
      val decline = w.hh.marketWage - wageAfterExp
      Math.max(resWage, wageAfterExp + decline * Config.UnionRigidity * aggDensity)
    else wageAfterExp

    // Demographics caps employment at working-age population
    val employed = if Config.DemEnabled then
      Math.min(rawEmployed, w.demographics.workingAgePop)
    else rawEmployed

    // Immigration step: compute inflow/outflow before demographics (net migration feeds into workingAgePop)
    val unempRateForImmig = 1.0 - employed.toDouble / Config.TotalPopulation
    val newImmig = ImmigrationLogic.step(
      w.immigration, households, newWage, unempRateForImmig,
      w.demographics.workingAgePop.max(Config.TotalPopulation), m)
    val netMigration = newImmig.monthlyInflow - newImmig.monthlyOutflow

    val newDemographics = PublicSectorLogic.demographicsStep(w.demographics, employed, netMigration)

    // ZUS (contributions + pensions) and PPK
    val newZus = PublicSectorLogic.zusStep(w.zus.fusBalance, employed, newWage, newDemographics.retirees)
    val newPpk = PublicSectorLogic.ppkStep(w.ppk.bondHoldings, employed, newWage)
    val rawPpkBondPurchase = PublicSectorLogic.ppkBondPurchase(newPpk)

    val wageGrowth = if w.hh.marketWage > 0 then newWage / w.hh.marketWage - 1.0 else 0.0

    // Import adjustment (used by both paths)
    val importAdj = Config.ImportPropensity *
      Math.pow(Config.BaseExRate / w.forex.exchangeRate, 0.5)

    // Aggregate-mode unemployment benefits (automatic stabilizer, GUS BAEL 2023)
    val aggUnempBenefit = if Config.GovUnempBenefitEnabled && households.isEmpty then
      val unempCount = Math.max(0, Config.TotalPopulation - employed)
      val avgBenefit = (Config.GovBenefitM1to3 * 3.0 + Config.GovBenefitM4to6 * 3.0) /
        Config.GovBenefitDuration.toDouble
      unempCount.toDouble * avgBenefit * Config.GovBenefitCoverage
    else 0.0

    // ---- Aggregate vs Individual household path ----
    val (totalIncome, consumption, importCons, domesticCons, updatedHouseholds, hhAgg, perBankHhFlowsOpt) =
      households match
        case None =>
          // Aggregate mode (+ ZUS deductions/pensions + PIT + 800+)
          // Include union wage premium: firms pay effectiveWageMult but workers must receive it
          val avgWageMult = if Config.UnionEnabled then
            SECTORS.indices.map(i =>
              SECTORS(i).share * (1.0 + Config.UnionWagePremium * Config.UnionDensity(i))).sum
          else 1.0
          val wageIncome = employed.toDouble * newWage * avgWageMult
          val bdpIncome  = if bdpActive then Config.TotalPopulation.toDouble * bdp else 0.0
          val grossIncome = wageIncome + bdpIncome
          val pitDeduction = if Config.PitEnabled then grossIncome * Config.PitEffectiveRate else 0.0
          val socialTransfers = if Config.Social800Enabled then
            Config.TotalPopulation.toDouble * Config.Social800ChildrenPerHh * Config.Social800Rate
          else 0.0
          val ti = grossIncome - newZus.contributions + newZus.pensionPayments - pitDeduction + socialTransfers + aggUnempBenefit
          val cons = ti * Config.Mpc
          val ic = cons * Math.min(0.65, importAdj)
          val dc = cons - ic
          (ti, cons, ic, dc, None, None, Option.empty[PerBankHhFlows])

        case Some(hhs) =>
          // Individual mode: process household agents
          // Separations (workers from automated/bankrupt firms)
          val afterSep = LaborMarket.separations(hhs, firms, firms)  // firms not yet updated
          // Wages update (will be refined after firm processing below)
          val afterWages = LaborMarket.updateWages(afterSep, newWage)
          // Compute bank rates for variable-rate HH loans + deposit interest
          val nBanksHh = w.bankingSector.map(_.banks.length).getOrElse(1)
          val hhBankRates = Some(BankRates(
            lendingRates = w.bankingSector match
              case Some(bs) => bs.banks.zip(bs.configs).map((b, cfg) =>
                BankingSector.lendingRate(b, cfg, lendingBaseRate)).toArray
              case None => Array(w.bank.lendingRate(lendingBaseRate)),
            depositRates = w.bankingSector match
              case Some(bs) => bs.banks.map(_ =>
                BankingSector.hhDepositRate(w.nbp.referenceRate)).toArray
              case None => Array(BankingSector.hhDepositRate(w.nbp.referenceRate))
          ))
          // Equity index return for HH wealth revaluation (lagged: uses previous month's return)
          val eqReturn = w.equity.monthlyReturn
          // Sectoral mobility signals (computed from lagged state)
          val secWages = if Config.LmSectoralMobility then Some(SectoralMobility.sectorWages(afterWages)) else None
          val secVacancies = if Config.LmSectoralMobility then Some(SectoralMobility.sectorVacancies(afterWages, firms)) else None
          // Household monthly step
          val (newHhs, agg, pbf) = HouseholdLogic.step(
            afterWages, w, bdp, newWage, resWage, importAdj, Random, nBanksHh, hhBankRates, eqReturn,
            secWages, secVacancies)
          (agg.totalIncome, agg.consumption, agg.importConsumption,
           agg.domesticConsumption, Some(newHhs), Some(agg), pbf)

    // PIT revenue: individual mode uses per-HH accumulation, aggregate mode uses flat effective rate
    val pitRevenue = if Config.PitEnabled then
      hhAgg.map(_.totalPit).getOrElse {
        // Aggregate mode: pitDeduction was computed inside the match arm
        val wageIncome = employed.toDouble * newWage
        val bdpIncome  = if bdpActive then Config.TotalPopulation.toDouble * bdp else 0.0
        (wageIncome + bdpIncome) * Config.PitEffectiveRate
      }
    else 0.0

    // Flow-of-funds: sector-level demand multipliers
    // Government purchases = autonomous base + fiscal recycling of lagged tax revenue.
    // Spending inertia: governments smooth expenditure via borrowing, max 2%/month decline
    // (Blanchard & Perotti 2002: government spending has high persistence ρ ≈ 0.95-0.98).
    val zusNetSurplus = if Config.ZusEnabled then
      Math.max(0.0, w.zus.contributions - w.zus.pensionPayments) else 0.0
    // Base spending anchored to max(1, priceLevel): nominal spending doesn't fall in deflation
    // (Polish budget law: government spending is set in nominal terms, not indexed to CPI)
    // Counter-cyclical fiscal stimulus: automatic stabilizer when unemployment > NAIRU
    // (Blanchard 2019: fiscal multiplier 1.5-2.0 in liquidity trap)
    val unempRateForFiscal = 1.0 - employed.toDouble / Config.TotalPopulation
    val unempGap = Math.max(0.0, unempRateForFiscal - Config.NbpNairu)
    val fiscalStimulus = Config.GovBaseSpending * unempGap * Config.GovAutoStabMult
    val targetGovPurchases = Config.GovBaseSpending * Math.max(1.0, w.priceLevel) +
      Config.GovFiscalRecyclingRate * (w.gov.taxRevenue + zusNetSurplus) + fiscalStimulus
    val prevGovSpend = w.gov.govCurrentSpend + w.gov.govCapitalSpend
    val govPurchases = if prevGovSpend > 0 then
      Math.max(targetGovPurchases, prevGovSpend * 0.98)
    else targetGovPurchases
    val laggedExports = w.forex.exports
    val sectorCap = (0 until SECTORS.length).map { s =>
      living.filter(_.sector.toInt == s).kahanSumBy(f => FirmOps.capacity(f).toDouble)
    }.toVector
    val sectorExports = if Config.GvcEnabled && Config.OeEnabled then
      w.gvc.sectorExports
    else
      Config.FofExportShares.map(_ * laggedExports)
    // Investment demand: lagged gross investment creates demand for capital goods sectors.
    // GDP = C + I + G + NX — without this term, demand is structurally ~20% below capacity.
    // Domestic portion only (imported capital goods don't create domestic demand).
    val laggedInvestDemand = w.grossInvestment * (1.0 - Config.PhysCapImportShare) +
      w.aggGreenInvestment * (1.0 - Config.GreenImportShare)
    val sectorDemand = (0 until SECTORS.length).map { s =>
      Config.FofConsWeights(s) * domesticCons +
      Config.FofGovWeights(s) * govPurchases +
      Config.FofInvestWeights(s) * laggedInvestDemand +
      sectorExports(s)
    }.toVector
    val fofTotalDemand = sectorDemand.kahanSum
    val totalCapacity = sectorCap.kahanSum
    // Demand spillover: when a sector hits capacity (demandMult > 1), excess demand
    // shifts to sectors with slack (substitution effect). This prevents unrealistic
    // super-capacity revenue and balances the sector demand distribution.
    val rawSectorMults = sectorDemand.indices.map { s =>
      if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * w.priceLevel) else 0.0
    }.toVector
    val excessDemand = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then (rawSectorMults(s) - 1.0) * sectorCap(s) * w.priceLevel else 0.0
    }.kahanSum
    val deficitCapacity = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) < 1.0 then (1.0 - rawSectorMults(s)) * sectorCap(s) * w.priceLevel else 0.0
    }.kahanSum
    val spilloverFrac = if deficitCapacity > 0 then Math.min(1.0, excessDemand / deficitCapacity) else 0.0
    val sectorMults = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then 1.0
      else rawSectorMults(s) + spilloverFrac * (1.0 - rawSectorMults(s))
    }.toVector
    // Channel 4: Real rate effect — negative real rate boosts demand, positive dampens
    val realRateEffect = if Config.ExpEnabled then
      val realRate = w.nbp.referenceRate - w.expectations.expectedInflation
      -realRate * 0.02
    else 0.0
    val avgDemandMult = (if totalCapacity > 0 then fofTotalDemand / (totalCapacity * w.priceLevel) else 1.0) + realRateEffect

    // ---- Lending dispatch: single-bank vs multi-bank ----
    val nBanks = w.bankingSector.map(_.banks.length).getOrElse(1)
    val perBankNewLoans = new Array[Double](nBanks)
    val perBankNplDebt  = new Array[Double](nBanks)
    val perBankIntIncome = new Array[Double](nBanks)
    val perBankWorkers  = new Array[Int](nBanks)

    // Pass macropru CCyB to canLend for effective MinCAR
    val currentCcyb = w.macropru.ccyb
    val (getLendRate, bankCanLendFn): (Int => Double, (Int, Double) => Boolean) =
      w.bankingSector match
        case Some(bs) =>
          val rates = bs.banks.zip(bs.configs).map((b, cfg) =>
            BankingSector.lendingRate(b, cfg, lendingBaseRate))
          ((bankId: Int) => rates(bankId),
           (bankId: Int, amt: Double) => BankingSector.canLend(bs.banks(bankId), amt, Random, currentCcyb))
        case None =>
          val rate = w.bank.lendingRate(lendingBaseRate)
          val canLendFn: (Int, Double) => Boolean = (_: Int, amt: Double) => {
            val approvalP = Math.max(0.1, 1.0 - w.bank.nplRatio * 3.0)
            w.bank.canLend(amt) && Random.nextDouble() < approvalP
          }
          ((_: Int) => rate, canLendFn)

    var sumTax      = 0.0
    var sumCapex    = 0.0
    var sumTechImp  = 0.0
    var sumNewLoans = 0.0
    var sumEquityIssuance = 0.0
    var sumGrossInvestment = 0.0
    var sumBondIssuance = 0.0
    var sumProfitShifting = 0.0
    var sumFdiRepatriation = 0.0
    var sumInventoryChange = 0.0
    var sumCitEvasion = 0.0
    var sumEnergyCost = 0.0
    var sumGreenInvestment = 0.0

    val macro4firms = w.copy(
      month = m, sectorDemandMult = sectorMults,
      hh = w.hh.copy(marketWage = newWage, reservationWage = resWage))

    // Per-firm bond issuance amounts (for demand-side constraint revert)
    val firmBondAmounts = scala.collection.mutable.HashMap.empty[FirmId, Double]

    // Process firms -- per-firm dispatch to bank, accumulate per-bank flows
    val newFirms = firms.map { f =>
      val firmRate = getLendRate(f.bankId.toInt)
      val firmCanLend: Double => Boolean = amt => bankCanLendFn(f.bankId.toInt, amt)
      val r = FirmLogic.process(f, macro4firms, firmRate, firmCanLend, firms, rc)
      sumTax      += r.taxPaid
      sumCapex    += r.capexSpent
      sumTechImp  += r.techImports
      sumGrossInvestment += r.grossInvestment
      sumProfitShifting += r.profitShiftCost
      sumFdiRepatriation += r.fdiRepatriation
      sumInventoryChange += r.inventoryChange
      sumCitEvasion += r.citEvasion
      sumEnergyCost += r.energyCost
      sumGreenInvestment += r.greenInvestment

      // GPW equity issuance: eligible firms replace fraction of loan with equity
      val (actualLoan, equityAmt, updatedFirm) =
        if Config.GpwEnabled && Config.GpwEquityIssuance && r.newLoan > 0 &&
           FirmOps.workers(r.firm) >= Config.GpwIssuanceMinSize then
          val eqAmt = r.newLoan * Config.GpwIssuanceFrac
          val adjLoan = r.newLoan - eqAmt
          // Firm: reduce debt by equity portion, track equityRaised
          val f2 = r.firm.copy(
            debt = r.firm.debt - eqAmt,
            equityRaised = r.firm.equityRaised + eqAmt
          )
          (adjLoan, eqAmt, f2)
        else
          (r.newLoan, 0.0, r.firm)

      // Corporate bond issuance: eligible firms replace fraction of loan with bonds (#40)
      val (finalLoan, bondAmt, bondUpdatedFirm) =
        if actualLoan > 0 && FirmOps.workers(updatedFirm) >= Config.CorpBondMinSize then
          val ba = actualLoan * Config.CorpBondIssuanceFrac
          val adjLoan = actualLoan - ba
          val f3 = updatedFirm.copy(
            debt = updatedFirm.debt - ba,
            bondDebt = updatedFirm.bondDebt + ba
          )
          (adjLoan, ba, f3)
        else
          (actualLoan, 0.0, updatedFirm)

      sumNewLoans += finalLoan
      sumEquityIssuance += equityAmt
      sumBondIssuance += bondAmt
      if bondAmt > 0 then firmBondAmounts(f.id) = bondAmt
      perBankNewLoans(f.bankId.toInt) += finalLoan
      bondUpdatedFirm
    }

    // Corporate bond demand-side constraint: compute absorption and revert unsold to bank loans
    val corpBondAbsorption = CorporateBondMarket.computeAbsorption(
      w.corporateBonds, sumBondIssuance, w.bank.car, Config.MinCar)
    val actualBondIssuance = sumBondIssuance * corpBondAbsorption
    val revertRatio = 1.0 - corpBondAbsorption
    val adjustedFirms = if revertRatio > 0.001 then
      newFirms.map { f =>
        val ba = firmBondAmounts.getOrElse(f.id, 0.0)
        if ba > 0 then
          val revert = ba * revertRatio
          f.copy(bondDebt = f.bondDebt - revert, debt = f.debt + revert)
        else f
      }
    else newFirms
    // Revert unsold bonds to bank loans
    if revertRatio > 0.001 then
      val unsoldBonds = sumBondIssuance * revertRatio
      sumNewLoans += unsoldBonds
      for (fid, ba) <- firmBondAmounts do
        val revert = ba * revertRatio
        adjustedFirms.find(_.id == fid).foreach(ff => perBankNewLoans(ff.bankId.toInt) += revert)

    // Track per-bank workers for deposit partitioning
    for f <- adjustedFirms if FirmOps.isAlive(f) do
      perBankWorkers(f.bankId.toInt) += FirmOps.workers(f)

    // I-O intermediate market (Paper-07)
    val (ioFirms, totalIoPaid) = if Config.IoEnabled then
      val r = IntermediateMarket.process(adjustedFirms, sectorMults, w.priceLevel,
        Config.IoMatrix, Config.IoColumnSums, Config.IoScale)
      (r.firms, r.totalPaid)
    else
      (adjustedFirms, 0.0)

    // Individual mode: post-firm-processing labor market update
    var postFirmCrossSectorHires = 0
    val preMigrationHouseholds = updatedHouseholds.map { hhs =>
      val afterSep = LaborMarket.separations(hhs, firms, ioFirms)
      val (afterSearch, csHires) = LaborMarket.jobSearch(afterSep, ioFirms, newWage, Random)
      postFirmCrossSectorHires += csHires
      LaborMarket.updateWages(afterSearch, newWage)  // normalize wages for next step
    }

    // Immigration: spawn new immigrants, remove returning migrants (individual mode)
    val finalHouseholds = if Config.ImmigEnabled then
      preMigrationHouseholds.map { hhs =>
        val afterRemoval = ImmigrationLogic.removeReturnMigrants(hhs, newImmig.monthlyOutflow)
        val startId = afterRemoval.map(_.id).maxOption.getOrElse(-1) + 1
        val newImmigrants = ImmigrationLogic.spawnImmigrants(newImmig.monthlyInflow, startId, Random)
        afterRemoval ++ newImmigrants
      }
    else preMigrationHouseholds

    // Aggregate mode: update TotalPopulation with net migration
    if Config.ImmigEnabled && households.isEmpty then
      Config.setTotalPopulation(Config.TotalPopulation + netMigration)

    val prevAlive = firms.filter(FirmOps.isAlive).map(_.id).toSet
    val newlyDead = ioFirms.filter(f => !FirmOps.isAlive(f) && prevAlive.contains(f.id))
    val firmDeaths = newlyDead.length
    val nplNew    = newlyDead.kahanSumBy(_.debt)
    val nplLoss   = nplNew * (1.0 - Config.LoanRecovery)
    val totalBondDefault = newlyDead.kahanSumBy(_.bondDebt)

    // Per-bank NPL attribution
    for f <- newlyDead do perBankNplDebt(f.bankId.toInt) += f.debt

    // Per-bank interest income
    for f <- firms if FirmOps.isAlive(f) do
      perBankIntIncome(f.bankId.toInt) += f.debt * getLendRate(f.bankId.toInt) / 12.0

    val intIncome = perBankIntIncome.kahanSum

    // SFC: household debt service flows to bank as interest income
    val hhDebtService = hhAgg.map(_.totalDebtService).getOrElse(0.0)
    // SFC: deposit interest paid to HH (monetary transmission channel 2)
    val depositInterestPaid = hhAgg.map(_.totalDepositInterest).getOrElse(0.0)
    // SFC: remittance outflow (immigrant wages sent abroad — deposit outflow + current account)
    val remittanceOutflow = hhAgg.map(_.totalRemittances).getOrElse(newImmig.remittanceOutflow)

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if Config.RemittanceEnabled then
      val wap = if Config.DemEnabled then w.demographics.workingAgePop else Config.TotalPopulation
      val base = Config.RemittancePerCapita * wap.toDouble
      val erAdj = Math.pow(w.forex.exchangeRate / Config.BaseExRate, Config.RemittanceErElasticity)
      val trendAdj = Math.pow(1.0 + Config.RemittanceGrowthRate / 12.0, m.toDouble)
      val unempForRemit = 1.0 - employed.toDouble / Config.TotalPopulation
      val cyclicalAdj = 1.0 + Config.RemittanceCyclicalSens * Math.max(0.0, unempForRemit - 0.05)
      base * erAdj * trendAdj * cyclicalAdj
    else 0.0

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if Config.TourismEnabled then
      val monthInYear = (m % 12) + 1
      val seasonalFactor = 1.0 + Config.TourismSeasonality *
        Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
      val inboundErAdj = Math.pow(w.forex.exchangeRate / Config.BaseExRate, Config.TourismErElasticity)
      val outboundErAdj = Math.pow(Config.BaseExRate / w.forex.exchangeRate, Config.TourismErElasticity)
      val trendAdj = Math.pow(1.0 + Config.TourismGrowthRate / 12.0, m.toDouble)
      val disruption = if Config.TourismShockMonth > 0 && m >= Config.TourismShockMonth then
        Config.TourismShockSize * Math.pow(1.0 - Config.TourismShockRecovery,
          (m - Config.TourismShockMonth).toDouble)
      else 0.0
      val shockFactor = 1.0 - disruption
      val baseGdp = Math.max(0.0, w.gdpProxy)
      val inbound = Math.max(0.0, baseGdp * Config.TourismInboundShare *
        seasonalFactor * inboundErAdj * trendAdj * shockFactor)
      val outbound = Math.max(0.0, baseGdp * Config.TourismOutboundShare *
        seasonalFactor * outboundErAdj * trendAdj * shockFactor)
      (inbound, outbound)
    else (0.0, 0.0)

    // Consumer credit flows
    val aggConsumerDS = w.bank.consumerLoans * (Config.CcAmortRate + (lendingBaseRate + Config.CcSpread) / 12.0)
    val aggConsumerOrig = domesticCons * 0.02  // ~2% of consumption financed by credit
    val consumerDebtService = hhAgg.map(_.totalConsumerDebtService).getOrElse(aggConsumerDS)
    val consumerOrigination = hhAgg.map(_.totalConsumerOrigination).getOrElse(aggConsumerOrig)
    val consumerDefaultAmt = hhAgg.map(_.totalConsumerDefault).getOrElse(0.0)
    val consumerNplLoss = consumerDefaultAmt * (1.0 - Config.CcNplRecovery)
    // Per-HH principal tracking (rate-independent): D * CcAmortRate per HH
    // Falls back to rate-ratio derivation in aggregate mode
    val consumerPrincipal = hhAgg.map(_.totalConsumerPrincipal).getOrElse {
      if Config.CcAmortRate + (lendingBaseRate + Config.CcSpread) / 12.0 > 0 then
        consumerDebtService * (Config.CcAmortRate / (Config.CcAmortRate + (lendingBaseRate + Config.CcSpread) / 12.0))
      else 0.0
    }
    // Note: newBank construction deferred until after bond market block (needs bankBondIncome)

    val living2 = ioFirms.filter(FirmOps.isAlive)
    val nLiving = living2.length.toDouble
    val autoR   = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]) / nLiving else 0.0
    val hybR    = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]) / nLiving else 0.0
    val aggInventoryStock = if Config.InventoryEnabled then
      living2.kahanSumBy(_.inventory) else 0.0
    val aggGreenCapital = if Config.EnergyEnabled then
      living2.kahanSumBy(_.greenCapital) else 0.0

    // EU Funds: time-varying absorption curve or flat legacy transfer
    val euMonthly = if Config.EuFundsEnabled then EuFunds.monthlyTransfer(m)
                    else Config.OeEuTransfers

    val govGdpContribution = if Config.GovInvestEnabled then
      Config.GovBaseSpending * (1.0 - Config.GovInvestShare) * Config.GovCurrentMultiplier +
      Config.GovBaseSpending * Config.GovInvestShare * Config.GovCapitalMultiplier
    else Config.GovBaseSpending
    // EU Funds: co-financing and capital investment for GDP proxy
    val euCofin = if Config.EuFundsEnabled then EuFunds.cofinancing(euMonthly) else 0.0
    val euProjectCapital = if Config.EuFundsEnabled && Config.GovInvestEnabled then
      EuFunds.capitalInvestment(euMonthly, euCofin)
    else 0.0
    val euGdpContribution = if Config.EuFundsEnabled && Config.GovInvestEnabled then
      euProjectCapital * Config.GovCapitalMultiplier +
      (euCofin - euProjectCapital).max(0.0) * Config.GovCurrentMultiplier
    else if Config.EuFundsEnabled then euCofin
    else 0.0
    val greenDomesticGFCF = if Config.EnergyEnabled then
      sumGreenInvestment * (1.0 - Config.GreenImportShare) else 0.0
    val domesticGFCF = (if Config.PhysCapEnabled then
      sumGrossInvestment * (1.0 - Config.PhysCapImportShare) else 0.0) + greenDomesticGFCF
    val investmentImports = (if Config.PhysCapEnabled then
      sumGrossInvestment * Config.PhysCapImportShare else 0.0) +
      (if Config.EnergyEnabled then sumGreenInvestment * Config.GreenImportShare else 0.0)
    val aggInventoryChange = if Config.InventoryEnabled then sumInventoryChange else 0.0
    val gdp     = domesticCons + govGdpContribution + euGdpContribution + w.forex.exports + domesticGFCF + aggInventoryChange

    // Macroprudential — compute CCyB from credit-to-GDP gap
    val totalSystemLoans = w.bankingSector.map(_.banks.kahanSumBy(_.loans)).getOrElse(w.bank.totalLoans)
    val newMacropru = Macroprudential.step(w.macropru, totalSystemLoans, gdp)

    // Endogenous sigma evolution (Paper-05)
    val sectorAdoption = SECTORS.indices.map { s =>
      val secFirms = living2.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else secFirms.count(f =>
        f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid]
      ).toDouble / secFirms.length
    }.toVector
    val baseSigmas = SECTORS.map(_.sigma).toVector
    val newSigmas = SigmaDynamics.evolve(
      w.currentSigmas, baseSigmas, sectorAdoption, Config.SigmaLambda, Config.SigmaCapMult)

    // Dynamic network rewiring (Paper-05)
    val rewiredFirms = DynamicNetwork.rewire(ioFirms, Config.RewireRho)

    val exDev = if rc.isEurozone then 0.0
               else (w.forex.exchangeRate / Config.BaseExRate) - 1.0
    val (newInfl, newPrice) = Sectors.updateInflation(
      w.inflation, w.priceLevel, avgDemandMult, wageGrowth, exDev, autoR, hybR, rc)

    // Firm profits for equity market (sum of after-tax profits of living firms)
    val firmProfits = living2.kahanSumBy { f =>
      val rev = FirmOps.capacity(f) * sectorMults(f.sector.toInt) * newPrice
      val labor = FirmOps.workers(f) * newWage * SECTORS(f.sector.toInt).wageMultiplier
      val other = Config.OtherCosts * newPrice
      val aiMaint = f.tech match
        case _: TechState.Automated => Config.AiOpex * (0.60 + 0.40 * newPrice)
        case _: TechState.Hybrid    => Config.HybridOpex * (0.60 + 0.40 * newPrice)
        case _                      => 0.0
      val interest = f.debt * getLendRate(f.bankId.toInt) / 12.0
      val gross = rev - labor - other - aiMaint - interest
      val tax = Math.max(0.0, gross) * Config.CitRate
      Math.max(0.0, gross - tax)
    }

    // GPW equity market step
    val prevGdp = if w.gdpProxy > 0 then w.gdpProxy else 1.0
    val gdpGrowthForEquity = (gdp - prevGdp) / prevGdp
    val equityAfterIndex = EquityMarket.step(w.equity, w.nbp.referenceRate, newInfl,
      gdpGrowthForEquity, firmProfits)
    val equityAfterIssuance = EquityMarket.processIssuance(sumEquityIssuance, equityAfterIndex)
    // HH equity wealth updated later (after reassignedHouseholds is available)

    // GPW dividends: firm profits → HH income + foreign outflow + tax
    val (netDomesticDividends, foreignDividendOutflow, dividendTax) =
      if Config.GpwEnabled && Config.GpwDividends then
        EquityMarket.computeDividends(firmProfits, equityAfterIssuance.dividendYield,
          equityAfterIssuance.marketCap, equityAfterIssuance.foreignOwnership)
      else (0.0, 0.0, 0.0)

    // Open economy (Paper-08) or legacy foreign sector
    val sectorOutputs = (0 until SECTORS.length).map { s =>
      living2.filter(_.sector.toInt == s).kahanSumBy(f => FirmOps.capacity(f) * sectorMults(f.sector.toInt) * w.priceLevel)
    }.toVector

    // GVC / Deep External Sector (v5.0)
    val newGvc = if Config.GvcEnabled && Config.OeEnabled then
      ExternalSector.step(w.gvc, sectorOutputs, w.priceLevel,
        w.forex.exchangeRate, autoR, m, rc)
    else w.gvc

    val (gvcExp, gvcImp) = if Config.GvcEnabled && Config.OeEnabled then
      (Some(newGvc.totalExports), Some(newGvc.sectorImports))
    else (None, None)

    val totalTechAndInvImports = sumTechImp + investmentImports
    val (newForex, newBop0, oeValuationEffect, fxResult) = if Config.OeEnabled then
      val oeResult = OpenEconomy.step(
        w.bop, w.forex, importCons, totalTechAndInvImports,
        autoR, w.nbp.referenceRate, gdp, w.priceLevel,
        sectorOutputs, m, rc,
        nbpFxReserves = w.nbp.fxReserves,
        gvcExports = gvcExp,
        gvcIntermImports = gvcImp,
        remittanceOutflow = remittanceOutflow,
        euFundsMonthly = euMonthly,
        diasporaInflow = diasporaInflow,
        tourismExport = tourismExport,
        tourismImport = tourismImport)
      (oeResult.forex, oeResult.bop, oeResult.valuationEffect, oeResult.fxIntervention)
    else
      val fx = Sectors.updateForeign(w.forex, importCons, totalTechAndInvImports, autoR, w.nbp.referenceRate, gdp, rc)
      (fx, w.bop, 0.0, CentralBankLogic.FxInterventionResult(0.0, 0.0, w.nbp.fxReserves))

    // Adjust BOP for foreign dividend outflow (primary income component) + EU funds tracking
    val newBop1 = if foreignDividendOutflow > 0 && Config.OeEnabled then
      newBop0.copy(
        currentAccount = newBop0.currentAccount - foreignDividendOutflow,
        nfa = newBop0.nfa - foreignDividendOutflow
      )
    else newBop0
    // FDI composition (#33): profit shifting (service import) + repatriation (primary income debit)
    val fdiTotalBopDebit = sumProfitShifting + sumFdiRepatriation
    val newBop2 = if fdiTotalBopDebit > 0 && Config.FdiEnabled && Config.OeEnabled then
      newBop1.copy(
        currentAccount = newBop1.currentAccount - fdiTotalBopDebit,
        nfa = newBop1.nfa - fdiTotalBopDebit,
        tradeBalance = newBop1.tradeBalance - sumProfitShifting,
        totalImports = newBop1.totalImports + sumProfitShifting)
    else newBop1
    val fdiCitLoss = sumProfitShifting * Config.CitRate
    val newBop = newBop2.copy(
      euFundsMonthly = euMonthly,
      euCumulativeAbsorption = w.bop.euCumulativeAbsorption + euMonthly
    )

    val exRateChg = if rc.isEurozone then 0.0
                    else (newForex.exchangeRate / w.forex.exchangeRate) - 1.0
    val newRefRate = Sectors.updateCbRate(w.nbp.referenceRate, newInfl, exRateChg, employed, rc)

    // Expectations step: update after inflation + rate computed
    val unempRateForExp = 1.0 - employed.toDouble / Config.TotalPopulation
    val newExp = if Config.ExpEnabled then
      Expectations.step(w.expectations, newInfl, newRefRate, unempRateForExp, rc)
    else w.expectations

    // Reserve interest, standing facilities, interbank interest
    // These flows are zero in single-bank mode (no reserves/interbank tracked)
    // Computed from LAGGED bank state (w.bankingSector) — avoids circular dependency
    val (totalReserveInterest, totalStandingFacilityIncome, totalInterbankInterest) =
      w.bankingSector match
        case Some(bs) =>
          val (_, resInt) = BankingSector.computeReserveInterest(bs.banks, w.nbp.referenceRate)
          val (_, sfInc) = BankingSector.computeStandingFacilities(bs.banks, w.nbp.referenceRate)
          val (_, ibInt) = BankingSector.interbankInterestFlows(bs.banks, bs.interbankRate)
          (resInt, sfInc, ibInt)
        case None => (0.0, 0.0, 0.0)

    // --- Bond market + QE ---
    val annualGdpForBonds = w.gdpProxy * 12.0
    val debtToGdp = if annualGdpForBonds > 0 then w.gov.cumulativeDebt / annualGdpForBonds else 0.0
    // QE compression: use qeCumulative (active purchases), not total govBondHoldings.
    // Initial NBP holdings (COVID-era legacy) are already priced into the market;
    // only active QE purchases create additional term premium compression.
    val nbpBondGdpShare = if annualGdpForBonds > 0 then w.nbp.qeCumulative / annualGdpForBonds else 0.0
    // Channel 3: De-anchored expectations → higher bond yields
    val credPremium = if Config.ExpEnabled then
      val target = if rc.isEurozone then Config.EcbTargetInfl else Config.NbpTargetInfl
      (1.0 - w.expectations.credibility) *
        Math.abs(w.expectations.expectedInflation - target) *
        Config.ExpBondSensitivity
    else 0.0
    val newBondYield = CentralBankLogic.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, w.bop.nfa, credPremium)

    // Debt service: use LAGGED bond stock (standard SFC approach — avoids circular dependency)
    // Cap at 50% of monthly GDP (implicit sovereign default ceiling — only activates in pathological scenarios)
    val rawDebtService = w.gov.bondsOutstanding * newBondYield / 12.0
    val monthlyDebtService = Math.min(rawDebtService, w.gdpProxy * 0.50)
    val bankBondIncome = w.bank.govBondHoldings * newBondYield / 12.0
    val nbpBondIncome = w.nbp.govBondHoldings * newBondYield / 12.0
    // NBP remittance: bond income minus reserve interest paid to banks
    // and adjusted for standing facility net (lombard income − deposit facility cost)
    val nbpRemittance = nbpBondIncome - totalReserveInterest - totalStandingFacilityIncome

    // QE logic
    val qeActivate = CentralBankLogic.shouldActivateQe(newRefRate, newInfl)
    val qeTaper = CentralBankLogic.shouldTaperQe(newInfl)
    val qeActive = if qeActivate then true
                   else if qeTaper then false
                   else w.nbp.qeActive
    val preQeNbp = NbpState(newRefRate, w.nbp.govBondHoldings, qeActive, w.nbp.qeCumulative)
    val (postQeNbp, qePurchaseAmount) = CentralBankLogic.executeQe(
      preQeNbp, w.bank.govBondHoldings, annualGdpForBonds)
    val postFxNbp = postQeNbp.copy(
      fxReserves = fxResult.newReserves,
      lastFxTraded = fxResult.eurTraded
    )

    // --- Corporate bond market step (#40) ---
    val corpBondAmort = CorporateBondMarket.amortization(w.corporateBonds)
    val newCorpBonds = CorporateBondMarket.step(w.corporateBonds, newBondYield,
      w.bank.nplRatio, totalBondDefault, actualBondIssuance)
      .copy(lastAbsorptionRate = corpBondAbsorption)
    // Coupon computed from LAGGED state (standard SFC approach)
    val (_, corpBondBankCoupon, _) = CorporateBondMarket.computeCoupon(w.corporateBonds)
    val (_, _, corpBondBankDefaultLoss, _) = CorporateBondMarket.processDefaults(
      w.corporateBonds, totalBondDefault)

    // --- Insurance sector step (#41) ---
    val insUnempRate = 1.0 - employed.toDouble / Config.TotalPopulation
    val newInsurance = if Config.InsEnabled then
      InsuranceSector.step(w.insurance, employed, newWage, w.priceLevel, insUnempRate,
        newBondYield, w.corporateBonds.corpBondYield, w.equity.monthlyReturn)
    else w.insurance
    val insNetDepositChange = newInsurance.lastNetDepositChange

    // --- Shadow Banking / NBFI step (#42) ---
    val nbfiDepositRate = Math.max(0.0, postFxNbp.referenceRate - 0.02)
    val nbfiUnempRate = 1.0 - employed.toDouble / Config.TotalPopulation
    val newNbfi = if Config.NbfiEnabled then
      ShadowBanking.step(w.nbfi, employed, newWage, w.priceLevel,
        nbfiUnempRate, w.bank.nplRatio, newBondYield,
        w.corporateBonds.corpBondYield, w.equity.monthlyReturn,
        nbfiDepositRate, domesticCons)
    else w.nbfi
    val nbfiDepositDrain = newNbfi.lastDepositDrain

    val vat = consumption * Config.FofConsWeights.zip(Config.VatRates).map((w, r) => w * r).kahanSum
    val exciseRevenue = consumption * Config.FofConsWeights.zip(Config.ExciseRates).map((w, r) => w * r).kahanSum
    val customsDutyRevenue = if Config.OeEnabled then
      newBop.totalImports * Config.CustomsNonEuShare * Config.CustomsDutyRate
    else 0.0
    val unempBenefitSpend = hhAgg.map(_.totalUnempBenefits).getOrElse(aggUnempBenefit)
    val socialTransferSpend = if Config.Social800Enabled then
      hhAgg.map(_.totalSocialTransfers).getOrElse(
        Config.TotalPopulation.toDouble * Config.Social800ChildrenPerHh * Config.Social800Rate
      )
    else 0.0

    // Informal economy: aggregate tax evasion (#45)
    val informalCyclicalAdj = w.informalCyclicalAdj
    val effectiveShadowShare = if Config.InformalEnabled then
      Config.FofConsWeights.zip(Config.InformalSectorShares).map((cw, ss) =>
        cw * Math.min(1.0, ss + informalCyclicalAdj)).kahanSum
    else 0.0
    val vatAfterEvasion = if Config.InformalEnabled then
      vat * (1.0 - effectiveShadowShare * Config.InformalVatEvasion) else vat
    val exciseAfterEvasion = if Config.InformalEnabled then
      exciseRevenue * (1.0 - effectiveShadowShare * Config.InformalExciseEvasion) else exciseRevenue
    val pitAfterEvasion = if Config.InformalEnabled then
      pitRevenue * (1.0 - effectiveShadowShare * Config.InformalPitEvasion) else pitRevenue

    val newGov = Sectors.updateGov(w.gov, sumTax + dividendTax + pitAfterEvasion, vatAfterEvasion,
      bdpActive, bdp, newPrice, unempBenefitSpend,
      monthlyDebtService, nbpRemittance, newZus.govSubvention, socialTransferSpend,
      euCofinancing = euCofin, euProjectCapital = euProjectCapital,
      exciseRevenue = exciseAfterEvasion, customsDutyRevenue = customsDutyRevenue,
      govPurchasesActual = govPurchases)
    val newGovWithYield = newGov.copy(bondYield = newBondYield)

    // JST (local government) — must precede newBank (JST deposits flow into bank)
    val nLivingFirms = living2.length
    val (newJst, jstDepositChange) = JstLogic.step(
      w.jst, newGovWithYield.taxRevenue, totalIncome, gdp, nLivingFirms, pitAfterEvasion)

    // ---- Housing market step ----
    val unempRate = 1.0 - employed.toDouble / Config.TotalPopulation
    val prevMortgageRate = w.housing.avgMortgageRate
    // Mortgage rate: WIBOR_3M + spread (when term structure) or refRate + spread
    val mortgageBaseRate = if Config.InterbankTermStructure then
      w.bankingSector.map(bs => YieldCurve.compute(bs.interbankRate).wibor3m)
        .getOrElse(w.nbp.referenceRate)
    else w.nbp.referenceRate
    val mortgageRate = mortgageBaseRate + Config.ReMortgageSpread

    val housingAfterPrice = HousingMarket.step(w.housing, mortgageRate, newInfl,
      wageGrowth, employed, prevMortgageRate)
    val housingAfterOrig = HousingMarket.processOrigination(housingAfterPrice, totalIncome,
      mortgageRate, true)
    val (mortgageInterestIncome, mortgagePrincipal, mortgageDefaultLoss) =
      HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRate, unempRate)
    val mortgageDefaultAmount = if Config.ReMortgageRecovery < 1.0 then
      mortgageDefaultLoss / (1.0 - Config.ReMortgageRecovery)
    else 0.0
    val housingAfterFlows = HousingMarket.applyFlows(housingAfterOrig,
      mortgagePrincipal, mortgageDefaultAmount, mortgageInterestIncome)

    // BFG levy (#48): computed from lagged bank state
    val bfgLevy = if Config.BankFailureEnabled then
      w.bankingSector match
        case Some(bs) =>
          val (_, total) = BankingSector.computeBfgLevy(bs.banks)
          total
        case None =>
          w.bank.deposits * Config.BfgLevyRate / 12.0
    else 0.0

    // Investment net deposit flow: lagged investment demand (revenue to capital goods sectors)
    // minus current investment spending (domestic portion). Approximately zero when investment is stable.
    val currentInvestDomestic = sumGrossInvestment * (1.0 - Config.PhysCapImportShare) +
      sumGreenInvestment * (1.0 - Config.GreenImportShare)
    val investNetDepositFlow = laggedInvestDemand - currentInvestDomestic

    val newBank = w.bank.copy(
      totalLoans = Math.max(0, w.bank.totalLoans + sumNewLoans - nplNew * Config.LoanRecovery),
      nplAmount  = Math.max(0, w.bank.nplAmount + nplNew - w.bank.nplAmount * 0.05),
      capital    = w.bank.capital - nplLoss - mortgageDefaultLoss - consumerNplLoss
                   - corpBondBankDefaultLoss - bfgLevy
                   + intIncome * 0.3 + hhDebtService * 0.3
                   + bankBondIncome * 0.3 - depositInterestPaid * 0.3
                   + totalReserveInterest * 0.3 + totalStandingFacilityIncome * 0.3
                   + totalInterbankInterest * 0.3
                   + mortgageInterestIncome * 0.3
                   + consumerDebtService * 0.3
                   + corpBondBankCoupon * 0.3,
      deposits   = w.bank.deposits + (totalIncome - consumption) + investNetDepositFlow
                   + jstDepositChange
                   + netDomesticDividends - foreignDividendOutflow - remittanceOutflow + diasporaInflow
                   + tourismExport - tourismImport
                   + consumerOrigination + insNetDepositChange + nbfiDepositDrain,
      consumerLoans = Math.max(0.0, w.bank.consumerLoans + consumerOrigination - consumerPrincipal - consumerDefaultAmt),
      consumerNpl = Math.max(0.0, w.bank.consumerNpl + consumerDefaultAmt - w.bank.consumerNpl * 0.05),
      corpBondHoldings = newCorpBonds.bankHoldings)

    // Recompute hhAgg from final households if in individual mode
    // Carry retraining counters from the monthly step (hhAgg) — not zeros
    val monthlyRetAttempts  = hhAgg.map(_.retrainingAttempts).getOrElse(0)
    val monthlyRetSuccesses = hhAgg.map(_.retrainingSuccesses).getOrElse(0)
    val finalHhAgg = finalHouseholds.map { hhs =>
      HouseholdLogic.computeAggregates(hhs, newWage, resWage, importAdj,
        monthlyRetAttempts, monthlyRetSuccesses, bdp)
    }

    // Actual bond change: bondsOutstanding is floored at 0, so actual issuance
    // may differ from raw deficit when surplus exceeds outstanding bonds
    val actualBondChange = newGovWithYield.bondsOutstanding - w.gov.bondsOutstanding

    // PPK bond purchases (capped at available bonds after QE)
    val availableBondsForPpk = newBank.govBondHoldings +
      (if Config.GovBondMarket then actualBondChange else 0.0) - qePurchaseAmount
    val ppkBondPurchase = Math.min(rawPpkBondPurchase, Math.max(0.0, availableBondsForPpk))
    val finalPpk = newPpk.copy(bondHoldings = w.ppk.bondHoldings + ppkBondPurchase)

    // Insurance gov bond purchases (capped at available bonds after QE + PPK)
    val insGovBondDelta = newInsurance.govBondHoldings - w.insurance.govBondHoldings
    val availableBondsForIns = newBank.govBondHoldings +
      (if Config.GovBondMarket then actualBondChange else 0.0) - qePurchaseAmount - ppkBondPurchase
    val insBondPurchase = Math.max(0.0, Math.min(Math.max(0.0, insGovBondDelta), Math.max(0.0, availableBondsForIns)))
    val finalInsurance = newInsurance.copy(govBondHoldings = w.insurance.govBondHoldings + insBondPurchase)

    // TFI gov bond purchases (#42) (capped at available bonds after QE + PPK + insurance)
    val tfiGovBondDelta = newNbfi.tfiGovBondHoldings - w.nbfi.tfiGovBondHoldings
    val availableBondsForTfi = newBank.govBondHoldings +
      (if Config.GovBondMarket then actualBondChange else 0.0) -
      qePurchaseAmount - ppkBondPurchase - insBondPurchase
    val tfiBondPurchase = Math.max(0.0, Math.min(Math.max(0.0, tfiGovBondDelta), Math.max(0.0, availableBondsForTfi)))
    val finalNbfi = newNbfi.copy(tfiGovBondHoldings = w.nbfi.tfiGovBondHoldings + tfiBondPurchase)

    // Bond allocation: new issuance goes to bank; QE, PPK, insurance, TFI transfer from bank
    val finalBank = if Config.GovBondMarket then
      newBank.copy(govBondHoldings = newBank.govBondHoldings + actualBondChange - qePurchaseAmount - ppkBondPurchase - insBondPurchase - tfiBondPurchase)
    else newBank.copy(govBondHoldings = newBank.govBondHoldings - qePurchaseAmount - ppkBondPurchase - insBondPurchase - tfiBondPurchase)

    // ---- Multi-bank update path ----
    // Compute per-bank flows from lagged state
    val (perBankReserveInt, perBankStandingFac, perBankInterbankInt) =
      w.bankingSector match
        case Some(bs) =>
          val (ri, _) = BankingSector.computeReserveInterest(bs.banks, w.nbp.referenceRate)
          val (sf, _) = BankingSector.computeStandingFacilities(bs.banks, w.nbp.referenceRate)
          val (ib, _) = BankingSector.interbankInterestFlows(bs.banks, bs.interbankRate)
          (ri, sf, ib)
        case None =>
          (Vector.empty[Double], Vector.empty[Double], Vector.empty[Double])

    val (finalBankingSector, reassignedFirms, reassignedHouseholds, bailInLoss, multiCapDestruction) = w.bankingSector match
      case Some(bs) =>
        val totalWorkers = perBankWorkers.kahanSumBy(_.toDouble)
        // 1. Update each bank with its per-bank flows
        val updatedBanks = bs.banks.map { b =>
          val bId = b.id.toInt
          val bankNplNew = perBankNplDebt(bId)
          val bankNplLoss = bankNplNew * (1.0 - Config.LoanRecovery)
          val bankIntIncome = perBankIntIncome(bId)
          // Per-bank HH flows: exact from individual HH data, or worker-proportional proxy
          val (bankIncomeShare, bankConsShare, bankHhDebtService, bankDepInterest,
               bankCcDSvc, bankCcOrig, bankCcDef) =
            perBankHhFlowsOpt match
              case Some(pbf) =>
                (pbf.income(bId), pbf.consumption(bId), pbf.debtService(bId), pbf.depositInterest(bId),
                 if pbf.consumerDebtService.nonEmpty then pbf.consumerDebtService(bId) else 0.0,
                 if pbf.consumerOrigination.nonEmpty then pbf.consumerOrigination(bId) else 0.0,
                 if pbf.consumerDefault.nonEmpty then pbf.consumerDefault(bId) else 0.0)
              case None =>
                // Aggregate HH mode fallback: worker-proportional proxy
                val ws = if totalWorkers > 0 then perBankWorkers(bId) / totalWorkers else 0.0
                (totalIncome * ws, consumption * ws, hhDebtService * ws, 0.0,
                 consumerDebtService * ws, consumerOrigination * ws, consumerDefaultAmt * ws)
          val bankBondInc = b.govBondHoldings * newBondYield / 12.0
          // Per-bank monetary plumbing flows
          val bankResInt = if perBankReserveInt.nonEmpty then perBankReserveInt(bId) else 0.0
          val bankSfInc = if perBankStandingFac.nonEmpty then perBankStandingFac(bId) else 0.0
          val bankIbInt = if perBankInterbankInt.nonEmpty then perBankInterbankInt(bId) else 0.0
          val newLoansTotal = Math.max(0, b.loans + perBankNewLoans(bId) - bankNplNew * Config.LoanRecovery)
          // Dividend deposit flows: proportional to worker share
          val ws = if totalWorkers > 0 then perBankWorkers(bId) / totalWorkers else 0.0
          val bankDivInflow = netDomesticDividends * ws
          val bankDivOutflow = foreignDividendOutflow * ws
          val bankRemittance = remittanceOutflow * ws
          val bankDiasporaInflow = diasporaInflow * ws
          val bankTourismExport = tourismExport * ws
          val bankTourismImport = tourismImport * ws
          val bankInsDepChange = insNetDepositChange * ws
          val bankNbfiDepDrain = nbfiDepositDrain * ws
          val bankJstDepChange = jstDepositChange * ws
          val bankInvestNetFlow = investNetDepositFlow * ws
          val newDep = b.deposits + (bankIncomeShare - bankConsShare) + bankInvestNetFlow + bankJstDepChange + bankDivInflow - bankDivOutflow - bankRemittance + bankDiasporaInflow + bankTourismExport - bankTourismImport + bankCcOrig + bankInsDepChange + bankNbfiDepDrain
          // Per-bank mortgage flows (proportional to deposit share)
          val bankDepShare = if totalWorkers > 0 then perBankWorkers(bId) / totalWorkers else 0.0
          val bankMortgageIntIncome = mortgageInterestIncome * bankDepShare
          val bankMortgageNplLoss = mortgageDefaultLoss * bankDepShare
          val bankCcNplLoss = bankCcDef * (1.0 - Config.CcNplRecovery)
          // Per-bank consumer principal: use HH-tracked data when available
          val bankCcPrincipal = perBankHhFlowsOpt match
            case Some(pbf) if pbf.consumerPrincipal.nonEmpty => pbf.consumerPrincipal(bId)
            case _ =>
              if Config.CcAmortRate + (lendingBaseRate + Config.CcSpread) / 12.0 > 0 then
                bankCcDSvc * (Config.CcAmortRate / (Config.CcAmortRate + (lendingBaseRate + Config.CcSpread) / 12.0))
              else 0.0
          // Per-bank corp bond flows (proportional to deposit share)
          val bankCorpBondCoupon = corpBondBankCoupon * bankDepShare
          val bankCorpBondDefaultLoss = corpBondBankDefaultLoss * bankDepShare
          // BFG levy (#48): per-bank monthly levy on lagged deposits
          val bankBfgLevy = if Config.BankFailureEnabled && !b.failed then
            b.deposits * Config.BfgLevyRate / 12.0
          else 0.0
          b.copy(
            loans = newLoansTotal,
            nplAmount = Math.max(0, b.nplAmount + bankNplNew - b.nplAmount * 0.05),
            capital = b.capital - bankNplLoss - bankMortgageNplLoss - bankCcNplLoss
              - bankCorpBondDefaultLoss - bankBfgLevy + bankIntIncome * 0.3 +
              bankHhDebtService * 0.3 + bankBondInc * 0.3 - bankDepInterest * 0.3
              + bankResInt * 0.3 + bankSfInc * 0.3 + bankIbInt * 0.3
              + bankMortgageIntIncome * 0.3
              + bankCcDSvc * 0.3
              + bankCorpBondCoupon * 0.3,
            deposits = newDep,
            // Deposit split + loan maturity tracking
            demandDeposits = newDep * (1.0 - Config.BankTermDepositFrac),
            termDeposits = newDep * Config.BankTermDepositFrac,
            loansShort = newLoansTotal * 0.20,   // 20% short-term
            loansMedium = newLoansTotal * 0.30,   // 30% medium-term
            loansLong = newLoansTotal * 0.50,      // 50% long-term
            consumerLoans = Math.max(0.0, b.consumerLoans + bankCcOrig - bankCcPrincipal - bankCcDef),
            consumerNpl = Math.max(0.0, b.consumerNpl + bankCcDef - b.consumerNpl * 0.05),
            corpBondHoldings = newCorpBonds.bankHoldings * bankDepShare
          )
        }
        // 2. Interbank clearing
        val ibRate = BankingSector.interbankRate(updatedBanks, w.nbp.referenceRate)
        val afterInterbank = BankingSector.clearInterbank(updatedBanks, bs.configs, ibRate)
        // 3. Bond allocation (use actualBondChange, not raw deficit — surplus can't redeem below 0)
        val afterBonds = if Config.GovBondMarket then
          BankingSector.allocateBonds(afterInterbank, actualBondChange)
        else afterInterbank
        // 4. QE allocation
        val afterQe = BankingSector.allocateQePurchases(afterBonds, qePurchaseAmount)
        // 4b. PPK bond purchases
        val afterPpk = BankingSector.allocateQePurchases(afterQe, ppkBondPurchase)
        // 4c. Insurance gov bond purchases (#41)
        val afterIns = BankingSector.allocateQePurchases(afterPpk, insBondPurchase)
        // 4d. TFI gov bond purchases (#42)
        val afterTfi = BankingSector.allocateQePurchases(afterIns, tfiBondPurchase)
        // 5. Failure checks + bail-in + BFG resolution (pass ccyb for effective MinCAR)
        val (afterFailCheck, anyFailed) = BankingSector.checkFailures(afterTfi, m,
          ccyb = newMacropru.ccyb)
        val (afterBailIn, multiBailInLoss) = if anyFailed then BankingSector.applyBailIn(afterFailCheck) else (afterFailCheck, 0.0)
        val (afterResolve, rawAbsorberId) = if anyFailed then BankingSector.resolveFailures(afterBailIn)
                           else (afterBailIn, BankId.NoBank)
        // Guard: resolveFailures returns NoBank when no newly-failed bank has deposits > 0
        // (e.g. bank failed with zero deposits). Fall back to healthiest bank.
        val absorberId = if rawAbsorberId.toInt >= 0 then rawAbsorberId
                         else BankingSector.healthiestBankId(afterResolve)
        // Track capital destruction: sum of capital wiped when banks fail
        val multiCapDestruction = if anyFailed then
          afterTfi.zip(afterFailCheck).map { (pre, post) =>
            if !pre.failed && post.failed then pre.capital else 0.0
          }.kahanSum
        else 0.0
        // Compute term structure from O/N rate when enabled
        val curve = if Config.InterbankTermStructure then Some(YieldCurve.compute(ibRate)) else None
        val newBs = bs.copy(banks = afterResolve, interbankRate = ibRate, interbankCurve = curve)
        // 6. Reassign firm/HH bankIds: use absorber ID directly for consistency
        val reFirms = if anyFailed then
          rewiredFirms.map { f =>
            if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then f.copy(bankId = absorberId)
            else f
          }
        else rewiredFirms
        val reHouseholds = if anyFailed then
          finalHouseholds.map(_.map { h =>
            if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then h.copy(bankId = absorberId)
            else h
          })
        else finalHouseholds
        // 7. Sync aggregate BankState from individual banks
        val aggBank = newBs.aggregate
        (Some(newBs), reFirms, reHouseholds, multiBailInLoss, multiCapDestruction)
      case None =>
        (None, rewiredFirms, finalHouseholds, 0.0, 0.0)

    // Use multi-bank aggregate if present, otherwise use single-bank finalBank
    val resolvedBank = finalBankingSector.map(_.aggregate).getOrElse(finalBank)

    // Credit diagnostics (M1/M2)
    val monAgg = if Config.CreditDiagnostics then
      val totalReserves = finalBankingSector.map(_.banks.kahanSumBy(_.reservesAtNbp)).getOrElse(0.0)
      Some(MonetaryAggregates.compute(resolvedBank.deposits, totalReserves))
    else None

    // GPW: finalize equity state with HH equity wealth
    val (totalHhEquityWealth, totalWealthEffectAgg) = reassignedHouseholds match
      case Some(hhs) =>
        (hhs.kahanSumBy(_.equityWealth), 0.0)  // wealth effect already embedded in individual consumption
      case None =>
        // Aggregate mode: HH equity wealth = previous × (1 + return)
        if Config.GpwHhEquity && Config.GpwEnabled then
          val prevHhEq = w.equity.hhEquityWealth
          val newHhEq = prevHhEq * (1.0 + equityAfterIssuance.monthlyReturn)
          val wEffect = if equityAfterIssuance.monthlyReturn > 0 then
            (newHhEq - prevHhEq) * Config.GpwWealthEffectMpc
          else 0.0
          (newHhEq, wEffect)
        else (0.0, 0.0)

    val equityAfterStep = equityAfterIssuance.copy(
      hhEquityWealth = totalHhEquityWealth,
      lastWealthEffect = totalWealthEffectAgg,
      lastDomesticDividends = netDomesticDividends,
      lastForeignDividends = foreignDividendOutflow,
      lastDividendTax = dividendTax
    )

    // Flow-of-funds residual: closes by construction (should be ~0)
    // Uses pre-processing living firms (same as sectorCap computation).
    // After demand spillover, compare against adjusted demand (= achievable supply),
    // not raw demand (which may exceed total capacity when aggregate demand > supply).
    val fofResidual = {
      val totalFirmRev = (0 until SECTORS.length).map { s =>
        living.filter(_.sector.toInt == s).kahanSumBy(f =>
          FirmOps.capacity(f).toDouble * sectorMults(s) * w.priceLevel)
      }.kahanSum
      val adjustedDemand = sectorMults.indices.map { s =>
        sectorCap(s) * sectorMults(s) * w.priceLevel
      }.kahanSum
      totalFirmRev - adjustedDemand
    }

    // Informal economy: aggregate metrics and next-month cyclical adjustment (#45)
    val taxEvasionLoss = if Config.InformalEnabled then
      sumCitEvasion + (vat - vatAfterEvasion) + (pitRevenue - pitAfterEvasion) + (exciseRevenue - exciseAfterEvasion)
    else 0.0
    val informalEmployed = if Config.InformalEnabled then
      employed.toDouble * effectiveShadowShare else 0.0
    val newInformalCyclicalAdj = if Config.InformalEnabled then
      val unemp = 1.0 - employed.toDouble / Config.TotalPopulation
      val target = Math.max(0.0, unemp - Config.InformalUnempThreshold) * Config.InformalCyclicalSens
      // Exponential smoothing: informal economy expands/contracts over quarters, not overnight
      w.informalCyclicalAdj * Config.InformalSmoothing + target * (1.0 - Config.InformalSmoothing)
    else 0.0

    val newW = World(m, newInfl, newPrice, newGovWithYield, postFxNbp,
      resolvedBank, newForex,
      HhState(employed, newWage, resWage, totalIncome, consumption, domesticCons, importCons,
        minWageLevel = baseMinWage, minWagePriceLevel = updatedMinWagePriceLevel),
      autoR, hybR, gdp, newSigmas,
      ioFlows = totalIoPaid,
      bop = newBop,
      hhAgg = finalHhAgg,
      households = reassignedHouseholds,
      bankingSector = finalBankingSector,
      monetaryAgg = monAgg,
      jst = newJst,
      zus = newZus,
      ppk = finalPpk,
      demographics = newDemographics,
      macropru = newMacropru,
      equity = equityAfterStep,
      housing = housingAfterFlows,
      sectoralMobility = SectoralMobilityState(
        crossSectorHires = postFirmCrossSectorHires + hhAgg.map(_.crossSectorHires).getOrElse(0),
        voluntaryQuits = hhAgg.map(_.voluntaryQuits).getOrElse(0),
        sectorMobilityRate = finalHhAgg.map(_.sectorMobilityRate).getOrElse(0.0)
      ),
      gvc = newGvc,
      expectations = newExp,
      immigration = newImmig,
      corporateBonds = newCorpBonds,
      insurance = finalInsurance,
      nbfi = finalNbfi,
      sectorDemandMult = sectorMults,
      fofResidual = fofResidual,
      grossInvestment = sumGrossInvestment,
      fdiProfitShifting = sumProfitShifting,
      fdiRepatriation = sumFdiRepatriation,
      fdiCitLoss = fdiCitLoss,
      aggInventoryStock = aggInventoryStock,
      aggInventoryChange = aggInventoryChange,
      informalCyclicalAdj = newInformalCyclicalAdj,
      taxEvasionLoss = taxEvasionLoss,
      informalEmployed = informalEmployed,
      aggEnergyCost = sumEnergyCost,
      aggGreenCapital = aggGreenCapital,
      aggGreenInvestment = sumGreenInvestment,
      diasporaRemittanceInflow = diasporaInflow,
      tourismExport = tourismExport,
      tourismImport = tourismImport,
      bfgFundBalance = w.bfgFundBalance + bfgLevy,
      bailInLoss = bailInLoss)

    // SFC accounting check: verify exact balance-sheet identities every step
    val prevSnap = SfcCheck.snapshot(w, firms, households)
    val currSnap = SfcCheck.snapshot(newW, reassignedFirms, reassignedHouseholds)
    val sfcFlows = SfcCheck.MonthlyFlows(
      govSpending = newGovWithYield.bdpSpending + newGovWithYield.unempBenefitSpend
        + newGovWithYield.socialTransferSpend
        + govPurchases + monthlyDebtService + newZus.govSubvention
        + euCofin,
      govRevenue = sumTax + dividendTax + pitAfterEvasion + vatAfterEvasion + nbpRemittance + exciseAfterEvasion + customsDutyRevenue,
      nplLoss = nplLoss,
      interestIncome = intIncome,
      hhDebtService = hhDebtService,
      totalIncome = totalIncome,
      totalConsumption = consumption,
      newLoans = sumNewLoans,
      nplRecovery = nplNew * Config.LoanRecovery,
      currentAccount = newBop.currentAccount,
      valuationEffect = oeValuationEffect,
      bankBondIncome = bankBondIncome,
      qePurchase = qePurchaseAmount,
      newBondIssuance = if Config.GovBondMarket then actualBondChange else 0.0,
      depositInterestPaid = depositInterestPaid,
      reserveInterest = totalReserveInterest,
      standingFacilityIncome = totalStandingFacilityIncome,
      interbankInterest = totalInterbankInterest,
      jstDepositChange = jstDepositChange,
      jstSpending = newJst.spending,
      jstRevenue = newJst.revenue,
      zusContributions = newZus.contributions,
      zusPensionPayments = newZus.pensionPayments,
      zusGovSubvention = newZus.govSubvention,
      dividendIncome = netDomesticDividends,
      foreignDividendOutflow = foreignDividendOutflow,
      dividendTax = dividendTax,
      mortgageInterestIncome = mortgageInterestIncome,
      mortgageNplLoss = mortgageDefaultLoss,
      mortgageOrigination = housingAfterFlows.lastOrigination,
      mortgagePrincipalRepaid = mortgagePrincipal,
      mortgageDefaultAmount = mortgageDefaultAmount,
      remittanceOutflow = remittanceOutflow,
      fofResidual = fofResidual,
      consumerDebtService = consumerDebtService,
      consumerNplLoss = consumerNplLoss,
      consumerOrigination = consumerOrigination,
      consumerPrincipalRepaid = consumerPrincipal,
      consumerDefaultAmount = consumerDefaultAmt,
      corpBondCouponIncome = corpBondBankCoupon,
      corpBondDefaultLoss = corpBondBankDefaultLoss,
      corpBondIssuance = actualBondIssuance,
      corpBondAmortization = corpBondAmort,
      corpBondDefaultAmount = totalBondDefault,
      insNetDepositChange = insNetDepositChange,
      nbfiDepositDrain = nbfiDepositDrain,
      nbfiOrigination = finalNbfi.lastNbfiOrigination,
      nbfiRepayment = finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = sumProfitShifting,
      fdiRepatriation = sumFdiRepatriation,
      diasporaInflow = diasporaInflow,
      tourismExport = tourismExport,
      tourismImport = tourismImport,
      bfgLevy = bfgLevy,
      bailInLoss = bailInLoss,
      bankCapitalDestruction = multiCapDestruction,
      investNetDepositFlow = investNetDepositFlow,
      exports = newBop.exports,
      totalImports = newBop.totalImports,
      grossInvestment = sumGrossInvestment,
      greenInvestment = sumGreenInvestment,
      inventoryChange = aggInventoryChange
    )
    val sfcResult = SfcCheck.validate(m, prevSnap, currSnap, sfcFlows)
    if !sfcResult.passed then
      System.err.println(
        f"[SFC] Month $m FAIL:" +
        f" bankCap=${sfcResult.bankCapitalError}%.2f" +
        f" bankDep=${sfcResult.bankDepositsError}%.2f" +
        f" govDebt=${sfcResult.govDebtError}%.2f" +
        f" nfa=${sfcResult.nfaError}%.2f" +
        f" bondClr=${sfcResult.bondClearingError}%.2f" +
        f" ibNet=${sfcResult.interbankNettingError}%.2f" +
        f" jstDebt=${sfcResult.jstDebtError}%.2f" +
        f" fusBal=${sfcResult.fusBalanceError}%.2f" +
        f" mortgage=${sfcResult.mortgageStockError}%.2f" +
        f" fof=${sfcResult.fofError}%.2f" +
        f" ccStock=${sfcResult.consumerCreditError}%.2f" +
        f" corpBond=${sfcResult.corpBondStockError}%.2f" +
        f" nbfiCredit=${sfcResult.nbfiCreditError}%.2f" +
        f" secBal=${sfcResult.sectoralBalancesError}%.2f")

    // FDI M&A: monthly domestic → foreign conversion (#33)
    val postFdiFirms = if Config.FdiEnabled && Config.FdiMaProb > 0 then
      reassignedFirms.map { f =>
        if FirmOps.isAlive(f) && !f.foreignOwned &&
           f.initialSize >= Config.FdiMaSizeMin &&
           Random.nextDouble() < Config.FdiMaProb then
          f.copy(foreignOwned = true)
        else f
      }
    else reassignedFirms

    // Endogenous Firm Entry (#35): recycle bankrupt slots
    val (finalFirms, firmBirths) = if Config.FirmEntryEnabled then
      val postLiving = postFdiFirms.filter(FirmOps.isAlive)
      val sectorCashSum = Array.fill(SECTORS.length)(0.0)
      val sectorCashCnt = Array.fill(SECTORS.length)(0)
      for f <- postLiving do
        sectorCashSum(f.sector.toInt) += f.cash
        sectorCashCnt(f.sector.toInt) += 1
      val sectorAvgCash = SECTORS.indices.map(s =>
        if sectorCashCnt(s) > 0 then sectorCashSum(s) / sectorCashCnt(s) else 0.0).toArray
      val globalAvgCash = if postLiving.nonEmpty then
        postLiving.map(_.cash).sum / postLiving.length else 1.0
      val profitSignals = sectorAvgCash.map { c =>
        Math.max(-1.0, Math.min(2.0,
          (c - globalAvgCash) / Math.max(1.0, Math.abs(globalAvgCash))))
      }

      val sectorWeights = SECTORS.indices.map { s =>
        Math.max(0.01, (1.0 + profitSignals(s) * Config.FirmEntryProfitSens) *
          Config.FirmEntrySectorBarriers(s))
      }.toArray
      val totalWeight = sectorWeights.sum

      val totalAdoption = newW.automationRatio + newW.hybridRatio
      val livingIds = postLiving.map(_.id.toInt)
      var births = 0

      val result = postFdiFirms.map { f =>
        if !FirmOps.isAlive(f) then
          val slotSector = f.sector.toInt
          val entryProb = Config.FirmEntryRate * Config.FirmEntrySectorBarriers(slotSector) *
            Math.max(0.0, 1.0 + profitSignals(slotSector) * Config.FirmEntryProfitSens)
          if Random.nextDouble() < entryProb then
            births += 1
            val roll = Random.nextDouble() * totalWeight
            var cumul = 0.0
            var newSector = 0
            var found = false
            for s <- SECTORS.indices if !found do
              cumul += sectorWeights(s)
              if roll < cumul then { newSector = s; found = true }

            val firmSize = Math.max(1, Random.between(1, 10))
            val sizeMult = firmSize.toDouble / Config.WorkersPerFirm

            val isAiNative = totalAdoption > Config.FirmEntryAiThreshold &&
              Random.nextDouble() < Config.FirmEntryAiProb
            val dr = if isAiNative then Random.between(0.50, 0.90)
                     else Math.max(0.02, Math.min(0.30,
                       SECTORS(newSector).baseDigitalReadiness + Random.nextGaussian() * 0.10))
            val startWorkers = if households.isDefined then 0 else firmSize
            val tech = if isAiNative then
              val hw = Math.max(1, (startWorkers * 0.6).toInt)
              TechState.Hybrid(hw, 0.5 + Random.nextDouble() * 0.3)
            else TechState.Traditional(startWorkers)

            val nNeighbors = Math.min(6, livingIds.length)
            val newNeighbors = if nNeighbors > 0 then
              Random.shuffle(livingIds.toList).take(nNeighbors).toArray
            else Array.empty[Int]

            val newBankId = if Config.BankMulti then
              BankingSector.assignBank(SectorIdx(newSector), BankingSector.DefaultConfigs, Random)
            else BankId(0)

            val foreignOwned = Config.FdiEnabled &&
              Random.nextDouble() < Config.FdiForeignShares(newSector)

            val capitalStock = if Config.PhysCapEnabled then
              firmSize.toDouble * Config.PhysCapKLRatios(newSector)
            else 0.0

            val initInventory = if Config.InventoryEnabled then
              val cap = Config.BaseRevenue * (firmSize.toDouble / Config.WorkersPerFirm) *
                SECTORS(newSector).revenueMultiplier
              cap * Config.InventoryTargetRatios(newSector) * Config.InventoryInitRatio
            else 0.0

            val initGreenK = if Config.EnergyEnabled then
              firmSize.toDouble * Config.GreenKLRatios(newSector) * Config.GreenInitRatio
            else 0.0

            Firm(
              id = f.id,
              cash = Config.FirmEntryStartupCash * sizeMult,
              debt = 0.0,
              tech = tech,
              riskProfile = Random.between(0.1, 0.9),
              innovationCostFactor = Random.between(0.8, 1.5),
              digitalReadiness = dr,
              sector = SectorIdx(newSector),
              neighbors = newNeighbors,
              bankId = newBankId,
              initialSize = firmSize,
              capitalStock = capitalStock,
              foreignOwned = foreignOwned,
              inventory = initInventory,
              greenCapital = initGreenK
            )
          else f
        else f
      }
      (result, births)
    else (postFdiFirms, 0)

    val finalW = newW.copy(firmBirths = firmBirths, firmDeaths = firmDeaths)
    (finalW, finalFirms, reassignedHouseholds)
