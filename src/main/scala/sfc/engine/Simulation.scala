package sfc.engine

import sfc.config.{Config, SECTORS, HH_MODE, HhMode, RunConfig}
import sfc.agents.*
import sfc.sfc.*
import sfc.networks.Network
import sfc.dynamics.{SigmaDynamics, DynamicNetwork}

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
      val outputGap = (unempRate - Config.NbpNairu) / Config.NbpNairu
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * infGap -
        Config.TaylorDelta * outputGap +
        Config.TaylorBeta  * exRateChange
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, smoothed))
    else
      // v1.0 legacy: asymmetric Taylor (inflation-only)
      val infGap  = inflation - Config.NbpTargetInfl
      val taylor  = Config.NbpNeutralRate +
        Config.TaylorAlpha * Math.max(0.0, infGap) +
        Config.TaylorBeta  * Math.max(0.0, exRateChange)
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, smoothed))

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
    zusGovSubvention: Double = 0.0): GovState =
    val bdpSpend   = if bdpActive then Config.TotalPopulation.toDouble * bdpAmount else 0.0
    val totalSpend = bdpSpend + unempBenefitSpend + Config.GovBaseSpending * priceLevel + debtService + zusGovSubvention
    val totalRev   = citPaid + vat + nbpRemittance
    val deficit    = totalSpend - totalRev
    val newBondsOutstanding = if Config.GovBondMarket then prev.bondsOutstanding + deficit
                              else prev.bondsOutstanding
    GovState(bdpActive, totalRev, bdpSpend, deficit, prev.cumulativeDebt + deficit,
      unempBenefitSpend, newBondsOutstanding, prev.bondYield, debtService)

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
    val resWage = Config.BaseReservationWage + bdp * Config.ReservationBdpMult

    // When term structure is enabled, use WIBOR 3M as lending base rate
    // instead of refRate. Uses LAGGED interbankRate (standard SFC approach).
    val lendingBaseRate = if Config.InterbankTermStructure then
      w.bankingSector.map { bs =>
        YieldCurve.compute(bs.interbankRate).wibor3m
      }.getOrElse(w.nbp.referenceRate)
    else w.nbp.referenceRate

    val living = firms.filter(FirmOps.isAlive)
    val laborDemand = living.map(FirmOps.workers).sum
    val (newWage, rawEmployed) = Sectors.updateLaborMarket(w.hh.marketWage, resWage, laborDemand)

    // Demographics caps employment at working-age population
    val employed = if Config.DemEnabled then
      Math.min(rawEmployed, w.demographics.workingAgePop)
    else rawEmployed
    val newDemographics = PublicSectorLogic.demographicsStep(w.demographics, employed)

    // ZUS (contributions + pensions) and PPK
    val newZus = PublicSectorLogic.zusStep(w.zus.fusBalance, employed, newWage, newDemographics.retirees)
    val newPpk = PublicSectorLogic.ppkStep(w.ppk.bondHoldings, employed, newWage)
    val rawPpkBondPurchase = PublicSectorLogic.ppkBondPurchase(newPpk)

    val wageGrowth = if w.hh.marketWage > 0 then newWage / w.hh.marketWage - 1.0 else 0.0

    // Import adjustment (used by both paths)
    val importAdj = Config.ImportPropensity *
      Math.pow(Config.BaseExRate / w.forex.exchangeRate, 0.5)

    // ---- Aggregate vs Individual household path ----
    val (totalIncome, consumption, importCons, domesticCons, updatedHouseholds, hhAgg, perBankHhFlowsOpt) =
      households match
        case None =>
          // Aggregate mode (+ ZUS deductions/pensions)
          val wageIncome = employed.toDouble * newWage
          val bdpIncome  = if bdpActive then Config.TotalPopulation.toDouble * bdp else 0.0
          val ti = wageIncome + bdpIncome - newZus.contributions + newZus.pensionPayments
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
          // Household monthly step
          val (newHhs, agg, pbf) = HouseholdLogic.step(
            afterWages, w, bdp, newWage, resWage, importAdj, Random, nBanksHh, hhBankRates)
          (agg.totalIncome, agg.consumption, agg.importConsumption,
           agg.domesticConsumption, Some(newHhs), Some(agg), pbf)

    val baseIncome = Config.TotalPopulation.toDouble * Config.BaseWage
    val demandMult = 1.0 + (totalIncome / baseIncome - 1.0) *
      Config.Mpc * (1.0 - Math.min(0.65, importAdj)) * Config.DemandPassthrough

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

    val macro4firms = w.copy(
      month = m, demandMultiplier = demandMult,
      hh = w.hh.copy(marketWage = newWage, reservationWage = resWage))

    // Process firms -- per-firm dispatch to bank, accumulate per-bank flows
    val newFirms = firms.map { f =>
      val firmRate = getLendRate(f.bankId)
      val firmCanLend: Double => Boolean = amt => bankCanLendFn(f.bankId, amt)
      val r = FirmLogic.process(f, macro4firms, firmRate, firmCanLend, firms, rc)
      sumTax      += r.taxPaid
      sumCapex    += r.capexSpent
      sumTechImp  += r.techImports
      sumNewLoans += r.newLoan
      perBankNewLoans(f.bankId) += r.newLoan
      r.firm
    }

    // Track per-bank workers for deposit partitioning
    for f <- newFirms if FirmOps.isAlive(f) do
      perBankWorkers(f.bankId) += FirmOps.workers(f)

    // I-O intermediate market (Paper-07)
    val (ioFirms, totalIoPaid) = if Config.IoEnabled then
      val r = IntermediateMarket.process(newFirms, demandMult, w.priceLevel,
        Config.IoMatrix, Config.IoColumnSums, Config.IoScale)
      (r.firms, r.totalPaid)
    else
      (newFirms, 0.0)

    // Individual mode: post-firm-processing labor market update
    val finalHouseholds = updatedHouseholds.map { hhs =>
      val afterSep = LaborMarket.separations(hhs, firms, ioFirms)
      val afterSearch = LaborMarket.jobSearch(afterSep, ioFirms, newWage, Random)
      LaborMarket.updateWages(afterSearch, newWage)  // normalize wages for next step
    }

    val prevAlive = firms.filter(FirmOps.isAlive).map(_.id).toSet
    val newlyDead = ioFirms.filter(f => !FirmOps.isAlive(f) && prevAlive.contains(f.id))
    val nplNew    = newlyDead.map(_.debt).sum
    val nplLoss   = nplNew * (1.0 - Config.LoanRecovery)

    // Per-bank NPL attribution
    for f <- newlyDead do perBankNplDebt(f.bankId) += f.debt

    // Per-bank interest income
    for f <- firms if FirmOps.isAlive(f) do
      perBankIntIncome(f.bankId) += f.debt * getLendRate(f.bankId) / 12.0

    val intIncome = perBankIntIncome.sum

    // SFC: household debt service flows to bank as interest income
    val hhDebtService = hhAgg.map(_.totalDebtService).getOrElse(0.0)
    // SFC: deposit interest paid to HH (monetary transmission channel 2)
    val depositInterestPaid = hhAgg.map(_.totalDepositInterest).getOrElse(0.0)
    // Note: newBank construction deferred until after bond market block (needs bankBondIncome)

    val living2 = ioFirms.filter(FirmOps.isAlive)
    val nLiving = living2.length.toDouble
    val autoR   = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]) / nLiving else 0.0
    val hybR    = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]) / nLiving else 0.0
    val gdp     = domesticCons + Config.GovBaseSpending + w.forex.exports

    // Macroprudential — compute CCyB from credit-to-GDP gap
    val totalSystemLoans = w.bankingSector.map(_.banks.map(_.loans).sum).getOrElse(w.bank.totalLoans)
    val newMacropru = Macroprudential.step(w.macropru, totalSystemLoans, gdp)

    // Endogenous sigma evolution (Paper-05)
    val sectorAdoption = SECTORS.indices.map { s =>
      val secFirms = living2.filter(_.sector == s)
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
      w.inflation, w.priceLevel, demandMult, wageGrowth, exDev, autoR, hybR, rc)

    // Open economy (Paper-08) or legacy foreign sector
    val sectorOutputs = (0 until 6).map { s =>
      living2.filter(_.sector == s).map(f => FirmOps.capacity(f) * demandMult * w.priceLevel).sum
    }.toVector

    val (newForex, newBop, oeValuationEffect, fxResult) = if Config.OeEnabled then
      val oeResult = OpenEconomy.step(
        w.bop, w.forex, importCons, sumTechImp,
        autoR, w.nbp.referenceRate, gdp, w.priceLevel,
        sectorOutputs, m, rc,
        nbpFxReserves = w.nbp.fxReserves)
      (oeResult.forex, oeResult.bop, oeResult.valuationEffect, oeResult.fxIntervention)
    else
      val fx = Sectors.updateForeign(w.forex, importCons, sumTechImp, autoR, w.nbp.referenceRate, gdp, rc)
      (fx, w.bop, 0.0, CentralBankLogic.FxInterventionResult(0.0, 0.0, w.nbp.fxReserves))

    val exRateChg = if rc.isEurozone then 0.0
                    else (newForex.exchangeRate / w.forex.exchangeRate) - 1.0
    val newRefRate = Sectors.updateCbRate(w.nbp.referenceRate, newInfl, exRateChg, employed, rc)

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
    val nbpBondGdpShare = if annualGdpForBonds > 0 then w.nbp.govBondHoldings / annualGdpForBonds else 0.0
    val newBondYield = CentralBankLogic.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, w.bop.nfa)

    // Debt service: use LAGGED bond stock (standard SFC approach — avoids circular dependency)
    val monthlyDebtService = w.gov.bondsOutstanding * newBondYield / 12.0
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

    val vat = consumption * Config.VatRate
    val unempBenefitSpend = hhAgg.map(_.totalUnempBenefits).getOrElse(0.0)
    val newGov = Sectors.updateGov(w.gov, sumTax, vat, bdpActive, bdp, newPrice, unempBenefitSpend,
      monthlyDebtService, nbpRemittance, newZus.govSubvention)
    val newGovWithYield = newGov.copy(bondYield = newBondYield)

    // JST (local government) — must precede newBank (JST deposits flow into bank)
    val nLivingFirms = living2.length
    val (newJst, jstDepositChange) = JstLogic.step(
      w.jst, newGovWithYield.taxRevenue, totalIncome, gdp, nLivingFirms)

    val newBank = w.bank.copy(
      totalLoans = Math.max(0, w.bank.totalLoans + sumNewLoans - nplNew * Config.LoanRecovery),
      nplAmount  = Math.max(0, w.bank.nplAmount + nplNew - w.bank.nplAmount * 0.05),
      capital    = w.bank.capital - nplLoss + intIncome * 0.3 + hhDebtService * 0.3
                   + bankBondIncome * 0.3 - depositInterestPaid * 0.3
                   + totalReserveInterest * 0.3 + totalStandingFacilityIncome * 0.3
                   + totalInterbankInterest * 0.3,
      deposits   = w.bank.deposits + (totalIncome - consumption) + jstDepositChange)

    // Recompute hhAgg from final households if in individual mode
    // Carry retraining counters from the monthly step (hhAgg) — not zeros
    val monthlyRetAttempts  = hhAgg.map(_.retrainingAttempts).getOrElse(0)
    val monthlyRetSuccesses = hhAgg.map(_.retrainingSuccesses).getOrElse(0)
    val finalHhAgg = finalHouseholds.map { hhs =>
      HouseholdLogic.computeAggregates(hhs, newWage, resWage, importAdj,
        monthlyRetAttempts, monthlyRetSuccesses, bdp)
    }

    // PPK bond purchases (capped at available bonds after QE)
    val availableBondsForPpk = newBank.govBondHoldings +
      (if Config.GovBondMarket then newGovWithYield.deficit else 0.0) - qePurchaseAmount
    val ppkBondPurchase = Math.min(rawPpkBondPurchase, Math.max(0.0, availableBondsForPpk))
    val finalPpk = newPpk.copy(bondHoldings = w.ppk.bondHoldings + ppkBondPurchase)

    // Bond allocation: new issuance goes to bank; QE and PPK transfer from bank
    val finalBank = if Config.GovBondMarket then
      newBank.copy(govBondHoldings = newBank.govBondHoldings + newGovWithYield.deficit - qePurchaseAmount - ppkBondPurchase)
    else newBank.copy(govBondHoldings = newBank.govBondHoldings - qePurchaseAmount - ppkBondPurchase)

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

    val (finalBankingSector, reassignedFirms, reassignedHouseholds) = w.bankingSector match
      case Some(bs) =>
        val totalWorkers = perBankWorkers.sum.toDouble
        // 1. Update each bank with its per-bank flows
        val updatedBanks = bs.banks.map { b =>
          val bId = b.id
          val bankNplNew = perBankNplDebt(bId)
          val bankNplLoss = bankNplNew * (1.0 - Config.LoanRecovery)
          val bankIntIncome = perBankIntIncome(bId)
          // Per-bank HH flows: exact from individual HH data, or worker-proportional proxy
          val (bankIncomeShare, bankConsShare, bankHhDebtService, bankDepInterest) =
            perBankHhFlowsOpt match
              case Some(pbf) =>
                (pbf.income(bId), pbf.consumption(bId), pbf.debtService(bId), pbf.depositInterest(bId))
              case None =>
                // Aggregate HH mode fallback: worker-proportional proxy
                val ws = if totalWorkers > 0 then perBankWorkers(bId) / totalWorkers else 0.0
                (totalIncome * ws, consumption * ws, hhDebtService * ws, 0.0)
          val bankBondInc = b.govBondHoldings * newBondYield / 12.0
          // Per-bank monetary plumbing flows
          val bankResInt = if perBankReserveInt.nonEmpty then perBankReserveInt(bId) else 0.0
          val bankSfInc = if perBankStandingFac.nonEmpty then perBankStandingFac(bId) else 0.0
          val bankIbInt = if perBankInterbankInt.nonEmpty then perBankInterbankInt(bId) else 0.0
          val newLoansTotal = Math.max(0, b.loans + perBankNewLoans(bId) - bankNplNew * Config.LoanRecovery)
          val newDep = b.deposits + (bankIncomeShare - bankConsShare)
          b.copy(
            loans = newLoansTotal,
            nplAmount = Math.max(0, b.nplAmount + bankNplNew - b.nplAmount * 0.05),
            capital = b.capital - bankNplLoss + bankIntIncome * 0.3 +
              bankHhDebtService * 0.3 + bankBondInc * 0.3 - bankDepInterest * 0.3
              + bankResInt * 0.3 + bankSfInc * 0.3 + bankIbInt * 0.3,
            deposits = newDep,
            // Deposit split + loan maturity tracking
            demandDeposits = newDep * (1.0 - Config.BankTermDepositFrac),
            termDeposits = newDep * Config.BankTermDepositFrac,
            loansShort = newLoansTotal * 0.20,   // 20% short-term
            loansMedium = newLoansTotal * 0.30,   // 30% medium-term
            loansLong = newLoansTotal * 0.50       // 50% long-term
          )
        }
        // 2. Interbank clearing
        val ibRate = BankingSector.interbankRate(updatedBanks, w.nbp.referenceRate)
        val afterInterbank = BankingSector.clearInterbank(updatedBanks, bs.configs, ibRate)
        // 3. Bond allocation
        val afterBonds = if Config.GovBondMarket then
          BankingSector.allocateBonds(afterInterbank, newGovWithYield.deficit)
        else afterInterbank
        // 4. QE allocation
        val afterQe = BankingSector.allocateQePurchases(afterBonds, qePurchaseAmount)
        // 4b. PPK bond purchases
        val afterPpk = BankingSector.allocateQePurchases(afterQe, ppkBondPurchase)
        // 5. Failure checks + BFG resolution (pass ccyb for effective MinCAR)
        val (afterFailCheck, anyFailed) = BankingSector.checkFailures(afterPpk, m,
          ccyb = newMacropru.ccyb)
        val afterResolve = if anyFailed then BankingSector.resolveFailures(afterFailCheck)
                           else afterFailCheck
        // Compute term structure from O/N rate when enabled
        val curve = if Config.InterbankTermStructure then Some(YieldCurve.compute(ibRate)) else None
        val newBs = bs.copy(banks = afterResolve, interbankRate = ibRate, interbankCurve = curve)
        // 6. Reassign firm/HH bankIds if any failure occurred
        val reFirms = if anyFailed then
          rewiredFirms.map(f => f.copy(bankId = BankingSector.reassignBankId(f.bankId, afterResolve)))
        else rewiredFirms
        val reHouseholds = if anyFailed then
          finalHouseholds.map(_.map(h => h.copy(bankId = BankingSector.reassignBankId(h.bankId, afterResolve))))
        else finalHouseholds
        // 7. Sync aggregate BankState from individual banks
        val aggBank = newBs.aggregate
        (Some(newBs), reFirms, reHouseholds)
      case None =>
        (None, rewiredFirms, finalHouseholds)

    // Use multi-bank aggregate if present, otherwise use single-bank finalBank
    val resolvedBank = finalBankingSector.map(_.aggregate).getOrElse(finalBank)

    // Credit diagnostics (M1/M2)
    val monAgg = if Config.CreditDiagnostics then
      val totalReserves = finalBankingSector.map(_.banks.map(_.reservesAtNbp).sum).getOrElse(0.0)
      Some(MonetaryAggregates.compute(resolvedBank.deposits, totalReserves))
    else None

    val newW = World(m, newInfl, newPrice, demandMult, newGovWithYield, postFxNbp,
      resolvedBank, newForex,
      HhState(employed, newWage, resWage, totalIncome, consumption, domesticCons, importCons),
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
      macropru = newMacropru)

    // SFC accounting check: verify exact balance-sheet identities every step
    val prevSnap = SfcCheck.snapshot(w, firms, households)
    val currSnap = SfcCheck.snapshot(newW, reassignedFirms, reassignedHouseholds)
    val sfcFlows = SfcCheck.MonthlyFlows(
      govSpending = newGovWithYield.bdpSpending + newGovWithYield.unempBenefitSpend
        + Config.GovBaseSpending * newPrice + monthlyDebtService + newZus.govSubvention,
      govRevenue = sumTax + vat + nbpRemittance,
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
      newBondIssuance = if Config.GovBondMarket then newGovWithYield.deficit else 0.0,
      depositInterestPaid = depositInterestPaid,
      reserveInterest = totalReserveInterest,
      standingFacilityIncome = totalStandingFacilityIncome,
      interbankInterest = totalInterbankInterest,
      jstDepositChange = jstDepositChange,
      jstSpending = newJst.spending,
      jstRevenue = newJst.revenue,
      zusContributions = newZus.contributions,
      zusPensionPayments = newZus.pensionPayments,
      zusGovSubvention = newZus.govSubvention
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
        f" fusBal=${sfcResult.fusBalanceError}%.2f")

    (newW, reassignedFirms, reassignedHouseholds)
