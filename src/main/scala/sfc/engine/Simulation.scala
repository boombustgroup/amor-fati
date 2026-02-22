package sfc.engine

import sfc.config.{Config, SECTORS, RunConfig}
import sfc.agents.*
import sfc.sfc.*
import sfc.networks.Network

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
    val importPush    = if rc.isEurozone then 0.0
                        else Math.max(0.0, exRateDeviation) * Config.ImportPropensity * 0.25
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
    rc: RunConfig): Double =
    if rc.isEurozone then
      // ECB Taylor rule reacting to Eurozone-wide inflation (exogenous to Poland)
      val infGap = Config.EuroInflation - Config.EcbTargetInfl
      val taylor = Config.EcbNeutralRate +
        Config.EcbAlpha * Math.max(0.0, infGap)
      val smoothed = prevRate * Config.EcbInertia + taylor * (1.0 - Config.EcbInertia)
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, smoothed))
    else
      // NBP Taylor rule reacting to Polish inflation + exchange rate
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
    bdpActive: Boolean, bdpAmount: Double, priceLevel: Double): GovState =
    val bdpSpend   = if bdpActive then Config.TotalPopulation.toDouble * bdpAmount else 0.0
    val totalSpend = bdpSpend + Config.GovBaseSpending * priceLevel
    val totalRev   = citPaid + vat
    val deficit    = totalSpend - totalRev
    GovState(bdpActive, totalRev, bdpSpend, deficit, prev.cumulativeDebt + deficit)

object Simulation:
  def step(w: World, firms: Array[Firm], rc: RunConfig): (World, Array[Firm]) =
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

    val living = firms.filter(FirmOps.isAlive)
    val laborDemand = living.map(FirmOps.workers).sum
    val (newWage, employed) = Sectors.updateLaborMarket(w.hh.marketWage, resWage, laborDemand)
    val wageGrowth = if w.hh.marketWage > 0 then newWage / w.hh.marketWage - 1.0 else 0.0

    val wageIncome = employed.toDouble * newWage
    val bdpIncome  = if bdpActive then Config.TotalPopulation.toDouble * bdp else 0.0
    val totalIncome = wageIncome + bdpIncome
    val consumption = totalIncome * Config.Mpc

    val importAdj = Config.ImportPropensity *
      Math.pow(Config.BaseExRate / w.forex.exchangeRate, 0.5)
    val importCons = consumption * Math.min(0.65, importAdj)
    val domesticCons = consumption - importCons

    val baseIncome = Config.TotalPopulation.toDouble * Config.BaseWage
    val demandMult = 1.0 + (totalIncome / baseIncome - 1.0) *
      Config.Mpc * (1.0 - Math.min(0.65, importAdj)) * Config.DemandPassthrough

    val lendRate = w.bank.lendingRate(w.nbp.referenceRate)
    val bankCanLend: Double => Boolean = { amt =>
      val approvalP = Math.max(0.1, 1.0 - w.bank.nplRatio * 3.0)
      w.bank.canLend(amt) && Random.nextDouble() < approvalP
    }

    var sumTax      = 0.0
    var sumCapex    = 0.0
    var sumTechImp  = 0.0
    var sumNewLoans = 0.0

    val macro4firms = w.copy(
      month = m, demandMultiplier = demandMult,
      hh = w.hh.copy(marketWage = newWage, reservationWage = resWage))

    // Process firms -- pass allFirms for network lookups
    val newFirms = firms.map { f =>
      val r = FirmLogic.process(f, macro4firms, lendRate, bankCanLend, firms, rc)
      sumTax      += r.taxPaid
      sumCapex    += r.capexSpent
      sumTechImp  += r.techImports
      sumNewLoans += r.newLoan
      r.firm
    }

    val prevAlive = firms.filter(FirmOps.isAlive).map(_.id).toSet
    val newlyDead = newFirms.filter(f => !FirmOps.isAlive(f) && prevAlive.contains(f.id))
    val nplNew    = newlyDead.map(_.debt).sum
    val nplLoss   = nplNew * (1.0 - Config.LoanRecovery)
    val intIncome = firms.filter(FirmOps.isAlive).map(_.debt * lendRate / 12.0).sum

    val newBank = w.bank.copy(
      totalLoans = Math.max(0, w.bank.totalLoans + sumNewLoans - nplNew * Config.LoanRecovery),
      nplAmount  = Math.max(0, w.bank.nplAmount + nplNew - w.bank.nplAmount * 0.05),
      capital    = w.bank.capital - nplLoss + intIncome * 0.3,
      deposits   = w.bank.deposits + (totalIncome - consumption))

    val living2 = newFirms.filter(FirmOps.isAlive)
    val nLiving = living2.length.toDouble
    val autoR   = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]) / nLiving else 0.0
    val hybR    = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]) / nLiving else 0.0
    val gdp     = domesticCons + Config.GovBaseSpending + w.forex.exports

    val exDev = if rc.isEurozone then 0.0
               else (w.forex.exchangeRate / Config.BaseExRate) - 1.0
    val (newInfl, newPrice) = Sectors.updateInflation(
      w.inflation, w.priceLevel, demandMult, wageGrowth, exDev, autoR, hybR, rc)

    val newForex = Sectors.updateForeign(
      w.forex, importCons, sumTechImp, autoR, w.nbp.referenceRate, gdp, rc)

    val exRateChg = if rc.isEurozone then 0.0
                    else (newForex.exchangeRate / w.forex.exchangeRate) - 1.0
    val newRefRate = Sectors.updateCbRate(w.nbp.referenceRate, newInfl, exRateChg, rc)

    val vat = consumption * Config.VatRate
    val newGov = Sectors.updateGov(w.gov, sumTax, vat, bdpActive, bdp, newPrice)

    val newW = World(m, newInfl, newPrice, demandMult, newGov, NbpState(newRefRate),
      newBank, newForex,
      HhState(employed, newWage, resWage, totalIncome, consumption, domesticCons, importCons),
      autoR, hybR, gdp)
    (newW, newFirms)
