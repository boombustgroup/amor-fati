package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.dynamics.{DynamicNetwork, SigmaDynamics}
import sfc.engine.{EuFunds, EquityMarket, Macroprudential, Sectors, World}
import sfc.types.*
import sfc.util.KahanSum.*

object PriceEquityStep:

  case class Input(
    ioFirms: Array[Firm],
    w: World,
    sectorMults: Vector[Double],
    newWage: Double,
    wageGrowth: Double,
    avgDemandMult: Double,
    m: Int,
    sumGrossInvestment: Double,
    sumGreenInvestment: Double,
    sumEquityIssuance: Double,
    sumInventoryChange: Double,
    domesticCons: Double,
    lendingRates: Array[Double],
    rc: RunConfig
  )

  case class Output(
    autoR: Double,
    hybR: Double,
    aggInventoryStock: Double,
    aggGreenCapital: Double,
    euMonthly: Double,
    euCofin: Double,
    euProjectCapital: Double,
    gdp: Double,
    newMacropru: Macroprudential.State,
    newSigmas: Vector[Double],
    rewiredFirms: Array[Firm],
    newInfl: Double,
    newPrice: Double,
    equityAfterIssuance: EquityMarket.State,
    netDomesticDividends: Double,
    foreignDividendOutflow: Double,
    dividendTax: Double,
    firmProfits: Double,
    domesticGFCF: Double,
    investmentImports: Double,
    aggInventoryChange: Double
  )

  def run(in: Input): Output =
    val living2 = in.ioFirms.filter(FirmOps.isAlive)
    val nLiving = living2.length.toDouble
    val autoR   = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]) / nLiving else 0.0
    val hybR    = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]) / nLiving else 0.0
    val aggInventoryStock = if Config.InventoryEnabled then
      living2.kahanSumBy(_.inventory.toDouble) else 0.0
    val aggGreenCapital = if Config.EnergyEnabled then
      living2.kahanSumBy(_.greenCapital.toDouble) else 0.0

    val euMonthly = if Config.EuFundsEnabled then EuFunds.monthlyTransfer(in.m)
                    else Config.OeEuTransfers

    val govGdpContribution = if Config.GovInvestEnabled then
      Config.GovBaseSpending * (1.0 - Config.GovInvestShare) * Config.GovCurrentMultiplier +
      Config.GovBaseSpending * Config.GovInvestShare * Config.GovCapitalMultiplier
    else Config.GovBaseSpending
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
      in.sumGreenInvestment * (1.0 - Config.GreenImportShare) else 0.0
    val domesticGFCF = (if Config.PhysCapEnabled then
      in.sumGrossInvestment * (1.0 - Config.PhysCapImportShare) else 0.0) + greenDomesticGFCF
    val investmentImports = (if Config.PhysCapEnabled then
      in.sumGrossInvestment * Config.PhysCapImportShare else 0.0) +
      (if Config.EnergyEnabled then in.sumGreenInvestment * Config.GreenImportShare else 0.0)
    val aggInventoryChange = if Config.InventoryEnabled then in.sumInventoryChange else 0.0
    val gdp = in.domesticCons + govGdpContribution + euGdpContribution + in.w.forex.exports.toDouble + domesticGFCF + aggInventoryChange

    val totalSystemLoans = in.w.bankingSector.map(_.banks.kahanSumBy(_.loans.toDouble)).getOrElse(in.w.bank.totalLoans.toDouble)
    val newMacropru = Macroprudential.step(in.w.macropru, totalSystemLoans, gdp)

    val sectorAdoption = SECTORS.indices.map { s =>
      val secFirms = living2.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else secFirms.count(f =>
        f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid]
      ).toDouble / secFirms.length
    }.toVector
    val baseSigmas = SECTORS.map(_.sigma).toVector
    val newSigmas = SigmaDynamics.evolve(
      in.w.currentSigmas, baseSigmas, sectorAdoption, Config.SigmaLambda, Config.SigmaCapMult)

    val rewiredFirms = DynamicNetwork.rewire(in.ioFirms, Config.RewireRho)

    val exDev = if in.rc.isEurozone then 0.0
               else (in.w.forex.exchangeRate / Config.BaseExRate) - 1.0
    val (newInfl, newPrice) = Sectors.updateInflation(
      in.w.inflation.toDouble, in.w.priceLevel, in.avgDemandMult, in.wageGrowth, exDev, autoR, hybR, in.rc)

    val firmProfits = living2.kahanSumBy { f =>
      val rev = FirmOps.capacity(f) * in.sectorMults(f.sector.toInt) * newPrice
      val labor = FirmOps.workers(f) * in.newWage * SECTORS(f.sector.toInt).wageMultiplier
      val other = Config.OtherCosts * newPrice
      val aiMaint = f.tech match
        case _: TechState.Automated => Config.AiOpex * (0.60 + 0.40 * newPrice)
        case _: TechState.Hybrid    => Config.HybridOpex * (0.60 + 0.40 * newPrice)
        case _                      => 0.0
      val interest = f.debt.toDouble * in.lendingRates(f.bankId.toInt) / 12.0
      val gross = rev - labor - other - aiMaint - interest
      val tax = Math.max(0.0, gross) * Config.CitRate
      Math.max(0.0, gross - tax)
    }

    val prevGdp = if in.w.gdpProxy > 0 then in.w.gdpProxy else 1.0
    val gdpGrowthForEquity = (gdp - prevGdp) / prevGdp
    val equityAfterIndex = EquityMarket.step(in.w.equity, in.w.nbp.referenceRate.toDouble, newInfl,
      gdpGrowthForEquity, firmProfits)
    val equityAfterIssuance = EquityMarket.processIssuance(in.sumEquityIssuance, equityAfterIndex)

    val (netDomesticDividends, foreignDividendOutflow, dividendTax) =
      if Config.GpwEnabled && Config.GpwDividends then
        EquityMarket.computeDividends(firmProfits, equityAfterIssuance.dividendYield.toDouble,
          equityAfterIssuance.marketCap.toDouble, equityAfterIssuance.foreignOwnership.toDouble)
      else (0.0, 0.0, 0.0)

    Output(autoR, hybR, aggInventoryStock, aggGreenCapital, euMonthly, euCofin,
      euProjectCapital, gdp, newMacropru, newSigmas, rewiredFirms, newInfl, newPrice,
      equityAfterIssuance, netDomesticDividends, foreignDividendOutflow, dividendTax,
      firmProfits, domesticGFCF, investmentImports, aggInventoryChange)
