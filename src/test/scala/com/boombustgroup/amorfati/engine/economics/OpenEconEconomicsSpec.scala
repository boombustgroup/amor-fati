package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests OpenEconEconomics produces self-consistent results. */
class OpenEconEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults
  private val TestSeed       = 42L

  private val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(TestSeed))
  private val w    = init.world
  private val rng  = RandomStream.seeded(TestSeed)

  // Run pipeline through Economics objects
  private val fiscal = FiscalConstraintEconomics.compute(w, init.banks)
  private val s1     = FiscalConstraintEconomics.toOutput(fiscal)
  private val labor  = LaborEconomics.compute(w, init.firms, init.households, s1)
  private val s2     = LaborEconomics.Output(
    labor.wage,
    labor.employed,
    labor.laborDemand,
    labor.wageGrowth,
    labor.operationalHiringSlack,
    labor.immigration,
    labor.netMigration,
    labor.demographics,
    SocialSecurity.ZusState.zero,
    SocialSecurity.NfzState.zero,
    SocialSecurity.PpkState.zero,
    PLN.Zero,
    EarmarkedFunds.State.zero,
    labor.living,
    labor.regionalWages,
  )
  private val s3     = HouseholdIncomeEconomics.compute(w, init.firms, init.households, init.banks, s1.lendingBaseRate, s1.resWage, s2.newWage, rng)
  private val s4     = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))
  private val s5     = FirmEconomics.runStep(w, init.firms, init.households, init.banks, s1, s2, s3, s4, rng)
  private val s6     = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, rng)
  private val s7     = PriceEquityEconomics.compute(
    PriceEquityEconomics.Input(
      w,
      s1.m,
      s2.newWage,
      s2.employed,
      s2.wageGrowth,
      s3.domesticCons,
      s4.govPurchases,
      s4.avgDemandMult,
      s4.sectorMults,
      init.banks,
      s5,
    ),
    rng,
  )

  private val newResult = OpenEconEconomics.compute(
    OpenEconEconomics.Input(
      w = w,
      banks = init.banks,
      employed = s2.employed,
      newWage = s2.newWage,
      domesticConsumption = s3.domesticCons,
      importConsumption = s3.importCons,
      totalTechAndInvImports = s5.sumTechImp,
      gdp = s7.gdp,
      newInflation = s7.newInfl,
      autoRatio = s7.autoR,
      govPurchases = s4.govPurchases,
      sectorMults = s4.sectorMults,
      livingFirms = s5.ioFirms,
      totalBondDefault = s5.totalBondDefault,
      actualBondIssuance = s5.actualBondIssuance,
      corpBondAbsorption = s5.corpBondAbsorption,
      euMonthly = s7.euMonthly,
      remittanceOutflow = s6.remittanceOutflow,
      diasporaInflow = s6.diasporaInflow,
      tourismExport = s6.tourismExport,
      tourismImport = s6.tourismImport,
      equityReturn = w.financial.equity.monthlyReturn,
      investmentImports = s7.investmentImports,
      profitShifting = s5.sumProfitShifting,
      fdiRepatriation = s5.sumFdiRepatriation,
      foreignDividendOutflow = s7.foreignDividendOutflow,
      month = s1.m,
      commodityRng = RandomStream.seeded(TestSeed),
    ),
  )

  "OpenEconEconomics (self-contained)" should "produce a valid reference rate" in {
    ComputationBoundary.toDouble(newResult.newRefRate) should be >= 0.0
  }

  it should "produce a valid bond yield" in {
    ComputationBoundary.toDouble(newResult.newBondYield) should be >= 0.0
  }

  it should "produce non-negative interbank flows" in {
    newResult.reserveInterest should be >= PLN.Zero
  }

  it should "produce flows that close at SFC == 0L" in {
    val flows = OpenEconFlows.emit(
      OpenEconFlows.Input(
        newResult.exports,
        newResult.totalImports,
        s6.tourismExport,
        s6.tourismImport,
        newResult.fdi,
        newResult.portfolioFlows,
        newResult.primaryIncome,
        newResult.euFunds,
        s6.diasporaInflow,
        PLN.Zero,
      ),
    )
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }
