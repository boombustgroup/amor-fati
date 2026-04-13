package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{OperationalSignals, World}
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FirmEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
  private val w    = init.world
  private val rng  = RandomStream.seeded(42)

  private val fiscal = FiscalConstraintEconomics.compute(w, init.banks, ExecutionMonth.First)
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

  private val rng2  = RandomStream.seeded(42)
  private val oldS5 = FirmEconomics.runStep(w, init.firms, init.households, init.banks, s1, s2, s3, s4, rng2)

  private val rng3   = RandomStream.seeded(42)
  private val result = FirmEconomics.compute(
    FirmEconomics.Input(
      w = w,
      firms = init.firms,
      households = init.households,
      banks = init.banks,
      month = s1.m,
      lendingBaseRate = s1.lendingBaseRate,
      resWage = s1.resWage,
      baseMinWage = s1.baseMinWage,
      minWagePriceLevel = s1.updatedMinWagePriceLevel,
      newWage = s2.newWage,
      employed = s2.employed,
      laborDemand = s2.laborDemand,
      wageGrowth = s2.wageGrowth,
      immigration = s2.newImmig,
      netMigration = s2.netMigration,
      demographics = s2.newDemographics,
      zusState = s2.newZus,
      nfzState = s2.newNfz,
      ppkState = s2.newPpk,
      rawPpkBondPurchase = s2.rawPpkBondPurchase,
      earmarked = s2.newEarmarked,
      living = s2.living,
      regionalWages = s2.regionalWages,
      hhOutput = s3,
      operationalSignals = OperationalSignals(
        sectorDemandMult = s4.sectorMults,
        sectorDemandPressure = s4.sectorDemandPressure,
        sectorHiringSignal = s4.sectorHiringSignal,
        operationalHiringSlack = s2.operationalHiringSlack,
      ),
      avgDemandMult = s4.avgDemandMult,
      sectorCapReal = s4.sectorCapReal,
      govPurchases = s4.govPurchases,
      laggedInvestDemand = s4.laggedInvestDemand,
      fiscalRuleStatus = s4.fiscalRuleStatus,
      rng = rng3,
    ),
  )

  private val resultR = FirmEconomics.toResult(result)
  private val ManufacturingSector = 1

  private def runStepFor(world: World, firms: Vector[Firm.State])(using SimParams): FirmEconomics.StepOutput =
    val fiscal = FiscalConstraintEconomics.compute(world, init.banks, ExecutionMonth.First)
    val s1     = FiscalConstraintEconomics.toOutput(fiscal)
    val labor  = LaborEconomics.compute(world, firms, init.households, s1)
    val s2     = LaborEconomics.Output(
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
    val s3     = HouseholdIncomeEconomics.compute(world, firms, init.households, init.banks, s1.lendingBaseRate, s1.resWage, s2.newWage, RandomStream.seeded(42))
    val s4     = DemandEconomics.compute(DemandEconomics.Input(world, s2.employed, s2.living, s3.domesticCons))
    FirmEconomics.runStep(world, firms, init.households, init.banks, s1, s2, s3, s4, RandomStream.seeded(43))

  private def manufacturingScenario(stateOwned: Boolean, cashRich: Boolean = false): Vector[Firm.State] =
    init.firms.map { firm =>
      val base =
        if firm.sector.toInt == ManufacturingSector && cashRich then
          firm.copy(cash = PLN(500e6), capitalStock = PLN.Zero, greenCapital = PLN.Zero)
        else firm
      if base.sector.toInt == ManufacturingSector then base.copy(stateOwned = stateOwned) else base.copy(stateOwned = false)
    }

  private def manufacturingOutputs(step: FirmEconomics.StepOutput): Vector[Firm.State] =
    step.ioFirms.filter(_.sector.toInt == ManufacturingSector)

  "FirmEconomics (self-contained Input)" should "match old step tax" in
    result.sumTax.shouldBe(oldS5.sumTax)

  it should "match old step loans and NPL" in {
    result.sumNewLoans.shouldBe(oldS5.sumNewLoans)
    result.nplLoss.shouldBe(oldS5.nplLoss)
    result.intIncome.shouldBe(oldS5.intIncome)
  }

  it should "produce flows that close at SFC == 0L" in {
    val flows = FirmFlows.emit(StateAdapter.firmInput(resultR, s3.totalIncome))
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
  }

  it should "increase manufacturing capital accumulation when strategic firms are state-owned" in {
    val privateRun          = runStepFor(w, manufacturingScenario(stateOwned = false, cashRich = true))
    val stateOwnedRun       = runStepFor(w, manufacturingScenario(stateOwned = true, cashRich = true))
    val privateManufactured = manufacturingOutputs(privateRun)
    val soeManufactured     = manufacturingOutputs(stateOwnedRun)

    privateManufactured should not be empty
    soeManufactured.zip(privateManufactured).foreach { case (soeFirm, privateFirm) =>
      soeFirm.capitalStock should be > privateFirm.capitalStock
    }
    stateOwnedRun.sumGrossInvestment should be > privateRun.sumGrossInvestment
  }

  it should "reduce manufacturing markup pass-through for state-owned firms under a commodity shock" in {
    val privateFirms          = manufacturingScenario(stateOwned = false)
    val stateOwnedFirms       = manufacturingScenario(stateOwned = true)
    val shockedWorld          = w.updateExternal(_.copy(gvc = w.external.gvc.copy(commodityPriceIndex = PriceIndex(1.80))))
    val baselinePrivateRun    = runStepFor(w, privateFirms)
    val baselineStateOwnedRun = runStepFor(w, stateOwnedFirms)
    val shockedPrivateRun     = runStepFor(shockedWorld, privateFirms)
    val shockedStateOwnedRun  = runStepFor(shockedWorld, stateOwnedFirms)
    val shockedPairs          = manufacturingOutputs(shockedStateOwnedRun).map(_.markup).zip(manufacturingOutputs(shockedPrivateRun).map(_.markup))

    manufacturingOutputs(baselineStateOwnedRun).map(_.markup) shouldBe manufacturingOutputs(baselinePrivateRun).map(_.markup)
    shockedStateOwnedRun.markupInflation should be < shockedPrivateRun.markupInflation
    shockedPairs.foreach { case (soeMarkup, privateMarkup) =>
      soeMarkup should be <= privateMarkup
    }
    shockedPairs.exists { case (soeMarkup, privateMarkup) => soeMarkup < privateMarkup } shouldBe true
  }
