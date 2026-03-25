package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FirmEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val init = WorldInit.initialize(42L)
  private val w    = init.world
  private val rng  = new scala.util.Random(42)

  private val s1 = FiscalConstraintStep.run(FiscalConstraintStep.Input(w))
  private val s2 = LaborDemographicsStep.run(LaborDemographicsStep.Input(w, init.firms, init.households, s1))
  private val s3 = HouseholdIncomeStep.run(HouseholdIncomeStep.Input(w, init.firms, init.households, s1, s2), rng)
  private val s4 = DemandStep.run(DemandStep.Input(w, s2, s3))

  private val rng2  = new scala.util.Random(42)
  private val oldS5 = FirmProcessingStep.run(FirmProcessingStep.Input(w, init.firms, init.households, s1, s2, s3, s4), rng2)

  private val rng3   = new scala.util.Random(42)
  private val result = FirmEconomics.compute(
    FirmEconomics.Input(
      w = w,
      firms = init.firms,
      households = init.households,
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
      sectorMults = s4.sectorMults,
      avgDemandMult = s4.avgDemandMult,
      sectorCap = s4.sectorCap,
      govPurchases = s4.govPurchases,
      laggedInvestDemand = s4.laggedInvestDemand,
      fiscalRuleStatus = s4.fiscalRuleStatus,
      rng = rng3,
    ),
  )

  "FirmEconomics (self-contained Input)" should "match old step tax" in
    result.tax.shouldBe(oldS5.sumTax)

  it should "match old step loans and NPL" in {
    result.newLoans.shouldBe(oldS5.sumNewLoans)
    result.nplLoss.shouldBe(oldS5.nplLoss)
    result.intIncome.shouldBe(oldS5.intIncome)
  }

  it should "produce flows that close at SFC == 0L" in {
    val flows = FirmFlows.emit(StateAdapter.firmInput(result, s3.totalIncome))
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
  }
