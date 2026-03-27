package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.*
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

  private val fiscal = FiscalConstraintEconomics.compute(w)
  private val s1     = FiscalConstraintEconomics.toOutput(fiscal)
  private val labor  = LaborEconomics.compute(w, init.firms, init.households, s1)
  private val s2     = LaborEconomics.Output(
    labor.wage,
    labor.employed,
    labor.laborDemand,
    labor.wageGrowth,
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
  private val s3     = HouseholdIncomeEconomics.compute(w, init.firms, init.households, s1.lendingBaseRate, s1.resWage, s2.newWage, rng)
  private val s4     = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))

  private val rng2  = new scala.util.Random(42)
  private val oldS5 = FirmEconomics.runStep(w, init.firms, init.households, s1, s2, s3, s4, rng2)

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
      rawSectorMults = s4.rawSectorMults,
      avgDemandMult = s4.avgDemandMult,
      sectorCap = s4.sectorCap,
      govPurchases = s4.govPurchases,
      laggedInvestDemand = s4.laggedInvestDemand,
      fiscalRuleStatus = s4.fiscalRuleStatus,
      rng = rng3,
    ),
  )

  private val resultR = FirmEconomics.toResult(result)

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
