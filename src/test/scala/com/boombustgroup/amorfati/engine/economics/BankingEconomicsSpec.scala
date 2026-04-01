package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BankingEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "BankingEconomics (own Input)" should "produce flows that close at SFC == 0L" in {
    val init = WorldInit.initialize(42L)
    val w    = init.world
    val rng  = new scala.util.Random(42)

    val fiscal = FiscalConstraintEconomics.compute(w, init.banks)
    val s1     = FiscalConstraintEconomics.toOutput(fiscal)
    val labor  = LaborEconomics.compute(w, init.firms, init.households, s1)
    val s2     = LaborEconomics.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
      labor.aggregateHiringSlack,
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
    val s3     = HouseholdIncomeEconomics.compute(w, init.firms, init.households, init.banks, s1.lendingBaseRate, s1.resWage, s2.newWage, rng)
    val s4     = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))
    val s5     = FirmEconomics.runStep(w, init.firms, init.households, init.banks, s1, s2, s3, s4, rng)
    val s6     = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, rng)
    val s7     = PriceEquityEconomics.compute(
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
    val s8     = OpenEconEconomics.runStep(OpenEconEconomics.StepInput(w, s1, s2, s3, s4, s5, s6, s7, init.banks, rng))

    val res = BankingEconomics.compute(
      BankingEconomics.Input(
        w = w,
        month = s1.m,
        lendingBaseRate = s1.lendingBaseRate,
        resWage = s1.resWage,
        baseMinWage = s1.baseMinWage,
        minWagePriceLevel = s1.updatedMinWagePriceLevel,
        employed = s2.employed,
        newWage = s2.newWage,
        laborDemand = s2.laborDemand,
        wageGrowth = s2.wageGrowth,
        govPurchases = s4.govPurchases,
        sectorMults = s4.sectorMults,
        avgDemandMult = s4.avgDemandMult,
        sectorCap = s4.sectorCap,
        laggedInvestDemand = s4.laggedInvestDemand,
        fiscalRuleStatus = s4.fiscalRuleStatus,
        laborOutput = s2,
        hhOutput = s3,
        firmOutput = s5,
        hhFinancialOutput = s6,
        priceEquityOutput = s7,
        openEconOutput = s8,
        banks = init.banks,
        depositRng = rng,
      ),
    )

    val flows = BankingFlows.emit(
      BankingFlows.Input(
        res.govBondIncome,
        res.reserveInterest,
        res.standingFacilityIncome,
        res.interbankInterest,
        res.bfgLevy,
        res.unrealizedBondLoss,
        res.bailInLoss,
        res.nbpRemittance,
      ),
    )

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
  }
