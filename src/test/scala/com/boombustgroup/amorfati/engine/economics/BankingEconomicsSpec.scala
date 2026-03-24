package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BankingEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "BankingEconomics" should "produce flows that close at SFC == 0L" in {
    val init = WorldInit.initialize(42L)
    val w    = init.world
    val rng  = new scala.util.Random(42)

    val s1  = FiscalConstraintStep.run(FiscalConstraintStep.Input(w))
    val s2  = LaborDemographicsStep.run(LaborDemographicsStep.Input(w, init.firms, init.households, s1))
    val s3  = HouseholdIncomeStep.run(HouseholdIncomeStep.Input(w, init.firms, init.households, s1, s2), rng)
    val s4  = DemandStep.run(DemandStep.Input(w, s2, s3))
    val s5  = FirmProcessingStep.run(FirmProcessingStep.Input(w, init.firms, init.households, s1, s2, s3, s4), rng)
    val s6  = HouseholdFinancialStep.run(HouseholdFinancialStep.Input(w, s1, s2, s3))
    val s7  = PriceEquityStep.run(PriceEquityStep.Input(w, s1, s2, s3, s4, s5), rng)
    val s8  = OpenEconomyStep.run(OpenEconomyStep.Input(w, s1, s2, s3, s4, s5, s6, s7, rng))
    val res = BankingEconomics.compute(w, init.firms, init.households, s1, s2, s3, s4, s5, s6, s7, s8, rng)

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

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }
