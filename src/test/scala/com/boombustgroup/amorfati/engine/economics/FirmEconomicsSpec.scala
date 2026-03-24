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

  "FirmEconomics.compute" should "match old FirmProcessingStep aggregate flows" in {
    val rng2   = new scala.util.Random(42)
    val oldS5  = FirmProcessingStep.run(FirmProcessingStep.Input(w, init.firms, init.households, s1, s2, s3, s4), rng2)
    val rng3   = new scala.util.Random(42)
    val result = FirmEconomics.compute(w, init.firms, init.households, s1, s2, s3, s4, rng3)

    result.tax shouldBe oldS5.sumTax
    result.newLoans shouldBe oldS5.sumNewLoans
    result.nplLoss shouldBe oldS5.nplLoss
    result.intIncome shouldBe oldS5.intIncome
    result.grossInvestment shouldBe oldS5.sumGrossInvestment
    result.ioPayments shouldBe oldS5.totalIoPaid
  }

  it should "produce flows that close at SFC == 0L" in {
    val rng2   = new scala.util.Random(42)
    val result = FirmEconomics.compute(w, init.firms, init.households, s1, s2, s3, s4, rng2)
    val flows  = FirmFlows.emit(StateAdapter.firmInput(result, s3.totalIncome))

    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }
