package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.WorldInit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorldAssemblyEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "WorldAssemblyEconomics (own Input)" should "produce valid world after simulation step" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val w      = result.newWorld

    w.month.shouldBe(1)
    w.derivedTotalPopulation should be > 0
    w.hhAgg.employed should be > 0
    w.external.tourismSeasonalFactor should not be 0.0
  }

  it should "preserve public-spending semantic aggregates on the assembled world" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val w      = result.newWorld

    w.gov.domesticBudgetDemand shouldBe (w.gov.govCurrentSpend + w.gov.govCapitalSpend)
    w.gov.domesticBudgetOutlays shouldBe (
      w.gov.unempBenefitSpend
        + w.gov.socialTransferSpend
        + w.gov.govCurrentSpend
        + w.gov.govCapitalSpend
        + w.gov.debtServiceSpend
        + w.gov.euCofinancing
    )

    w.gov.domesticBudgetDemand shouldBe result.calculus.govPurchases
    w.gov.domesticBudgetOutlays should be >= w.gov.domesticBudgetDemand
  }
