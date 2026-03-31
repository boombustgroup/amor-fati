package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PriceEquityEconomicsSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults
  private val td          = ComputationBoundary

  "PriceEquityEconomics.governmentDemandContribution" should "scale with constrained runtime government purchases" in {
    val low  = PriceEquityEconomics.governmentDemandContribution(PLN(250e6))
    val high = PriceEquityEconomics.governmentDemandContribution(PLN(600e6))

    high should be > low
    high / low shouldBe (600e6 / 250e6) +- 1e-9
  }

  it should "respect the configured current-vs-capital multipliers" in {
    val gp           = PLN(500e6)
    val contribution = PriceEquityEconomics.governmentDemandContribution(gp)
    val expected     =
      td.toDouble(gp) * (1.0 - td.toDouble(summon[SimParams].fiscal.govInvestShare)) * td.toDouble(summon[SimParams].fiscal.govCurrentMultiplier) +
        td.toDouble(gp) * td.toDouble(summon[SimParams].fiscal.govInvestShare) * td.toDouble(summon[SimParams].fiscal.govCapitalMultiplier)

    contribution shouldBe expected +- 1e-6
  }
