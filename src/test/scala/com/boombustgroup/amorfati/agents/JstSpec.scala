package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

/** JST (local government) unit tests. */
class JstSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  private given SimParams = SimParams.defaults

  "Jst.State.zero" should "have all zero fields" in {
    val z = Jst.State.zero
    z.debt shouldBe PLN.Zero
    z.revenue shouldBe PLN.Zero
    z.spending shouldBe PLN.Zero
    z.deficit shouldBe PLN.Zero
  }

  "Jst.step" should "compute revenue components and fiscal identities" in {
    val openingDeposits   = PLN(100000000)
    val centralCitRevenue = PLN(5000000)
    val totalWageIncome   = PLN(50000000)
    val gdp               = PLN(100000000)
    val nFirms            = 9000
    val pitRevenue        = PLN(3000000)
    val expectedPitShare  = PLN(1153800)
    val expectedCitShare  = PLN(335500)
    val expectedProperty  = PLN(3750000)
    val expectedSubv      = PLN(250000)
    val expectedDotacje   = PLN.decimal(833333333L, 4)
    val expectedRevenue   = PLN.decimal(55726333333L, 4)
    val expectedSpending  = PLN(5684086)
    val expectedDeficit   = PLN.decimal(1114526667L, 4)

    val result = Jst.step(
      Jst.State.zero,
      openingDeposits,
      centralCitRevenue,
      totalWageIncome,
      gdp,
      nFirms,
      pitRevenue,
    )

    Seq(expectedPitShare, expectedCitShare, expectedProperty, expectedSubv, expectedDotacje).sumPln shouldBe expectedRevenue
    result.state.revenue shouldBe expectedRevenue
    result.state.spending shouldBe expectedSpending
    result.state.deficit shouldBe expectedDeficit
    result.depositChange shouldBe -expectedDeficit
    result.closingDeposits shouldBe PLN.decimal(998885473333L, 4)
    result.state.debt shouldBe expectedDeficit

    result.state.deficit shouldBe result.state.spending - result.state.revenue
    result.depositChange shouldBe result.state.revenue - result.state.spending
    result.closingDeposits shouldBe openingDeposits + result.depositChange
  }

  it should "use fallback PIT rate when PIT revenue is unavailable" in {
    val openingDeposits   = PLN(100000000)
    val centralCitRevenue = PLN(5000000)
    val totalWageIncome   = PLN(50000000)
    val gdp               = PLN(100000000)
    val nFirms            = 9000
    val pitRevenue        = PLN.Zero
    val expectedPitShare  = PLN(2310000)
    val expectedCitShare  = PLN(335500)
    val expectedProperty  = PLN(3750000)
    val expectedSubv      = PLN(250000)
    val expectedDotacje   = PLN.decimal(833333333L, 4)
    val expectedRevenue   = PLN.decimal(67288333333L, 4)
    val expectedSpending  = PLN(6863410)
    val expectedDeficit   = PLN.decimal(1345766667L, 4)

    val result = Jst.step(
      Jst.State.zero,
      openingDeposits,
      centralCitRevenue,
      totalWageIncome,
      gdp,
      nFirms,
      pitRevenue,
    )

    Seq(expectedPitShare, expectedCitShare, expectedProperty, expectedSubv, expectedDotacje).sumPln shouldBe expectedRevenue
    result.state.revenue shouldBe expectedRevenue
    result.state.spending shouldBe expectedSpending
    result.state.deficit shouldBe expectedDeficit
    result.depositChange shouldBe -expectedDeficit
    result.closingDeposits shouldBe PLN.decimal(998654233333L, 4)
    result.state.debt shouldBe expectedDeficit

    result.state.deficit shouldBe result.state.spending - result.state.revenue
    result.depositChange shouldBe result.state.revenue - result.state.spending
    result.closingDeposits shouldBe openingDeposits + result.depositChange
  }
