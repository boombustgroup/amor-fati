package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

/** JST (local government) unit tests. */
class JstSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  private given SimParams = SimParams.defaults
  private val p           = summon[SimParams]

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

    val result = Jst.step(
      Jst.State.zero,
      openingDeposits,
      centralCitRevenue,
      totalWageIncome,
      gdp,
      nFirms,
      pitRevenue,
    )

    val expectedRevenue =
      pitRevenue * p.fiscal.jstPitShare +
        centralCitRevenue * p.fiscal.jstCitShare +
        nFirms * p.fiscal.jstPropertyTax / 12L +
        gdp * p.fiscal.jstSubventionShare / 12L +
        gdp * p.fiscal.jstDotacjeShare / 12L

    result.state.revenue shouldBe expectedRevenue
    result.state.spending shouldBe expectedRevenue * p.fiscal.jstSpendingMult
    result.state.deficit shouldBe result.state.spending - result.state.revenue
    result.depositChange shouldBe result.state.revenue - result.state.spending
    result.closingDeposits shouldBe openingDeposits + result.depositChange
    result.state.debt shouldBe result.state.deficit
  }
