package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BufferStockMpcSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val baseMpc = Share(0.82)
  private val income  = PLN(8000.0)

  private def mkHh(savings: PLN = PLN(48000.0), mpc: Share = baseMpc): Household.State =
    Household.State(
      HhId(0),
      savings,
      PLN.Zero,
      PLN(1800.0),
      Share(0.7),
      Share.Zero,
      mpc,
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
      Array.empty[HhId],
      BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(0),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share(0.5),
      wageScar = Share.Zero,
    )

  // target savings = 8000 * 6 = 48000

  "updateMpc" should "keep MPC near base when savings = target" in {
    val hh     = mkHh(savings = PLN(48000.0))
    val result = Household.updateMpc(hh, income, hh.status)
    result shouldBe baseMpc
  }

  it should "lower MPC when buffer is fat (savings >> target)" in {
    val hh     = mkHh(savings = PLN(120000.0))
    val result = Household.updateMpc(hh, income, hh.status)
    result should be < baseMpc
  }

  it should "raise MPC when buffer is depleted (savings << target)" in {
    val hh     = mkHh(savings = PLN(5000.0))
    val result = Household.updateMpc(hh, income, hh.status)
    result should be >= baseMpc
  }

  it should "boost MPC for unemployed" in {
    val employed   = mkHh()
    val unemployed = employed.copy(status = HhStatus.Unemployed(3))
    val mpcEmp     = Household.updateMpc(employed, income, employed.status)
    val mpcUnemp   = Household.updateMpc(unemployed, income, unemployed.status)
    mpcUnemp should be > mpcEmp
  }

  it should "clamp MPC to floor" in {
    val hh     = mkHh(savings = PLN(1_000_000.0))
    val result = Household.updateMpc(hh, income, hh.status)
    result should be >= Share(0.50)
  }

  it should "clamp MPC to ceiling" in {
    val hh     = mkHh(savings = PLN.Zero)
    val result = Household.updateMpc(hh, income, HhStatus.Unemployed(12))
    result should be <= Share(0.98)
  }

  it should "return base MPC when income is zero" in {
    val hh     = mkHh()
    val result = Household.updateMpc(hh, PLN.Zero, hh.status)
    result shouldBe baseMpc
  }

  it should "be monotonically decreasing in savings" in {
    val employed      = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0))
    val savingsLevels = Seq(1000.0, 10000.0, 48000.0, 100000.0, 200000.0)
    val mpcs          = savingsLevels.map(s => Household.updateMpc(mkHh(savings = PLN(s)), income, employed))
    for i <- 0 until mpcs.length - 1 do
      withClue(s"savings=${savingsLevels(i)} vs ${savingsLevels(i + 1)}: ") {
        mpcs(i) should be >= mpcs(i + 1)
      }
  }

  "computeSavingsDrawdown" should "draw only excess savings for employed households" in {
    val hh       = mkHh(savings = PLN(120000.0))
    val drawdown = Household.computeSavingsDrawdown(hh, income, hh.status)

    drawdown shouldBe PLN(14400.0)
  }

  it should "allow stressed households to draw below the full target buffer but above the protected floor" in {
    val hh       = mkHh(savings = PLN(30000.0)).copy(status = HhStatus.Unemployed(4))
    val drawdown = Household.computeSavingsDrawdown(hh, income, hh.status)

    drawdown shouldBe PLN(2100.0)
  }

  it should "return zero when savings are already below the protected floor under stress" in {
    val hh       = mkHh(savings = PLN(10000.0)).copy(status = HhStatus.Unemployed(4))
    val drawdown = Household.computeSavingsDrawdown(hh, income, hh.status)

    drawdown shouldBe PLN.Zero
  }
