package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmoothLaborAdjustmentSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val wage = PLN(8000.0)

  private def mkFirm(workers: Int, cash: PLN): Firm.State =
    Firm.State(
      FirmId(0),
      cash,
      PLN.Zero,
      TechState.Traditional(workers),
      Share(0.5),
      1.0,
      Share(0.5),
      SectorIdx(2),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = workers,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  private def mkPnl(revenue: PLN, costs: PLN): Firm.PnL =
    Firm.PnL(revenue, costs, PLN.Zero, revenue - costs, PLN.Zero, PLN.Zero, PLN.Zero)

  private def mkStateOwnedFirm(workers: Int, cash: PLN): Firm.State =
    mkFirm(workers, cash).copy(stateOwned = true)

  "attemptDownsize" should "cut fewer workers than old 30% panic (smooth λ=0.15)" in {
    val firm   = mkFirm(workers = 100, cash = PLN(500000.0))
    val pnl    = mkPnl(revenue = PLN(100000.0), costs = PLN(120000.0))
    val nc     = firm.cash + pnl.netAfterTax
    val result = Firm.attemptDownsize(firm, pnl, nc, 100, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result match
      case Firm.Decision.Downsize(_, newWorkers, _, _, _) =>
        val cut = 100 - newWorkers
        cut should be < 30 // old was 30% = 30 workers
        cut should be > 0
      case other                                          => fail(s"Expected Downsize, got $other")
  }

  it should "never go below minWorkersRetained" in {
    val firm   = mkFirm(workers = 5, cash = PLN(100000.0))
    val pnl    = mkPnl(revenue = PLN(1000.0), costs = PLN(50000.0))
    val nc     = firm.cash + pnl.netAfterTax
    val result = Firm.attemptDownsize(firm, pnl, nc, 5, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result match
      case Firm.Decision.Downsize(_, newWorkers, _, _, _) =>
        newWorkers should be >= summon[SimParams].firm.minWorkersRetained
      case _: Firm.Decision.GoBankrupt                    => succeed // also acceptable if severance too high
      case other                                          => fail(s"Expected Downsize or GoBankrupt, got $other")
  }

  it should "allow a small temporary cash shortfall at minWorkersRetained when profit stays positive" in {
    val min    = summon[SimParams].firm.minWorkersRetained
    val pnl    = mkPnl(revenue = PLN(5000.0), costs = PLN(1200.0))
    val firm   = mkFirm(workers = min, cash = -pnl.netAfterTax + PLN(-100.0))
    val nc     = firm.cash + pnl.netAfterTax
    val result = Firm.attemptDownsize(firm, pnl, nc, min, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result shouldBe a[Firm.Decision.Survive]
  }

  it should "still go bankrupt at minWorkersRetained when the liquidity gap is too large" in {
    val min    = summon[SimParams].firm.minWorkersRetained
    val firm   = mkFirm(workers = min, cash = PLN(-10000.0))
    val pnl    = mkPnl(revenue = PLN(5000.0), costs = PLN(1200.0))
    val nc     = firm.cash + pnl.netAfterTax
    val result = Firm.attemptDownsize(firm, pnl, nc, min, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result shouldBe a[Firm.Decision.GoBankrupt]
  }

  it should "go bankrupt when severance cost exceeds cash" in {
    val firm   = mkFirm(workers = 50, cash = PLN(100.0))
    val pnl    = mkPnl(revenue = PLN(10000.0), costs = PLN(100000.0))
    val nc     = firm.cash + pnl.netAfterTax // very negative
    val result = Firm.attemptDownsize(firm, pnl, nc, 50, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result shouldBe a[Firm.Decision.GoBankrupt]
  }

  it should "account for severance in adjusted cash" in {
    val firm   = mkFirm(workers = 100, cash = PLN(500000.0))
    val pnl    = mkPnl(revenue = PLN(100000.0), costs = PLN(110000.0))
    val nc     = firm.cash + pnl.netAfterTax
    val result = Firm.attemptDownsize(firm, pnl, nc, 100, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result match
      case Firm.Decision.Downsize(_, _, adjustedCash, _, _) =>
        // Adjusted cash should be less than nc + laborSaved (severance eats into it)
        adjustedCash should be < nc
      case other                                            => fail(s"Expected Downsize, got $other")
  }

  it should "retain more workers when non-labor costs are higher" in {
    // Higher fixed overhead requires more contribution margin from retained workers,
    // so the break-even target headcount should not fall.
    val firm       = mkFirm(workers = 100, cash = PLN(500000.0))
    val pnlLow     = mkPnl(PLN(200000.0), PLN(210000.0)) // low non-labor overhead
    val pnlHigh    = mkPnl(PLN(200000.0), PLN(280000.0)) // high non-labor overhead
    val resultLow  = Firm.attemptDownsize(
      firm,
      pnlLow,
      firm.cash + pnlLow.netAfterTax,
      100,
      TechState.Traditional(_),
      wage,
      BankruptReason.LaborCostInsolvency,
    )
    val resultHigh = Firm.attemptDownsize(
      firm,
      pnlHigh,
      firm.cash + pnlHigh.netAfterTax,
      100,
      TechState.Traditional(_),
      wage,
      BankruptReason.LaborCostInsolvency,
    )
    (resultLow, resultHigh) match
      case (Firm.Decision.Downsize(_, w1, _, _, _), Firm.Decision.Downsize(_, w2, _, _, _)) =>
        w2 should be >= w1
      case _                                                                                => succeed // one might bankrupt, that's fine
  }

  it should "not force immediate labor-cost bankruptcy for state-owned firms" in {
    val firm   = mkStateOwnedFirm(workers = 50, cash = PLN(100.0))
    val pnl    = mkPnl(revenue = PLN(10000.0), costs = PLN(100000.0))
    val nc     = firm.cash + pnl.netAfterTax
    val result = Firm.attemptDownsize(firm, pnl, nc, 50, TechState.Traditional(_), wage, BankruptReason.LaborCostInsolvency)
    result should not be a[Firm.Decision.GoBankrupt]
  }
