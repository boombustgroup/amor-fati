package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class BankingSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val configs = Banking.DefaultConfigs

  @annotation.nowarn("msg=unused private member") // defaults used by callers
  private def mkBankRow(
      id: Int = 0,
      deposits: PLN = PLN(1000000),
      loans: PLN = PLN(1000000),
      capital: PLN = PLN(200000),
      nplAmount: PLN = PLN.Zero,
      govBondHoldings: PLN = PLN.Zero,
      reservesAtNbp: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
  ): (Banking.BankState, Banking.BankFinancialStocks) = (
    Banking.BankState(
      id = BankId(id),
      capital = capital,
      nplAmount = nplAmount,
      htmBookYield = Rate.decimal(55, 3),
      status = status,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerNpl = PLN.Zero,
    ),
    Banking.BankFinancialStocks(
      totalDeposits = deposits,
      firmLoan = loans,
      govBondAfs = govBondHoldings * Share.decimal(40, 2),
      govBondHtm = govBondHoldings * Share.decimal(60, 2),
      reserve = reservesAtNbp,
      interbankLoan = interbankNet,
      demandDeposit = PLN.Zero,
      termDeposit = PLN.Zero,
      consumerLoan = PLN.Zero,
    ),
  )

  // ---- Interbank netting invariant ----

  "clearInterbank" should "always produce interbankNet that sums to zero" in
    forAll(genBanking.Sector) { bs =>
      val cleared = Banking.clearInterbank(bs.banks, bs.financialStocks, bs.configs)
      val netSum  = cleared.financialStocks.map(stocks => decimal(stocks.interbankLoan)).sum
      netSum shouldBe BigDecimal("0.0") +- BigDecimal("1.0") // tolerance for floating-point
    }

  // ---- bond allocation increments sum exactly ----

  "allocateBondIssuance/allocateBondRedemption" should "have individual deltas summing to exactly the requested change (residual)" in
    forAll(genBanking.Sector, genDecimal("-1e8", "1e8")) { (bs, signedChange: BigDecimal) =>
      val aliveDep = bs.banks.zip(bs.financialStocks).filterNot(_._1.failed).map((_, stocks) => decimal(stocks.totalDeposits)).sum
      whenever(aliveDep > 0 && signedChange != BigDecimal("0.0")) {
        val after  =
          if signedChange > BigDecimal("0.0") then Banking.allocateBondIssuance(bs.banks, bs.financialStocks, plnBD(signedChange), Rate.decimal(5, 2))
          else Banking.allocateBondRedemption(bs.banks, bs.financialStocks, plnBD(-signedChange), Rate.decimal(5, 2))
        // Per-bank deltas sum to the signed issuance/redemption request.
        val deltas =
          after.financialStocks.zip(bs.financialStocks).map((a, b) => decimal(Banking.govBondHoldings(a)) - decimal(Banking.govBondHoldings(b)))
        deltas.sum shouldBe signedChange +- BigDecimal("1.0")
      }
    }

  it should "keep aggregate bond change within tight tolerance for large issuance" in
    forAll(genBanking.Sector, genDecimal("1e10", "1e14")) { (bs, issuance: BigDecimal) =>
      val alive = bs.banks.filterNot(_.failed)
      whenever(alive.nonEmpty) {
        val before   = bs.financialStocks.map(stocks => decimal(Banking.govBondHoldings(stocks))).sum
        val after    = Banking.allocateBondIssuance(bs.banks, bs.financialStocks, plnBD(issuance), Rate.decimal(5, 2))
        val afterSum = after.financialStocks.map(stocks => decimal(Banking.govBondHoldings(stocks))).sum
        (afterSum - before) shouldBe issuance +- BigDecimal("1.0") // well within SFC tolerance
      }
    }

  // ---- lendingRate monotonic in NPL ----

  "lendingRate" should "be monotonically non-decreasing with NPL ratio" in
    forAll(genRate, genDecimal("0.0", "0.15"), genDecimal("0.0", "0.15")) { (refRate: BigDecimal, npl1Frac: BigDecimal, npl2Frac: BigDecimal) =>
      val loans              = BigDecimal("1e6")
      val (lo, hi)           = if npl1Frac <= npl2Frac then (npl1Frac, npl2Frac) else (npl2Frac, npl1Frac)
      val (bankLo, stocksLo) = mkBankRow(nplAmount = plnBD(loans * lo))
      val (bankHi, stocksHi) = mkBankRow(nplAmount = plnBD(loans * hi))
      val rateLo             = Banking.lendingRate(bankLo, stocksLo, configs(0), rateBD(refRate), Rate.Zero, PLN.Zero)
      val rateHi             = Banking.lendingRate(bankHi, stocksHi, configs(0), rateBD(refRate), Rate.Zero, PLN.Zero)
      rateHi should be >= rateLo
    }

  // ---- assignBank returns valid index ----

  "assignBank" should "always return valid index in [0, nBanks)" in
    forAll(Gen.choose(0, 5)) { (sector: Int) =>
      val rng = RandomStream.seeded(42L + sector.toLong)
      for _ <- 0 until 50 do
        val bId = Banking.assignBank(SectorIdx(sector), configs, rng)
        bId.toInt should be >= 0
        bId.toInt should be < configs.length
    }

  // ---- aggregate.deposits == sum of individual deposits ----

  "aggregate" should "have deposits equal to sum of individual deposits" in
    forAll(genBanking.Sector) { bs =>
      val agg         = Banking.aggregateFromBankStocks(bs.banks, bs.financialStocks)
      val expectedDep = bs.financialStocks.map(stocks => decimal(stocks.totalDeposits)).sum
      decimal(agg.deposits) shouldBe expectedDep +- BigDecimal("1.0")
    }

  it should "have capital equal to sum of individual capital" in
    forAll(genBanking.Sector) { bs =>
      val agg         = Banking.aggregateFromBankStocks(bs.banks, bs.financialStocks)
      val expectedCap = bs.banks.map(b => decimal(b.capital)).sum
      decimal(agg.capital) shouldBe expectedCap +- BigDecimal("1.0")
    }

  // ---- Failed banks stay failed ----

  "checkFailures" should "never un-fail a bank" in
    forAll(genBanking.Sector) { bs =>
      val failedBefore = bs.banks.filter(_.failed).map(_.id).toSet
      val afterCheck   = Banking.checkFailures(bs.banks, bs.financialStocks, ExecutionMonth(50), enabled = true, Multiplier.Zero)
      val failedAfter  = afterCheck.banks.filter(_.failed).map(_.id).toSet
      failedBefore.subsetOf(failedAfter) shouldBe true
    }

  // ---- Reserves non-negative after clearing ----

  "clearInterbank" should "keep reserves non-negative" in
    forAll(genBanking.Sector) { bs =>
      val cleared = Banking.clearInterbank(bs.banks, bs.financialStocks, bs.configs)
      cleared.financialStocks.foreach(_.reserve should be >= PLN.Zero)
    }

  // ---- Initialize preserves totals ----

  "initialize" should "preserve total deposits and capital" in
    forAll(genDecimal("1e5", "1e10"), genDecimal("1e4", "1e9")) { (totalDep: BigDecimal, totalCap: BigDecimal) =>
      val bs = testBankingSector(totalDeposits = plnBD(totalDep), totalCapital = plnBD(totalCap), totalLoans = PLN.Zero, configs = configs)
      bs.financialStocks.map(stocks => decimal(stocks.totalDeposits)).sum shouldBe totalDep +- BigDecimal("1.0")
      bs.banks.map(b => decimal(b.capital)).sum shouldBe totalCap +- BigDecimal("1.0")
    }

  // ---- QE purchases don't exceed bond holdings ----

  "sellToBuyer" should "not make any bank's bond holdings negative" in
    forAll(genBanking.Sector, genDecimal("0.0", "1e9")) { (bs, qe: BigDecimal) =>
      val result = Banking.sellToBuyer(bs.banks, bs.financialStocks, plnBD(qe)).financialStocks
      result.foreach(stocks => Banking.govBondHoldings(stocks) should be >= PLN.Zero)
    }
