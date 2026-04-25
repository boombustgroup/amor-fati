package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.types.*

/** Monetary plumbing property-based tests. */
class MonetaryPlumbingPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private def mkBankRow(
      id: Int = 0,
      deposits: PLN = PLN("1e9"),
      loans: PLN = PLN("5e8"),
      capital: PLN = PLN("1e8"),
      reservesAtNbp: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
  ): (Banking.BankState, Banking.BankFinancialStocks) = (
    Banking.BankState(
      id = BankId(id),
      capital = capital,
      nplAmount = PLN.Zero,
      htmBookYield = Rate.Zero,
      status = status,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerNpl = PLN.Zero,
    ),
    Banking.BankFinancialStocks(
      totalDeposits = deposits,
      firmLoan = loans,
      govBondAfs = PLN.Zero,
      govBondHtm = PLN.Zero,
      reserve = reservesAtNbp,
      interbankLoan = interbankNet,
      demandDeposit = PLN.Zero,
      termDeposit = PLN.Zero,
      consumerLoan = PLN.Zero,
    ),
  )

  // =========================================================================
  // Reserve Interest Properties
  // =========================================================================

  "reserveInterest" should "be non-negative for alive banks with non-negative reserves" in
    forAll(genBanking.BankRow, genRate) { case ((bank, stocks), rate) =>
      whenever(!bank.failed && stocks.reserve >= PLN.Zero && rate >= 0) {
        Banking.reserveInterest(bank, stocks, Rate(rate)) should be >= PLN.Zero
      }
    }

  it should "scale linearly with reserves" in
    forAll(genRate, genDecimal("1e4", "1e9"), genDecimal("1.1", "5.0")) { (rate, reserves, mult) =>
      whenever(rate > BigDecimal("0.001")) {
        val (b1, s1) = mkBankRow(reservesAtNbp = PLN(reserves))
        val s2       = s1.copy(reserve = PLN(reserves * mult))
        val r1       = Banking.reserveInterest(b1, s1, Rate(rate))
        val r2       = Banking.reserveInterest(b1, s2, Rate(rate))
        decimal(r2) shouldBe (decimal(r1) * mult +- BigDecimal("1.0"))
      }
    }

  "computeReserveInterest total" should "equal sum of per-bank interest" in
    forAll(genBanking.Sector, genRate) { (bs, rate) =>
      val result = Banking.computeReserveInterest(bs.banks, bs.financialStocks, Rate(rate))
      decimal(result.total) shouldBe (result.perBank.map(decimal(_)).sum +- BigDecimal("0.01"))
    }

  // =========================================================================
  // Interbank Interest Properties
  // =========================================================================

  "interbankInterestFlows" should "net to zero for balanced positions" in
    forAll(genDecimal("-1e8", "1e8"), genRate) { (net1, rate) =>
      whenever(rate > BigDecimal("0.001")) {
        val rows   = Vector(
          mkBankRow(id = 0, interbankNet = PLN(net1)),
          mkBankRow(id = 1, interbankNet = PLN(-net1)),
        )
        val result = Banking.interbankInterestFlows(rows.map(_._1), rows.map(_._2), Rate(rate))
        decimal(result.total) shouldBe (BigDecimal("0.0") +- BigDecimal("1.0"))
      }
    }

  it should "return zero for all-zero positions" in
    forAll(genRate) { rate =>
      val rows   = Vector(
        mkBankRow(id = 0, interbankNet = PLN.Zero),
        mkBankRow(id = 1, interbankNet = PLN.Zero),
      )
      val result = Banking.interbankInterestFlows(rows.map(_._1), rows.map(_._2), Rate(rate))
      result.perBank.foreach(_ shouldBe PLN.Zero)
      result.total shouldBe PLN.Zero
    }

  // =========================================================================
  // SFC: consistent flows with monetary plumbing still pass
  // =========================================================================

  "Sfc with monetary plumbing flows" should "pass for consistent snapshots" in
    forAll(genConsistentFlowsAndSnapshots) { case (prev, curr, flows) =>
      val result = Sfc.validateStockExactness(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "detect reserve interest perturbation" in
    forAll(genConsistentFlowsAndSnapshots, genDecimal("5000.0", "1e6")) { case ((prev, curr, flows), delta) =>
      // Add reserve interest to flows but NOT to bank capital → should fail
      val perturbedFlows = flows.copy(reserveInterest = flows.reserveInterest + PLN(delta))
      val result         = Sfc.validateStockExactness(prev, curr, perturbedFlows)
      result shouldBe a[Left[?, ?]]
    }
