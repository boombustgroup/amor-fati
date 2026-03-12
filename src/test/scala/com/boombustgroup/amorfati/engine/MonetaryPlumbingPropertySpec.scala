package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
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

  private def mkBank(
      id: Int = 0,
      deposits: PLN = PLN(1e9),
      loans: PLN = PLN(5e8),
      capital: PLN = PLN(1e8),
      reservesAtNbp: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
  ): Banking.BankState = Banking.BankState(
    id = BankId(id),
    deposits = deposits,
    loans = loans,
    capital = capital,
    nplAmount = PLN.Zero,
    govBondHoldings = PLN.Zero,
    reservesAtNbp = reservesAtNbp,
    interbankNet = interbankNet,
    status = status,
    demandDeposits = PLN.Zero,
    termDeposits = PLN.Zero,
    loansShort = PLN.Zero,
    loansMedium = PLN.Zero,
    loansLong = PLN.Zero,
    consumerLoans = PLN.Zero,
    consumerNpl = PLN.Zero,
    corpBondHoldings = PLN.Zero,
  )

  // =========================================================================
  // Reserve Interest Properties
  // =========================================================================

  "reserveInterest" should "be non-negative for alive banks with non-negative reserves" in
    forAll(genBanking.BankState, genRate) { (bank, rate) =>
      whenever(!bank.failed && bank.reservesAtNbp >= PLN.Zero && rate >= 0) {
        Banking.reserveInterest(bank, Rate(rate)) should be >= PLN.Zero
      }
    }

  it should "scale linearly with reserves" in
    forAll(genRate, Gen.choose(1e4, 1e9), Gen.choose(1.1, 5.0)) { (rate, reserves, mult) =>
      whenever(rate > 0.001) {
        val b1 = mkBank(reservesAtNbp = PLN(reserves))
        val b2 = b1.copy(reservesAtNbp = PLN(reserves * mult))
        val r1 = Banking.reserveInterest(b1, Rate(rate))
        val r2 = Banking.reserveInterest(b2, Rate(rate))
        r2.toDouble shouldBe (r1.toDouble * mult +- 1.0)
      }
    }

  "computeReserveInterest total" should "equal sum of per-bank interest" in
    forAll(genBanking.State, genRate) { (bs, rate) =>
      val result = Banking.computeReserveInterest(bs.banks, Rate(rate))
      result.total.toDouble shouldBe (result.perBank.map(_.toDouble).sum +- 0.01)
    }

  // =========================================================================
  // Interbank Interest Properties
  // =========================================================================

  "interbankInterestFlows" should "net to zero for balanced positions" in
    forAll(Gen.choose(-1e8, 1e8), genRate) { (net1, rate) =>
      whenever(rate > 0.001) {
        val banks  = Vector(
          mkBank(id = 0, interbankNet = PLN(net1)),
          mkBank(id = 1, interbankNet = PLN(-net1)),
        )
        val result = Banking.interbankInterestFlows(banks, Rate(rate))
        result.total.toDouble shouldBe (0.0 +- 1.0)
      }
    }

  it should "return zero for all-zero positions" in
    forAll(genRate) { rate =>
      val banks  = Vector(
        mkBank(id = 0, interbankNet = PLN.Zero),
        mkBank(id = 1, interbankNet = PLN.Zero),
      )
      val result = Banking.interbankInterestFlows(banks, Rate(rate))
      result.perBank.foreach(_ shouldBe PLN.Zero)
      result.total shouldBe PLN.Zero
    }

  // =========================================================================
  // SFC: consistent flows with monetary plumbing still pass
  // =========================================================================

  "Sfc with monetary plumbing flows" should "pass for consistent snapshots" in
    forAll(genConsistentFlowsAndSnapshots) { case (prev, curr, flows) =>
      val result = Sfc.validate(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "detect reserve interest perturbation" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(1000.0, 1e6)) { case ((prev, curr, flows), delta) =>
      // Add reserve interest to flows but NOT to bank capital → should fail
      val perturbedFlows = flows.copy(reserveInterest = flows.reserveInterest + PLN(delta))
      val result         = Sfc.validate(prev, curr, perturbedFlows)
      result shouldBe a[Left[?, ?]]
    }
