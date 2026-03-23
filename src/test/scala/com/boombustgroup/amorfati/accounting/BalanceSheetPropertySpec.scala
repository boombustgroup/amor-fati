package com.boombustgroup.amorfati.accounting

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.types.*

class BalanceSheetPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // Combined generator for gov update inputs (avoids >6 forAll limit)
  private val genGovUpdateInputs: Gen[(FiscalBudget.GovState, Double, Double, Double, Double)] =
    for
      prev     <- genGovState
      cit      <- Gen.choose(0.0, 1e8)
      vat      <- Gen.choose(0.0, 1e8)
      price    <- genPrice
      unempBen <- Gen.choose(0.0, 1e7)
    yield (prev, cit, vat, price, unempBen)

  // --- BankState properties ---

  "BankingAggregate.nplRatio" should "be in [0, 1] when totalLoans > 1" in
    forAll(genBankingAggregate) { (bs: Banking.Aggregate) =>
      whenever(bs.totalLoans > PLN(1.0)) {
        td.toDouble(bs.nplRatio) should be >= 0.0
        td.toDouble(bs.nplRatio) should be <= 1.0
      }
    }

  it should "be 0 when totalLoans <= 1" in
    forAll(Gen.choose(-100.0, 1.0), Gen.choose(0.0, 1e6)) { (loans: Double, capital: Double) =>
      val bs =
        Banking.Aggregate(PLN(loans), PLN(500.0), PLN(capital), PLN(1000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
      bs.nplRatio shouldBe Share.Zero
    }

  "BankingAggregate.car" should "be >= 0 for non-negative capital and loans" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (capital: Double, loans: Double) =>
      whenever(loans > 1.0) {
        val bs =
          Banking.Aggregate(PLN(loans), PLN(0.0), PLN(capital), PLN(1000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
        td.toDouble(bs.car) should be >= 0.0
      }
    }

  it should "be 10.0 when totalLoans <= 1" in
    forAll(Gen.choose(-100.0, 1.0)) { (loans: Double) =>
      val bs = Banking.Aggregate(PLN(loans), PLN(0.0), PLN(1000.0), PLN(1000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
      bs.car shouldBe Multiplier(10.0)
    }

  // lendingRate and canLend removed from BankingAggregate — now only on Banking.BankState

  // --- GovState properties ---

  "GovState" should "have deficit = spending - revenue via updateGov" in
    forAll(genGovUpdateInputs) { (inputs: (FiscalBudget.GovState, Double, Double, Double, Double)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               = FiscalBudget.update(FiscalBudget.Input(prev, price, citPaid = PLN(cit), vat = PLN(vat), unempBenefitSpend = PLN(unempBen)))
      val totalRev                          = cit + vat
      val totalSpend                        = unempBen + td.toDouble(p.fiscal.govBaseSpending) * price
      td.toDouble(gov.deficit) shouldBe (totalSpend - totalRev +- 1.0)
    }

  // --- ForexState properties ---

  "ForexState" should "have tradeBalance = exports - imports" in
    forAll(genForexState) { (fs: OpenEconomy.ForexState) =>
      td.toDouble(fs.tradeBalance) shouldBe (td.toDouble(fs.exports - fs.imports) +- 1.0)
    }

  // --- BopState properties ---

  "BopState" should "have CA = tradeBalance + primaryIncome + secondaryIncome" in
    forAll(genBopState) { (bop: OpenEconomy.BopState) =>
      td.toDouble(bop.currentAccount) shouldBe (td.toDouble(bop.tradeBalance + bop.primaryIncome + bop.secondaryIncome) +- 1.0)
    }
