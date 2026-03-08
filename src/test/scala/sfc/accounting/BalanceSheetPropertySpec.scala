package sfc.accounting

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.config.Config
import sfc.types.*

class BalanceSheetPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // Combined generator for gov update inputs (avoids >6 forAll limit)
  private val genGovUpdateInputs: Gen[(GovState, Double, Double, Boolean, Double, Double, Double)] =
    for
      prev <- genGovState
      cit <- Gen.choose(0.0, 1e8)
      vat <- Gen.choose(0.0, 1e8)
      active <- Gen.oneOf(true, false)
      bdp <- Gen.choose(0.0, 5000.0)
      price <- genPrice
      unempBen <- Gen.choose(0.0, 1e7)
    yield (prev, cit, vat, active, bdp, price, unempBen)

  // --- BankState properties ---

  "BankingAggregate.nplRatio" should "be in [0, 1] when totalLoans > 1" in {
    forAll(genBankingAggregate) { (bs: BankingAggregate) =>
      whenever(bs.totalLoans > PLN(1.0)) {
        bs.nplRatio should be >= 0.0
        bs.nplRatio should be <= 1.0
      }
    }
  }

  it should "be 0 when totalLoans <= 1" in {
    forAll(Gen.choose(-100.0, 1.0), Gen.choose(0.0, 1e6)) { (loans: Double, capital: Double) =>
      val bs =
        BankingAggregate(PLN(loans), PLN(500.0), PLN(capital), PLN(1000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
      bs.nplRatio shouldBe 0.0
    }
  }

  "BankingAggregate.car" should "be >= 0 for non-negative capital and loans" in {
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (capital: Double, loans: Double) =>
      whenever(loans > 1.0) {
        val bs =
          BankingAggregate(PLN(loans), PLN(0.0), PLN(capital), PLN(1000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
        bs.car should be >= 0.0
      }
    }
  }

  it should "be 10.0 when totalLoans <= 1" in {
    forAll(Gen.choose(-100.0, 1.0)) { (loans: Double) =>
      val bs = BankingAggregate(PLN(loans), PLN(0.0), PLN(1000.0), PLN(1000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
      bs.car shouldBe 10.0
    }
  }

  // lendingRate and canLend removed from BankingAggregate — now only on Banking.BankState

  // --- GovState properties ---

  "GovState" should "have deficit = spending - revenue via updateGov" in {
    forAll(genGovUpdateInputs) { (inputs: (GovState, Double, Double, Boolean, Double, Double, Double)) =>
      val (prev, cit, vat, active, bdp, price, unempBen) = inputs
      val gov = sfc.engine.FiscalBudget.update(prev, cit, vat, active, bdp, price, unempBen)
      val totalRev = cit + vat
      val bdpSpend = if active then Config.TotalPopulation.toDouble * bdp else 0.0
      val totalSpend = bdpSpend + unempBen + Config.GovBaseSpending * price
      gov.deficit.toDouble shouldBe (totalSpend - totalRev +- 1.0)
    }
  }

  // --- ForexState properties ---

  "ForexState" should "have tradeBalance = exports - imports" in {
    forAll(genForexState) { (fs: ForexState) =>
      fs.tradeBalance.toDouble shouldBe ((fs.exports - fs.imports).toDouble +- 1e-6)
    }
  }

  // --- BopState properties ---

  "BopState" should "have CA = tradeBalance + primaryIncome + secondaryIncome" in {
    forAll(genBopState) { (bop: BopState) =>
      bop.currentAccount.toDouble shouldBe ((bop.tradeBalance + bop.primaryIncome + bop.secondaryIncome).toDouble +- 1e-6)
    }
  }
