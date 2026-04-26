package com.boombustgroup.amorfati.accounting

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
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

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // Combined generator for gov update inputs (avoids >6 forAll limit)
  private val genGovUpdateInputs: Gen[(FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal)] =
    for
      prev     <- genGovState
      cit      <- genDecimal("0.0", "1e8")
      vat      <- genDecimal("0.0", "1e8")
      price    <- genPrice
      unempBen <- genDecimal("0.0", "1e7")
    yield (prev, cit, vat, price, unempBen)

  // --- BankState properties ---

  "BankingAggregate.nplRatio" should "be in [0, 1] when totalLoans > 1" in
    forAll(genBankingAggregate) { (bs: Banking.Aggregate) =>
      whenever(bs.totalLoans > PLN(1)) {
        bs.nplRatio.bd should be >= BigDecimal("0.0")
        bs.nplRatio.bd should be <= BigDecimal("1.0")
      }
    }

  it should "be 0 when totalLoans <= 1" in
    forAll(genDecimal("-100.0", "1.0"), genDecimal("0.0", "1e6")) { (loans: BigDecimal, capital: BigDecimal) =>
      val bs =
        Banking.Aggregate(plnBD(loans), PLN(500), plnBD(capital), PLN(1000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
      bs.nplRatio shouldBe Share.Zero
    }

  "BankingAggregate.car" should "be >= 0 for non-negative capital and loans" in
    forAll(genDecimal("0.0", "1e9"), genDecimal("0.0", "1e9")) { (capital: BigDecimal, loans: BigDecimal) =>
      whenever(loans > BigDecimal("1.0")) {
        val bs =
          Banking.Aggregate(plnBD(loans), PLN(0), plnBD(capital), PLN(1000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
        bs.car.bd should be >= BigDecimal("0.0")
      }
    }

  it should "be 10.0 when totalLoans <= 1" in
    forAll(genDecimal("-100.0", "1.0")) { (loans: BigDecimal) =>
      val bs = Banking.Aggregate(plnBD(loans), PLN(0), PLN(1000), PLN(1000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
      bs.car shouldBe Multiplier(10)
    }

  // lendingRate and canLend removed from BankingAggregate — now only on Banking.BankState

  // --- GovState properties ---

  "GovState" should "have deficit = spending - revenue via updateGov" in
    forAll(genGovUpdateInputs) { (inputs: (FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      whenever(price >= BigDecimal("0.01")) {
        val gov        =
          FiscalBudget.update(
            FiscalBudget.Input(
              prev,
              priceIndexBD(price),
              citPaid = plnBD(cit),
              govDividendRevenue = PLN.Zero,
              vat = plnBD(vat),
              unempBenefitSpend = plnBD(unempBen),
            ),
          )
        val totalRev   = cit + vat
        val totalSpend = unempBen + p.fiscal.govBaseSpending.bd * price
        val tol        = p.fiscal.govBaseSpending.bd * BigDecimal("0.0001") + BigDecimal("1.0") // fixed-point rounding tolerance
        gov.deficit.bd shouldBe (totalSpend - totalRev +- tol)
      }
    }

  // --- ForexState properties ---

  "ForexState" should "have tradeBalance = exports - imports" in
    forAll(genForexState) { (fs: OpenEconomy.ForexState) =>
      fs.tradeBalance.bd shouldBe ((fs.exports - fs.imports).bd +- BigDecimal("1.0"))
    }

  // --- BopState properties ---

  "BopState" should "have CA = tradeBalance + primaryIncome + secondaryIncome" in
    forAll(genBopState) { (bop: OpenEconomy.BopState) =>
      bop.currentAccount.bd shouldBe ((bop.tradeBalance + bop.primaryIncome + bop.secondaryIncome).bd +- BigDecimal("1.0"))
    }
