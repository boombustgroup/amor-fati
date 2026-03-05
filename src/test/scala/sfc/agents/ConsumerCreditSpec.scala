package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankState, ForexState, GovState, SfcCheck}
import sfc.config.Config
import sfc.engine.World
import sfc.types.*

import scala.util.Random

/** Consumer credit unit tests. */
class ConsumerCreditSpec extends AnyFlatSpec with Matchers:

  // Config defaults: CcSpread=0.04, CcMaxDti=0.40, CcMaxLoan=50000, CcAmortRate=0.025,
  //                  CcNplRecovery=0.15, CcEligRate=0.30

  "Config defaults" should "have sensible consumer credit parameters" in {
    Config.CcSpread shouldBe 0.04
    Config.CcMaxDti shouldBe 0.40
    Config.CcMaxLoan shouldBe 50000.0
    Config.CcAmortRate shouldBe 0.025
    Config.CcNplRecovery shouldBe 0.15
    Config.CcEligRate shouldBe 0.30
  }

  "DTI limit" should "cap loan at headroom × income" in {
    // HH with income 8000, existing DTI = 0.20 → headroom = (0.40 - 0.20) × 8000 = 1600
    val income = 8000.0
    val existingDti = 0.20
    val headroom = Math.max(0.0, Config.CcMaxDti - existingDti) * income
    headroom shouldBe 1600.0 +- 0.01
    headroom should be < Config.CcMaxLoan  // 1600 < 50000
  }

  it should "produce zero loan when at max DTI" in {
    val income = 8000.0
    val existingDti = 0.40
    val headroom = Math.max(0.0, Config.CcMaxDti - existingDti) * income
    headroom shouldBe 0.0
  }

  "Loan size" should "not exceed CcMaxLoan" in {
    // HH with high income, low DTI → headroom > CcMaxLoan → capped
    val income = 200000.0
    val existingDti = 0.0
    val headroom = Math.max(0.0, Config.CcMaxDti - existingDti) * income
    val desired = Math.min(headroom, Config.CcMaxLoan)
    desired shouldBe Config.CcMaxLoan
  }

  "Consumer debt service" should "include both amortization and interest" in {
    val consumerDebt = 10000.0
    val refRate = 0.0575
    val rate = refRate + Config.CcSpread
    val debtService = consumerDebt * (Config.CcAmortRate + rate / 12.0)
    // 10000 × (0.025 + (0.0575 + 0.04) / 12) = 10000 × (0.025 + 0.008125) = 331.25
    debtService shouldBe 331.25 +- 0.01
    debtService should be > 0.0
  }

  it should "reduce disposable income" in {
    val income = 8000.0
    val rent = 1800.0
    val existingDebtSvc = 500.0
    val consumerDebtSvc = 331.25
    val obligations = rent + existingDebtSvc + consumerDebtSvc
    val disposable = Math.max(0.0, income - obligations)
    disposable shouldBe (8000.0 - 1800.0 - 500.0 - 331.25) +- 0.01
    disposable should be < (income - rent - existingDebtSvc)
  }

  "Consumer spread" should "be applied on top of reference rate" in {
    val refRate = 0.0575
    val consumerRate = refRate + Config.CcSpread
    consumerRate shouldBe 0.0975 +- 0.001
    // Annualized consumer rate ~9.75% (NBP MIR consumer ~9-10%)
  }

  "Bankruptcy" should "trigger consumer debt default" in {
    val hh = Household(
      id = 0, savings = -5000.0, debt = 1000.0, monthlyRent = 1000.0,
      skill = 0.8, healthPenalty = 0.0, mpc = 0.82,
      status = HhStatus.Bankrupt, socialNeighbors = Array.empty[Int],
      consumerDebt = 5000.0
    )
    // Bankrupt HH should have consumer debt → NPL
    hh.consumerDebt shouldBe 5000.0
    // NPL loss = consumerDebt × (1 - recovery)
    val nplLoss = hh.consumerDebt * (1.0 - Config.CcNplRecovery)
    nplLoss shouldBe 4250.0 +- 0.01
  }

  "Bank capital" should "absorb consumer NPL loss with CcNplRecovery" in {
    val defaultAmount = 10000.0
    val nplLoss = defaultAmount * (1.0 - Config.CcNplRecovery)
    nplLoss shouldBe 8500.0 +- 0.01
    // Lower recovery (15%) than firm NPL (30%) → higher bank capital impact
    val firmNplLoss = defaultAmount * (1.0 - Config.LoanRecovery)
    nplLoss should be > firmNplLoss
  }

  "Consumer credit stock identity" should "balance origination minus principal minus defaults" in {
    val prevStock = 100000.0
    val origination = 5000.0
    val refRate = 0.0575
    val totalRate = Config.CcAmortRate + (refRate + Config.CcSpread) / 12.0
    val debtService = prevStock * totalRate
    val principal = debtService * (Config.CcAmortRate / totalRate)
    val defaultAmt = 1000.0
    val newStock = prevStock + origination - principal - defaultAmt
    val expectedChange = origination - principal - defaultAmt
    (newStock - prevStock) shouldBe expectedChange +- 0.01
  }

  "Consumption smoothing" should "allow borrower to consume more than without credit" in {
    val disposable = 2000.0
    val newLoan = 3000.0
    val mpc = 0.82
    val consWithCredit = (disposable + newLoan) * mpc
    val consWithoutCredit = disposable * mpc
    consWithCredit should be > consWithoutCredit
    (consWithCredit - consWithoutCredit) shouldBe (newLoan * mpc) +- 0.01
  }

  "HhAggregates consumer fields" should "default to 0.0" in {
    val agg = HhAggregates(
      employed = 0, unemployed = 0, retraining = 0, bankrupt = 0,
      totalIncome = 0, consumption = 0, domesticConsumption = 0, importConsumption = 0,
      marketWage = 0, reservationWage = 0, giniIndividual = 0, giniWealth = 0,
      meanSavings = 0, medianSavings = 0, povertyRate50 = 0, bankruptcyRate = 0,
      meanSkill = 0, meanHealthPenalty = 0, retrainingAttempts = 0, retrainingSuccesses = 0,
      consumptionP10 = 0, consumptionP50 = 0, consumptionP90 = 0,
      meanMonthsToRuin = 0, povertyRate30 = 0, totalRent = 0,
      totalDebtService = 0, totalUnempBenefits = 0
    )
    agg.totalConsumerDebtService shouldBe 0.0
    agg.totalConsumerOrigination shouldBe 0.0
    agg.totalConsumerDefault shouldBe 0.0
  }

  "Household" should "have consumerDebt field defaulting to 0" in {
    val hh = Household(
      id = 0, savings = 10000.0, debt = 0.0, monthlyRent = 1000.0,
      skill = 0.8, healthPenalty = 0.0, mpc = 0.82,
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), 8266.0), socialNeighbors = Array.empty[Int]
    )
    hh.consumerDebt shouldBe 0.0
  }

  "BankState" should "have consumerLoans and consumerNpl fields" in {
    val bank = BankState(1000.0, 50.0, 500.0, 2000.0)
    bank.consumerLoans shouldBe 0.0
    bank.consumerNpl shouldBe 0.0
  }

  "BankState.car" should "include consumer loans in RWA" in {
    val bank = BankState(1000.0, 50.0, 500.0, 2000.0, consumerLoans = 1000.0)
    // CAR = capital / (totalLoans + consumerLoans) = 500 / 2000 = 0.25
    bank.car shouldBe 0.25 +- 0.01
    // Without consumer loans: CAR = 500 / 1000 = 0.50
    val bankNoCc = BankState(1000.0, 50.0, 500.0, 2000.0)
    bankNoCc.car shouldBe 0.50 +- 0.01
    bank.car should be < bankNoCc.car
  }

  "SfcCheck" should "include consumerCreditError in SfcResult" in {
    val snap = SfcCheck.Snapshot(0, 0, 0, 0, 100.0, 200.0, 0, 0)
    val flow = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val result = SfcCheck.validate(1, snap, snap, flow)
    result.consumerCreditError shouldBe 0.0 +- 0.01
  }
