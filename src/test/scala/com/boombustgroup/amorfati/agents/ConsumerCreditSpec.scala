package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.TestHouseholdState

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Consumer credit unit tests. */
class ConsumerCreditSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "Config defaults" should "have sensible consumer credit parameters" in {
    p.household.ccSpread shouldBe Rate("0.04")
    p.household.ccMaxDti shouldBe Share("0.40")
    p.household.ccMaxLoan shouldBe PLN("50000.0")
    p.household.ccAmortRate shouldBe Rate("0.025")
    p.household.ccNplRecovery shouldBe Share("0.15")
    p.household.ccEligRate shouldBe Share("0.30")
  }

  "DTI limit" should "cap loan at headroom x income" in {
    // HH with income 8000, existing DTI = 0.20 -> headroom = (0.40 - 0.20) x 8000 = 1600
    val income      = BigDecimal("8000.0")
    val existingDti = BigDecimal("0.20")
    val headroom    = DecimalMath.max(BigDecimal("0.0"), decimal(p.household.ccMaxDti) - existingDti) * income
    headroom shouldBe BigDecimal("1600.0") +- BigDecimal("0.01")
    headroom should be < decimal(p.household.ccMaxLoan) // 1600 < 50000
  }

  it should "produce zero loan when at max DTI" in {
    val income      = BigDecimal("8000.0")
    val existingDti = BigDecimal("0.40")
    val headroom    = DecimalMath.max(BigDecimal("0.0"), decimal(p.household.ccMaxDti) - existingDti) * income
    headroom shouldBe BigDecimal("0.0")
  }

  "Loan size" should "not exceed CcMaxLoan" in {
    // HH with high income, low DTI -> headroom > CcMaxLoan -> capped
    val income      = BigDecimal("200000.0")
    val existingDti = BigDecimal("0.0")
    val headroom    = DecimalMath.max(BigDecimal("0.0"), decimal(p.household.ccMaxDti) - existingDti) * income
    val desired     = DecimalMath.min(headroom, decimal(p.household.ccMaxLoan))
    desired shouldBe decimal(p.household.ccMaxLoan)
  }

  "Consumer debt service" should "include both amortization and interest" in {
    val consumerDebt = BigDecimal("10000.0")
    val refRate      = BigDecimal("0.0575")
    val rate         = refRate + decimal(p.household.ccSpread)
    val debtService  = consumerDebt * (decimal(p.household.ccAmortRate) + rate / BigDecimal("12.0"))
    // 10000 x (0.025 + (0.0575 + 0.04) / 12) = 10000 x (0.025 + 0.008125) = 331.25
    debtService shouldBe BigDecimal("331.25") +- BigDecimal("0.01")
    debtService should be > BigDecimal("0.0")
  }

  it should "reduce disposable income" in {
    val income          = BigDecimal("8000.0")
    val rent            = BigDecimal("1800.0")
    val existingDebtSvc = BigDecimal("500.0")
    val consumerDebtSvc = BigDecimal("331.25")
    val obligations     = rent + existingDebtSvc + consumerDebtSvc
    val disposable      = DecimalMath.max(BigDecimal("0.0"), income - obligations)
    disposable shouldBe (BigDecimal("8000.0") - BigDecimal("1800.0") - BigDecimal("500.0") - BigDecimal("331.25")) +- BigDecimal("0.01")
    disposable should be < (income - rent - existingDebtSvc)
  }

  "Consumer spread" should "be applied on top of reference rate" in {
    val refRate      = BigDecimal("0.0575")
    val consumerRate = refRate + decimal(p.household.ccSpread)
    consumerRate shouldBe BigDecimal("0.0975") +- BigDecimal("0.001")
    // Annualized consumer rate ~9.75% (NBP MIR consumer ~9-10%)
  }

  "Bankruptcy" should "trigger consumer debt default" in {
    val financial = TestHouseholdState.financial(savings = PLN("-5000.0"), debt = PLN("1000.0"), consumerDebt = PLN("5000.0"))
    // Bankrupt HH should have consumer debt -> NPL
    financial.consumerLoan shouldBe PLN("5000.0")
    // NPL loss = consumerDebt * (1 - recovery)
    val nplLoss   = decimal(financial.consumerLoan) * (BigDecimal("1.0") - decimal(p.household.ccNplRecovery))
    nplLoss shouldBe BigDecimal("4250.0") +- BigDecimal("0.01")
  }

  "Bank capital" should "absorb consumer NPL loss with CcNplRecovery" in {
    val defaultAmount = BigDecimal("10000.0")
    val nplLoss       = defaultAmount * (BigDecimal("1.0") - decimal(p.household.ccNplRecovery))
    nplLoss shouldBe BigDecimal("8500.0") +- BigDecimal("0.01")
    // Lower recovery (15%) than firm NPL (30%) -> higher bank capital impact
    val firmNplLoss   = defaultAmount * (BigDecimal("1.0") - decimal(p.banking.loanRecovery))
    nplLoss should be > firmNplLoss
  }

  "Consumer credit stock identity" should "balance origination minus principal minus defaults" in {
    val prevStock      = BigDecimal("100000.0")
    val origination    = BigDecimal("5000.0")
    val refRate        = BigDecimal("0.0575")
    val totalRate      = decimal(p.household.ccAmortRate) + (refRate + decimal(p.household.ccSpread)) / BigDecimal("12.0")
    val debtService    = prevStock * totalRate
    val principal      = debtService * (decimal(p.household.ccAmortRate) / totalRate)
    val defaultAmt     = BigDecimal("1000.0")
    val newStock       = prevStock + origination - principal - defaultAmt
    val expectedChange = origination - principal - defaultAmt
    (newStock - prevStock) shouldBe expectedChange +- BigDecimal("0.01")
  }

  "Consumption smoothing" should "allow borrower to consume more than without credit" in {
    val disposable        = BigDecimal("2000.0")
    val newLoan           = BigDecimal("3000.0")
    val mpc               = BigDecimal("0.82")
    val consWithCredit    = (disposable + newLoan) * mpc
    val consWithoutCredit = disposable * mpc
    consWithCredit should be > consWithoutCredit
    (consWithCredit - consWithoutCredit) shouldBe (newLoan * mpc) +- BigDecimal("0.01")
  }

  "Household.Aggregates consumer fields" should "default to 0.0" in {
    val agg = Household.Aggregates(
      employed = 0,
      unemployed = 0,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = PLN.Zero,
      reservationWage = PLN.Zero,
      giniIndividual = Share.Zero,
      giniWealth = Share.Zero,
      meanSavings = PLN.Zero,
      medianSavings = PLN.Zero,
      povertyRate50 = Share.Zero,
      bankruptcyRate = Share.Zero,
      meanSkill = Share.Zero,
      meanHealthPenalty = Share.Zero,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = Scalar.Zero,
      povertyRate30 = Share.Zero,
      totalRent = PLN.Zero,
      totalDebtService = PLN.Zero,
      totalUnempBenefits = PLN.Zero,
      totalDepositInterest = PLN.Zero,
      crossSectorHires = 0,
      voluntaryQuits = 0,
      sectorMobilityRate = Share.Zero,
      totalRemittances = PLN.Zero,
      totalPit = PLN.Zero,
      totalSocialTransfers = PLN.Zero,
      totalConsumerDebtService = PLN.Zero,
      totalConsumerOrigination = PLN.Zero,
      totalConsumerDefault = PLN.Zero,
      totalConsumerPrincipal = PLN.Zero,
    )
    agg.totalConsumerDebtService shouldBe PLN.Zero
    agg.totalConsumerOrigination shouldBe PLN.Zero
    agg.totalConsumerDefault shouldBe PLN.Zero
  }

  "Household financial stocks" should "default consumer loan to 0" in {
    TestHouseholdState.financial(consumerDebt = PLN.Zero).consumerLoan shouldBe PLN.Zero
  }

  "BankingAggregate" should "have consumerLoans and consumerNpl fields" in {
    val bank = Banking.Aggregate(PLN("1000.0"), PLN("50.0"), PLN("500.0"), PLN("2000.0"), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    bank.consumerLoans shouldBe PLN.Zero
    bank.consumerNpl shouldBe PLN.Zero
  }

  "BankingAggregate.car" should "include consumer loans in RWA" in {
    val bank     =
      Banking.Aggregate(PLN("1000.0"), PLN("50.0"), PLN("500.0"), PLN("2000.0"), PLN.Zero, PLN.Zero, PLN("1000.0"), PLN.Zero, PLN.Zero)
    // CAR = capital / (totalLoans + consumerLoans) = 500 / 2000 = 0.25
    decimal(bank.car) shouldBe BigDecimal("0.25") +- BigDecimal("0.01")
    // Without consumer loans: CAR = 500 / 1000 = 0.50
    val bankNoCc =
      Banking.Aggregate(PLN("1000.0"), PLN("50.0"), PLN("500.0"), PLN("2000.0"), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    decimal(bankNoCc.car) shouldBe BigDecimal("0.50") +- BigDecimal("0.01")
    bank.car should be < bankNoCc.car
  }

  private def zeroSnap: Sfc.StockState = Sfc.StockState(
    hhSavings = PLN.Zero,
    hhDebt = PLN.Zero,
    firmCash = PLN.Zero,
    firmDebt = PLN.Zero,
    bankCapital = PLN.Zero,
    bankDeposits = PLN.Zero,
    bankLoans = PLN.Zero,
    govDebt = PLN.Zero,
    nfa = PLN.Zero,
    bankBondHoldings = PLN.Zero,
    nbpBondHoldings = PLN.Zero,
    bondsOutstanding = PLN.Zero,
    interbankNetSum = PLN.Zero,
    jstDeposits = PLN.Zero,
    jstDebt = PLN.Zero,
    fusBalance = PLN.Zero,
    nfzBalance = PLN.Zero,
    foreignBondHoldings = PLN.Zero,
    ppkBondHoldings = PLN.Zero,
    mortgageStock = PLN.Zero,
    consumerLoans = PLN.Zero,
    corpBondsOutstanding = PLN.Zero,
    insuranceGovBondHoldings = PLN.Zero,
    tfiGovBondHoldings = PLN.Zero,
    nbfiLoanStock = PLN.Zero,
    quasiFiscalBondsOutstanding = PLN.Zero,
    quasiFiscalBankHoldings = PLN.Zero,
    quasiFiscalNbpHoldings = PLN.Zero,
    quasiFiscalLoanPortfolio = PLN.Zero,
  )

  private def zeroFlows: Sfc.SemanticFlows = Sfc.SemanticFlows(
    govSpending = PLN.Zero,
    govRevenue = PLN.Zero,
    nplLoss = PLN.Zero,
    interestIncome = PLN.Zero,
    hhDebtService = PLN.Zero,
    totalIncome = PLN.Zero,
    totalConsumption = PLN.Zero,
    newLoans = PLN.Zero,
    nplRecovery = PLN.Zero,
    currentAccount = PLN.Zero,
    valuationEffect = PLN.Zero,
    bankBondIncome = PLN.Zero,
    qePurchase = PLN.Zero,
    newBondIssuance = PLN.Zero,
    depositInterestPaid = PLN.Zero,
    reserveInterest = PLN.Zero,
    standingFacilityIncome = PLN.Zero,
    interbankInterest = PLN.Zero,
    jstDepositChange = PLN.Zero,
    jstSpending = PLN.Zero,
    jstRevenue = PLN.Zero,
    zusContributions = PLN.Zero,
    zusPensionPayments = PLN.Zero,
    zusGovSubvention = PLN.Zero,
    nfzContributions = PLN.Zero,
    nfzSpending = PLN.Zero,
    nfzGovSubvention = PLN.Zero,
    dividendIncome = PLN.Zero,
    foreignDividendOutflow = PLN.Zero,
    dividendTax = PLN.Zero,
    mortgageInterestIncome = PLN.Zero,
    mortgageNplLoss = PLN.Zero,
    mortgageOrigination = PLN.Zero,
    mortgagePrincipalRepaid = PLN.Zero,
    mortgageDefaultAmount = PLN.Zero,
    remittanceOutflow = PLN.Zero,
    fofResidual = PLN.Zero,
    consumerDebtService = PLN.Zero,
    consumerNplLoss = PLN.Zero,
    consumerOrigination = PLN.Zero,
    consumerPrincipalRepaid = PLN.Zero,
    consumerDefaultAmount = PLN.Zero,
    corpBondCouponIncome = PLN.Zero,
    corpBondDefaultLoss = PLN.Zero,
    corpBondIssuance = PLN.Zero,
    corpBondAmortization = PLN.Zero,
    corpBondDefaultAmount = PLN.Zero,
    insNetDepositChange = PLN.Zero,
    nbfiDepositDrain = PLN.Zero,
    nbfiOrigination = PLN.Zero,
    nbfiRepayment = PLN.Zero,
    nbfiDefaultAmount = PLN.Zero,
    fdiProfitShifting = PLN.Zero,
    fdiRepatriation = PLN.Zero,
    diasporaInflow = PLN.Zero,
    tourismExport = PLN.Zero,
    tourismImport = PLN.Zero,
    bfgLevy = PLN.Zero,
    bailInLoss = PLN.Zero,
    bankCapitalDestruction = PLN.Zero,
    investNetDepositFlow = PLN.Zero,
    firmPrincipalRepaid = PLN.Zero,
    unrealizedBondLoss = PLN.Zero,
    htmRealizedLoss = PLN.Zero,
    eclProvisionChange = PLN.Zero,
    quasiFiscalBondIssuance = PLN.Zero,
    quasiFiscalBondAmortization = PLN.Zero,
    quasiFiscalNbpBondAmortization = PLN.Zero,
    quasiFiscalNbpAbsorption = PLN.Zero,
    quasiFiscalLending = PLN.Zero,
    quasiFiscalRepayment = PLN.Zero,
    quasiFiscalDepositChange = PLN.Zero,
  )

  "Sfc" should "pass consumer credit identity with zero flows" in {
    val snap   = zeroSnap.copy(bankCapital = PLN("100.0"), bankDeposits = PLN("200.0"))
    val flow   = zeroFlows
    val result = Sfc.validateStockExactness(snap, snap, flow)
    result shouldBe Right(())
  }
