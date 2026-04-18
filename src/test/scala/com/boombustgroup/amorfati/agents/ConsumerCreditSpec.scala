package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

/** Consumer credit unit tests. */
class ConsumerCreditSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  "Config defaults" should "have sensible consumer credit parameters" in {
    p.household.ccSpread shouldBe Rate(0.04)
    p.household.ccMaxDti shouldBe Share(0.40)
    p.household.ccMaxLoan shouldBe PLN(50000.0)
    p.household.ccAmortRate shouldBe Rate(0.025)
    p.household.ccNplRecovery shouldBe Share(0.15)
    p.household.ccEligRate shouldBe Share(0.30)
  }

  "DTI limit" should "cap loan at headroom x income" in {
    // HH with income 8000, existing DTI = 0.20 -> headroom = (0.40 - 0.20) x 8000 = 1600
    val income      = 8000.0
    val existingDti = 0.20
    val headroom    = Math.max(0.0, td.toDouble(p.household.ccMaxDti) - existingDti) * income
    headroom shouldBe 1600.0 +- 0.01
    headroom should be < td.toDouble(p.household.ccMaxLoan) // 1600 < 50000
  }

  it should "produce zero loan when at max DTI" in {
    val income      = 8000.0
    val existingDti = 0.40
    val headroom    = Math.max(0.0, td.toDouble(p.household.ccMaxDti) - existingDti) * income
    headroom shouldBe 0.0
  }

  "Loan size" should "not exceed CcMaxLoan" in {
    // HH with high income, low DTI -> headroom > CcMaxLoan -> capped
    val income      = 200000.0
    val existingDti = 0.0
    val headroom    = Math.max(0.0, td.toDouble(p.household.ccMaxDti) - existingDti) * income
    val desired     = Math.min(headroom, td.toDouble(p.household.ccMaxLoan))
    desired shouldBe td.toDouble(p.household.ccMaxLoan)
  }

  "Consumer debt service" should "include both amortization and interest" in {
    val consumerDebt = 10000.0
    val refRate      = 0.0575
    val rate         = refRate + td.toDouble(p.household.ccSpread)
    val debtService  = consumerDebt * (td.toDouble(p.household.ccAmortRate) + rate / 12.0)
    // 10000 x (0.025 + (0.0575 + 0.04) / 12) = 10000 x (0.025 + 0.008125) = 331.25
    debtService shouldBe 331.25 +- 0.01
    debtService should be > 0.0
  }

  it should "reduce disposable income" in {
    val income          = 8000.0
    val rent            = 1800.0
    val existingDebtSvc = 500.0
    val consumerDebtSvc = 331.25
    val obligations     = rent + existingDebtSvc + consumerDebtSvc
    val disposable      = Math.max(0.0, income - obligations)
    disposable shouldBe (8000.0 - 1800.0 - 500.0 - 331.25) +- 0.01
    disposable should be < (income - rent - existingDebtSvc)
  }

  "Consumer spread" should "be applied on top of reference rate" in {
    val refRate      = 0.0575
    val consumerRate = refRate + td.toDouble(p.household.ccSpread)
    consumerRate shouldBe 0.0975 +- 0.001
    // Annualized consumer rate ~9.75% (NBP MIR consumer ~9-10%)
  }

  "Bankruptcy" should "trigger consumer debt default" in {
    val hh      = TestHouseholdState(
      id = HhId(0),
      savings = PLN(-5000.0),
      debt = PLN(1000.0),
      monthlyRent = PLN(1000.0),
      skill = Share(0.8),
      healthPenalty = Share(0.0),
      mpc = Share(0.82),
      status = HhStatus.Bankrupt,
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN(5000.0),
      education = 2,
      taskRoutineness = Share(0.5),
      wageScar = Share.Zero,
    )
    // Bankrupt HH should have consumer debt -> NPL
    hh.consumerDebt shouldBe PLN(5000.0)
    // NPL loss = consumerDebt * (1 - recovery)
    val nplLoss = td.toDouble(hh.consumerDebt) * (1.0 - td.toDouble(p.household.ccNplRecovery))
    nplLoss shouldBe 4250.0 +- 0.01
  }

  "Bank capital" should "absorb consumer NPL loss with CcNplRecovery" in {
    val defaultAmount = 10000.0
    val nplLoss       = defaultAmount * (1.0 - td.toDouble(p.household.ccNplRecovery))
    nplLoss shouldBe 8500.0 +- 0.01
    // Lower recovery (15%) than firm NPL (30%) -> higher bank capital impact
    val firmNplLoss   = defaultAmount * (1.0 - td.toDouble(p.banking.loanRecovery))
    nplLoss should be > firmNplLoss
  }

  "Consumer credit stock identity" should "balance origination minus principal minus defaults" in {
    val prevStock      = 100000.0
    val origination    = 5000.0
    val refRate        = 0.0575
    val totalRate      = td.toDouble(p.household.ccAmortRate) + (refRate + td.toDouble(p.household.ccSpread)) / 12.0
    val debtService    = prevStock * totalRate
    val principal      = debtService * (td.toDouble(p.household.ccAmortRate) / totalRate)
    val defaultAmt     = 1000.0
    val newStock       = prevStock + origination - principal - defaultAmt
    val expectedChange = origination - principal - defaultAmt
    (newStock - prevStock) shouldBe expectedChange +- 0.01
  }

  "Consumption smoothing" should "allow borrower to consume more than without credit" in {
    val disposable        = 2000.0
    val newLoan           = 3000.0
    val mpc               = 0.82
    val consWithCredit    = (disposable + newLoan) * mpc
    val consWithoutCredit = disposable * mpc
    consWithCredit should be > consWithoutCredit
    (consWithCredit - consWithoutCredit) shouldBe (newLoan * mpc) +- 0.01
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

  "Household" should "have consumerDebt field defaulting to 0" in {
    val hh = TestHouseholdState(
      id = HhId(0),
      savings = PLN(10000.0),
      debt = PLN.Zero,
      monthlyRent = PLN(1000.0),
      skill = Share(0.8),
      healthPenalty = Share(0.0),
      mpc = Share(0.82),
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8266.0)),
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share(0.5),
      wageScar = Share.Zero,
    )
    hh.consumerDebt shouldBe PLN.Zero
  }

  "BankingAggregate" should "have consumerLoans and consumerNpl fields" in {
    val bank = Banking.Aggregate(PLN(1000.0), PLN(50.0), PLN(500.0), PLN(2000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    bank.consumerLoans shouldBe PLN.Zero
    bank.consumerNpl shouldBe PLN.Zero
  }

  "BankingAggregate.car" should "include consumer loans in RWA" in {
    val bank     =
      Banking.Aggregate(PLN(1000.0), PLN(50.0), PLN(500.0), PLN(2000.0), PLN.Zero, PLN.Zero, PLN(1000.0), PLN.Zero, PLN.Zero)
    // CAR = capital / (totalLoans + consumerLoans) = 500 / 2000 = 0.25
    td.toDouble(bank.car) shouldBe 0.25 +- 0.01
    // Without consumer loans: CAR = 500 / 1000 = 0.50
    val bankNoCc =
      Banking.Aggregate(PLN(1000.0), PLN(50.0), PLN(500.0), PLN(2000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    td.toDouble(bankNoCc.car) shouldBe 0.50 +- 0.01
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
  )

  "Sfc" should "pass consumer credit identity with zero flows" in {
    val snap   = zeroSnap.copy(bankCapital = PLN(100.0), bankDeposits = PLN(200.0))
    val flow   = zeroFlows
    val result = Sfc.validateStockExactness(snap, snap, flow)
    result shouldBe Right(())
  }
