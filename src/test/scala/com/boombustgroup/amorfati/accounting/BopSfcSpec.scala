package com.boombustgroup.amorfati.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

class BopSfcSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private def errorDelta(result: Either[Vector[Sfc.SfcIdentityError], Unit], id: Sfc.SfcIdentity): Double =
    result.swap.getOrElse(Vector.empty).find(_.identity == id).map(e => (e.actual - e.expected).toDouble).getOrElse(0.0)

  private def zeroSnap: Sfc.Snapshot = Sfc.Snapshot(
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

  private def zeroFlows: Sfc.MonthlyFlows = Sfc.MonthlyFlows(
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

  private val baseSnap =
    zeroSnap.copy(firmCash = PLN(500000), bankCapital = PLN(200000), bankDeposits = PLN(1000000))

  // ---- Identity 4: NFA ----

  "Sfc.validate (NFA)" should "pass trivially when OPEN_ECON is off (all zeros)" in {
    val result = Sfc.validate(baseSnap, baseSnap, zeroFlows)
    result shouldBe Right(())
  }

  it should "pass when NFA change matches CA + valuation" in {
    val ca        = PLN(50000.0)
    val valuation = PLN(3000.0)
    val prev      = baseSnap.copy(nfa = PLN(100000.0))
    val curr      = baseSnap.copy(nfa = PLN(100000.0) + ca + valuation)
    val flows     = zeroFlows.copy(currentAccount = ca, valuationEffect = valuation)
    val result    = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "detect error when NFA doesn't match CA + valuation" in {
    val prev   = baseSnap.copy(nfa = PLN(100000.0))
    // Bug: NFA unchanged despite positive CA
    val curr   = baseSnap.copy(nfa = PLN(100000.0))
    val flows  = zeroFlows.copy(currentAccount = PLN(25000.0), valuationEffect = PLN(0.0))
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe a[Left[?, ?]]
    errorDelta(result, Sfc.SfcIdentity.Nfa) shouldBe -25000.0 +- 0.01
  }

  it should "handle negative NFA changes (capital outflow)" in {
    val prev   = baseSnap.copy(nfa = PLN(100000.0))
    val curr   = baseSnap.copy(nfa = PLN(70000.0))
    val flows  = zeroFlows.copy(currentAccount = PLN(-30000.0), valuationEffect = PLN(0.0))
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "include valuation effects in NFA check" in {
    val prev   = baseSnap.copy(nfa = PLN(100000.0))
    // CA=-10000, valuation=+15000 → expected ΔNFA = +5000
    val curr   = baseSnap.copy(nfa = PLN(105000.0))
    val flows  = zeroFlows.copy(currentAccount = PLN(-10000.0), valuationEffect = PLN(15000.0))
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "fail 4th identity without breaking other 3 identities" in {
    val prev   = baseSnap.copy(nfa = PLN(100000.0))
    // NFA jumps by 50000 but CA+valuation = 0 → error on identity 4 only
    val curr   = baseSnap.copy(nfa = PLN(150000.0))
    val result = Sfc.validate(prev, curr, zeroFlows)
    result shouldBe a[Left[?, ?]]
    result.swap.getOrElse(Vector.empty).map(_.identity) should contain only Sfc.SfcIdentity.Nfa
    errorDelta(result, Sfc.SfcIdentity.Nfa) shouldBe 50000.0 +- 0.01
  }
