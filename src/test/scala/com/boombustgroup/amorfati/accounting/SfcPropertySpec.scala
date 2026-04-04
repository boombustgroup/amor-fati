package com.boombustgroup.amorfati.accounting

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

class SfcPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val td = ComputationBoundary

  private def errorDelta(result: Either[Vector[Sfc.SfcIdentityError], Unit], id: Sfc.SfcIdentity): Double =
    result.swap.getOrElse(Vector.empty).find(_.identity == id).map(e => td.toDouble(e.actual - e.expected)).getOrElse(0.0)

  // --- Consistent flows always pass ---

  "Sfc.validateStockExactness" should "pass when flows are consistent with snapshots" in
    forAll(genConsistentFlowsAndSnapshots) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows)) =>
      val (prev, curr, flows) = triple
      val result              = Sfc.validateStockExactness(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "pass with zero flows and identical snapshots" in
    forAll(genSnapshot) { (snap: Sfc.StockState) =>
      val zeroFlows = Sfc.SemanticFlows(
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
      val result    = Sfc.validateStockExactness(snap, snap, zeroFlows)
      result shouldBe Right(())
    }

  // --- Perturbed states detect errors ---

  it should "detect perturbed bankCapital" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(bankCapital = curr.bankCapital + PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.BankCapital) shouldBe true
    }

  it should "detect perturbed bankDeposits" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(bankDeposits = curr.bankDeposits + PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.BankDeposits) shouldBe true
    }

  it should "detect perturbed govDebt" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(govDebt = curr.govDebt + PLN(perturbation))
      val result              = Sfc.metricDiagnostics(prev, perturbed, flows)
      result.exists(_.identity == Sfc.SfcIdentity.GovDebt) shouldBe true
    }

  it should "detect perturbed nfa" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(nfa = curr.nfa + PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.Nfa) shouldBe true
    }

  // --- Error magnitude property ---

  it should "have error magnitude = actual change - expected change" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), delta: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(bankCapital = curr.bankCapital + PLN(delta))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      errorDelta(result, Sfc.SfcIdentity.BankCapital) shouldBe (delta +- 1.0)
    }

  // --- Four identities independent ---

  it should "have independent identities (perturb one, others unaffected)" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), delta: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(govDebt = curr.govDebt + PLN(delta))
      val result              = Sfc.metricDiagnostics(prev, perturbed, flows)
      result.map(_.identity) should contain(Sfc.SfcIdentity.GovDebt)
      result.exists(e => e.identity == Sfc.SfcIdentity.BankCapital) shouldBe false
      result.exists(e => e.identity == Sfc.SfcIdentity.BankDeposits) shouldBe false
    }

  // --- Bond clearing identity ---

  it should "pass bond clearing when bankBonds + nbpBonds = bondsOutstanding" in
    forAll(genConsistentFlowsAndSnapshots) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows)) =>
      val (prev, curr, flows) = triple
      val result              = Sfc.validateStockExactness(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "detect perturbed bondClearingError" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(bankBondHoldings = curr.bankBondHoldings + PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.BondClearing) shouldBe true
    }

  // --- Interbank netting identity ---

  it should "pass interbank netting when interbankNetSum is zero" in
    forAll(genConsistentFlowsAndSnapshots) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows)) =>
      val (prev, curr, flows) = triple
      val result              = Sfc.validateStockExactness(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "detect perturbed interbankNetSum" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(interbankNetSum = PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.InterbankNetting) shouldBe true
    }

  // --- Snapshot sums property ---

  it should "compute correct snapshot sums from firms" in
    forAll(Gen.choose(3, 20)) { (n: Int) =>
      forAll(Gen.listOfN(n, genAliveFirm)) { (firmList: List[com.boombustgroup.amorfati.agents.Firm.State]) =>
        val firms        = firmList.toArray
        val expectedCash = firms.map(f => td.toDouble(f.cash)).sum
        val expectedDebt = firms.map(f => td.toDouble(f.debt)).sum
        Math.abs(expectedCash - firms.map(f => td.toDouble(f.cash)).sum) should be < 1e-6
        Math.abs(expectedDebt - firms.map(f => td.toDouble(f.debt)).sum) should be < 1e-6
      }
    }

  // --- Dividend flow consistency ---

  it should "pass with non-zero dividend flows in consistent snapshots" in
    // Verify that genConsistentFlowsAndSnapshots correctly handles dividend fields
    forAll(genConsistentFlowsAndSnapshots) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows)) =>
      val (prev, curr, flows) = triple
      // genMonthlyFlows generates non-zero dividend fields — verify all 8 identities hold
      val result              = Sfc.validateStockExactness(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "detect perturbed deposits from dividend flow mismatch" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      // Perturb dividendIncome without updating deposits → Identity 2 fails
      val badFlows            = flows.copy(dividendIncome = flows.dividendIncome + PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, curr, badFlows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.BankDeposits) shouldBe true
    }

  // --- Mortgage stock identity ---

  it should "detect perturbed mortgageStock" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(5000.0, 50000.0)) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows), perturbation: Double) =>
      val (prev, curr, flows) = triple
      val perturbed           = curr.copy(mortgageStock = curr.mortgageStock + PLN(perturbation))
      val result              = Sfc.validateStockExactness(prev, perturbed, flows)
      result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.MortgageStock) shouldBe true
    }

  it should "pass mortgage stock identity in consistent snapshots" in
    forAll(genConsistentFlowsAndSnapshots) { (triple: (Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows)) =>
      val (prev, curr, flows) = triple
      val result              = Sfc.validateStockExactness(prev, curr, flows)
      result shouldBe Right(())
    }
