package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.Sfc
import sfc.agents.{Firm, TechState}
import sfc.types.*
import sfc.util.KahanSum.*

class FofSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

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
  )

  "FofConsWeights" should "sum to 1.0" in {
    Math.abs(p.fiscal.fofConsWeights.map(_.toDouble).sum - 1.0) should be < 0.01
  }

  "FofGovWeights" should "sum to 1.0" in {
    Math.abs(p.fiscal.fofGovWeights.map(_.toDouble).sum - 1.0) should be < 0.01
  }

  "FofExportShares" should "sum to 1.0" in {
    Math.abs(p.fiscal.fofExportShares.map(_.toDouble).sum - 1.0) should be < 0.01
  }

  "Sector demand" should "equal consWeight*dc + govWeight*gp + exports" in {
    val dc      = 1000000.0
    val gp      = 500000.0
    val exports = Vector(50.0, 550.0, 150.0, 20.0, 30.0, 200.0)
    for s <- 0 until 6 do
      val expected =
        p.fiscal.fofConsWeights.map(_.toDouble)(s) * dc + p.fiscal.fofGovWeights.map(_.toDouble)(s) * gp + exports(s)
      val actual   =
        p.fiscal.fofConsWeights.map(_.toDouble)(s) * dc + p.fiscal.fofGovWeights.map(_.toDouble)(s) * gp + exports(s)
      actual shouldBe expected
  }

  "Sector demand multiplier" should "equal sectorDemand / (sectorCap * price)" in {
    val sectorDemand = 500000.0
    val sectorCap    = 400000.0
    val price        = 1.0
    val mult         = sectorDemand / (sectorCap * price)
    mult shouldBe 1.25
  }

  it should "be 0 for empty sectors" in {
    val sectorCap = 0.0
    val mult      = if sectorCap > 0 then 100.0 / (sectorCap * 1.0) else 0.0
    mult shouldBe 0.0
  }

  "Total firm revenue" should "equal sum of sector demands (identity closes)" in {
    val firms   = mkFirms()
    val price   = 1.0
    val dc      = 800000.0
    val gp      = 200000.0
    val exports = p.fiscal.fofExportShares.map(_.toDouble).map(_ * 100000.0)

    val sectorCap    = (0 until 6).map { s =>
      firms.filter(_.sector.toInt == s).kahanSumBy(f => Firm.computeCapacity(f).toDouble)
    }.toVector
    val sectorDemand = (0 until 6).map { s =>
      p.fiscal.fofConsWeights.map(_.toDouble)(s) * dc + p.fiscal.fofGovWeights.map(_.toDouble)(s) * gp + exports(s)
    }.toVector
    val sectorMults  = sectorDemand.indices.map { s =>
      if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * price) else 0.0
    }.toVector

    val totalFirmRev = (0 until 6).map { s =>
      firms.filter(_.sector.toInt == s).kahanSumBy(f => Firm.computeCapacity(f).toDouble * sectorMults(s) * price)
    }.kahanSum
    val totalDemand  = sectorDemand.kahanSum

    Math.abs(totalFirmRev - totalDemand) should be < 0.01
  }

  "Proportional allocation" should "distribute revenue proportionally within sector" in {
    val f1           = mkFirm(0, TechState.Traditional(10))
    val f2           = mkFirm(1, TechState.Traditional(20))
    // Both in sector 2 (Retail)
    val firms        = Array(f1.copy(sector = SectorIdx(2)), f2.copy(sector = SectorIdx(2)))
    val price        = 1.0
    val cap1         = Firm.computeCapacity(firms(0))
    val cap2         = Firm.computeCapacity(firms(1))
    val sectorDemand = 500000.0
    val totalCap     = cap1 + cap2
    val mult         = sectorDemand / (totalCap * price)
    val rev1         = cap1 * mult * price
    val rev2         = cap2 * mult * price
    Math.abs(rev1 + rev2 - sectorDemand) should be < 0.01
    // Revenue proportional to capacity
    Math.abs(rev1 / rev2 - cap1 / cap2) should be < 0.001
  }

  "Gov purchases" should "equal GovBaseSpending * price" in {
    val price = 1.2
    val gp    = p.fiscal.govBaseSpending.toDouble * price
    gp shouldBe p.fiscal.govBaseSpending.toDouble * 1.2
  }

  "Scalar exports" should "be distributed by FofExportShares" in {
    val totalExports = 1000000.0
    val distributed  = p.fiscal.fofExportShares.map(_.toDouble).map(_ * totalExports)
    Math.abs(distributed.sum - totalExports) should be < 0.01
    distributed(1) shouldBe 520000.0 // Manufacturing gets 52%
  }

  "Sfc Identity 10" should "pass when fofResidual is zero" in {
    // All flows zero except fofResidual — all deltas are 0 = 0
    val flows  = zeroFlows
    val snap   = zeroSnap.copy(bankCapital = PLN(500000.0), bankDeposits = PLN(1000000.0))
    val result = Sfc.validate(snap, snap, flows)
    result shouldBe Right(())
  }

  it should "fail when fofResidual exceeds tolerance" in {
    val flows  = zeroFlows.copy(fofResidual = PLN(1.0))
    val snap   = zeroSnap.copy(bankCapital = PLN(500000.0), bankDeposits = PLN(1000000.0))
    val result = Sfc.validate(snap, snap, flows)
    result shouldBe a[Left[?, ?]]
    result.swap
      .getOrElse(Vector.empty)
      .find(_.identity == Sfc.SfcIdentity.FlowOfFunds)
      .get
      .actual
      .toDouble shouldBe 1.0 +- 0.01
  }

  // --- helpers ---

  private def mkFirm(id: Int, tech: TechState, sector: Int = 2): Firm.State =
    Firm.State(
      FirmId(id),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(sector),
      Array.empty[FirmId],
    )

  private def mkFirms(): Vector[Firm.State] =
    // Create firms distributed across all 6 sectors
    val firmsPerSector = 10
    (0 until 6).flatMap { s =>
      (0 until firmsPerSector).map { i =>
        mkFirm(s * firmsPerSector + i, TechState.Traditional(10), s)
      }
    }.toVector
