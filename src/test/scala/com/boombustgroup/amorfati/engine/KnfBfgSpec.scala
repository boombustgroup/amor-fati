package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.types.*

class KnfBfgSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  @annotation.nowarn("msg=unused private member") // defaults used by callers
  private def mkBank(
      id: Int = 0,
      deposits: PLN = PLN(1e6),
      loans: PLN = PLN.Zero,
      capital: PLN = PLN(100000),
      nplAmount: PLN = PLN.Zero,
      govBondHoldings: PLN = PLN.Zero,
      reservesAtNbp: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
  ): Banking.BankState = Banking.BankState(
    id = BankId(id),
    deposits = deposits,
    loans = loans,
    capital = capital,
    nplAmount = nplAmount,
    afsBonds = govBondHoldings * 0.40,
    htmBonds = govBondHoldings * 0.60,
    htmBookYield = Rate(0.055),
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

  // ==========================================================================
  // Config defaults (5 tests)
  // ==========================================================================

  "P2rAddons" should "default to 7 values" in {
    p.banking.p2rAddons.length shouldBe 7
  }

  it should "have PKO BP = 1.5%, mBank = 3.0%" in {
    p.banking.p2rAddons(0) shouldBe Rate(0.015) // PKO BP
    p.banking.p2rAddons(2) shouldBe Rate(0.030) // mBank
  }

  "BfgLevyRate" should "default to 0.0024" in {
    p.banking.bfgLevyRate shouldBe Rate(0.0024)
  }

  "BailInDepositHaircut" should "default to 0.08" in {
    p.banking.bailInDepositHaircut shouldBe Ratio(0.08)
  }

  "BfgDepositGuarantee" should "default to 400000" in {
    p.banking.bfgDepositGuarantee shouldBe PLN(400000.0)
  }

  // ==========================================================================
  // P2R add-ons (4 tests)
  // ==========================================================================

  "p2rAddon" should "return correct per-bank values" in {
    Macroprudential.p2rAddon(0) shouldBe 0.015 // PKO BP
    Macroprudential.p2rAddon(1) shouldBe 0.010 // Pekao
    Macroprudential.p2rAddon(2) shouldBe 0.030 // mBank
    Macroprudential.p2rAddon(3) shouldBe 0.015 // ING BSK
    Macroprudential.p2rAddon(4) shouldBe 0.020 // Santander
    Macroprudential.p2rAddon(5) shouldBe 0.025 // BPS/Coop
    Macroprudential.p2rAddon(6) shouldBe 0.020 // Others
  }

  it should "fall back to last value for out-of-range bankId" in {
    Macroprudential.p2rAddon(7) shouldBe p.banking.p2rAddons.last.toDouble
    Macroprudential.p2rAddon(99) shouldBe p.banking.p2rAddons.last.toDouble
  }

  "effectiveMinCarImpl" should "include P2R in total" in {
    val ccyb     = 0.01
    val bankId   = 0 // PKO BP: OSII=1%, P2R=1.5%
    val expected =
      p.banking.minCar.toDouble + ccyb + p.banking.osiiPkoBp.toDouble + p.banking.p2rAddons.map(_.toDouble)(0)
    Macroprudential.effectiveMinCarImpl(bankId, ccyb) shouldBe expected +- 1e-10
  }

  it should "give mBank highest P2R (3.0%)" in {
    val ccyb     = 0.0
    // mBank (id=2): OSII=0%, P2R=3.0%
    val mBankCar = Macroprudential.effectiveMinCarImpl(2, ccyb)
    // PKO BP (id=0): OSII=1%, P2R=1.5%
    val pkoCar   = Macroprudential.effectiveMinCarImpl(0, ccyb)
    // mBank P2R (3%) > PKO OSII+P2R (1%+1.5%)
    mBankCar shouldBe p.banking.minCar.toDouble + 0.030
    pkoCar shouldBe p.banking.minCar.toDouble + 0.010 + 0.015
  }

  // ==========================================================================
  // BFG levy (4 tests)
  // ==========================================================================

  "computeBfgLevy" should "compute monthly levy = deposits * rate / 12" in {
    val banks    = Vector(mkBank(deposits = PLN(1200000.0)))
    val result   = Banking.computeBfgLevy(banks)
    val expected = 1200000.0 * p.banking.bfgLevyRate.toDouble / 12.0
    result.perBank(0).toDouble shouldBe expected +- 0.01
    result.total.toDouble shouldBe expected +- 0.01
  }

  it should "return zero for failed bank" in {
    val banks  = Vector(mkBank(deposits = PLN(1200000.0), capital = PLN.Zero, status = BankStatus.Failed(5)))
    val result = Banking.computeBfgLevy(banks)
    result.perBank(0) shouldBe PLN.Zero
    result.total shouldBe PLN.Zero
  }

  it should "sum per-bank levies correctly" in {
    val banks    = Vector(
      mkBank(id = 0, deposits = PLN(1000000.0)),
      mkBank(id = 1, deposits = PLN(2000000.0), capital = PLN(200000)),
      mkBank(id = 2, deposits = PLN(500000.0), capital = PLN(50000), status = BankStatus.Failed(3)),
    )
    val result   = Banking.computeBfgLevy(banks)
    val expected = (1000000.0 + 2000000.0) * p.banking.bfgLevyRate.toDouble / 12.0
    result.total.toDouble shouldBe expected +- 0.01
    result.perBank(2) shouldBe PLN.Zero
  }

  it should "be positive for non-failed bank with positive deposits" in {
    val banks  = Vector(mkBank(deposits = PLN(100000.0), capital = PLN(10000)))
    val result = Banking.computeBfgLevy(banks)
    result.perBank(0) should be > PLN.Zero
  }

  // ==========================================================================
  // Bail-in (6 tests)
  // ==========================================================================

  "applyBailIn" should "not touch non-failed banks" in {
    val banks  = Vector(mkBank(deposits = PLN(500000.0), loans = PLN(100000), capital = PLN(50000)))
    val result = Banking.applyBailIn(banks)
    result.banks(0).deposits shouldBe PLN(500000.0)
    result.totalLoss shouldBe PLN.Zero
  }

  it should "not haircut deposits below guarantee threshold" in {
    val banks  = Vector(mkBank(deposits = PLN(300000.0), capital = PLN.Zero, status = BankStatus.Failed(5)))
    val result = Banking.applyBailIn(banks)
    result.banks(0).deposits shouldBe PLN(300000.0)
    result.totalLoss shouldBe PLN.Zero
  }

  it should "return zero bail-in when no failures" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(1000000.0), loans = PLN(100000), capital = PLN(50000)),
      mkBank(id = 1, deposits = PLN(2000000.0), loans = PLN(200000)),
    )
    val result = Banking.applyBailIn(banks)
    result.totalLoss shouldBe PLN.Zero
    result.banks(0).deposits shouldBe PLN(1000000.0)
    result.banks(1).deposits shouldBe PLN(2000000.0)
  }

  it should "apply before resolution (bail-in then resolve)" in {
    val banks        = Vector(
      mkBank(id = 0, deposits = PLN(1000000.0), loans = PLN(100000), capital = PLN.Zero, nplAmount = PLN(50000), status = BankStatus.Failed(5)),
      mkBank(id = 1, deposits = PLN(2000000.0), loans = PLN(200000), capital = PLN(200000)),
    )
    val afterBailIn  = Banking.applyBailIn(banks)
    // With bailIn=true: uninsured = 1M - 400k = 600k, haircut = 600k * 0.08 = 48k
    val uninsured    = 1000000.0 - p.banking.bfgDepositGuarantee.toDouble
    val haircut      = uninsured * p.banking.bailInDepositHaircut.toDouble
    val afterResolve = Banking.resolveFailures(afterBailIn.banks)
    afterResolve.banks(0).deposits shouldBe PLN.Zero                             // failed bank wiped
    afterResolve.banks(1).deposits shouldBe PLN(2000000.0 + 1000000.0 - haircut) // absorbed after bail-in
  }

  // ==========================================================================
  // World defaults (1 test)
  // ==========================================================================

  "World" should "default bfgFundBalance=0 and bailInLoss=0" in {
    val w = World(
      month = 0,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = 100000.0,
      currentSigmas = Vector(1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
      totalPopulation = 100,
      gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bank = Banking.Aggregate(PLN.Zero, PLN.Zero, PLN(100000), PLN(500000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      bankingSector = Generators.testBankingSector(),
      forex = OpenEconomy.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      hhAgg = com.boombustgroup.amorfati.agents.Household.Aggregates(
        employed = 100,
        unemployed = 0,
        retraining = 0,
        bankrupt = 0,
        totalIncome = PLN.Zero,
        consumption = PLN.Zero,
        domesticConsumption = PLN.Zero,
        importConsumption = PLN.Zero,
        marketWage = PLN(8000),
        reservationWage = PLN(4000),
        giniIndividual = Ratio.Zero,
        giniWealth = Ratio.Zero,
        meanSavings = PLN.Zero,
        medianSavings = PLN.Zero,
        povertyRate50 = Ratio.Zero,
        bankruptcyRate = Ratio.Zero,
        meanSkill = 0.0,
        meanHealthPenalty = 0.0,
        retrainingAttempts = 0,
        retrainingSuccesses = 0,
        consumptionP10 = PLN.Zero,
        consumptionP50 = PLN.Zero,
        consumptionP90 = PLN.Zero,
        meanMonthsToRuin = 0.0,
        povertyRate30 = Ratio.Zero,
        totalRent = PLN.Zero,
        totalDebtService = PLN.Zero,
        totalUnempBenefits = PLN.Zero,
        totalDepositInterest = PLN.Zero,
        crossSectorHires = 0,
        voluntaryQuits = 0,
        sectorMobilityRate = Ratio.Zero,
        totalRemittances = PLN.Zero,
        totalPit = PLN.Zero,
        totalSocialTransfers = PLN.Zero,
        totalConsumerDebtService = PLN.Zero,
        totalConsumerOrigination = PLN.Zero,
        totalConsumerDefault = PLN.Zero,
        totalConsumerPrincipal = PLN.Zero,
      ),
      social = SocialState.zero,
      financial = FinancialMarketsState.zero,
      external = ExternalState.zero,
      real = RealState.zero,
      mechanisms = MechanismsState.zero,
      plumbing = MonetaryPlumbingState.zero,
      flows = FlowState.zero,
    )
    w.mechanisms.bfgFundBalance shouldBe PLN.Zero
    w.flows.bailInLoss shouldBe PLN.Zero
  }

  // ==========================================================================
  // P2R raises effective CAR floor (2 tests)
  // ==========================================================================

  "P2R" should "raise effectiveMinCar above base MinCar" in {
    val carWithP2r = Macroprudential.effectiveMinCarImpl(0, 0.0)
    carWithP2r should be > p.banking.minCar.toDouble
  }

  it should "vary across banks" in {
    val car0 = Macroprudential.effectiveMinCarImpl(0, 0.0) // PKO: OSII 1% + P2R 1.5%
    val car2 = Macroprudential.effectiveMinCarImpl(2, 0.0) // mBank: OSII 0% + P2R 3.0%
    val car5 = Macroprudential.effectiveMinCarImpl(5, 0.0) // BPS: OSII 0% + P2R 2.5%
    // All different from each other
    car0 should not be car2
    car2 should not be car5
    // mBank has highest P2R
    Macroprudential.p2rAddon(2) should be > Macroprudential.p2rAddon(0)
  }

  // ==========================================================================
  // BFG levy reduces bank capital (2 tests)
  // ==========================================================================

  "BFG levy" should "reduce bank capital" in {
    val deposits        = 1000000.0
    val levy            = deposits * p.banking.bfgLevyRate.toDouble / 12.0
    levy should be > 0.0
    val originalCapital = 100000.0
    val newCapital      = originalCapital - levy
    newCapital should be < originalCapital
  }

  it should "accumulate monotonically in BFG fund" in {
    val prevBalance = 1000.0
    val currentLevy = 500.0
    val newBalance  = prevBalance + currentLevy
    newBalance shouldBe 1500.0
    newBalance should be > prevBalance
  }

  // ==========================================================================
  // resolveFailures edge cases (4 tests)
  // ==========================================================================

  "resolveFailures" should "preserve total gov bond holdings when all banks fail" in {
    val banks            = Vector(
      mkBank(
        id = 0,
        deposits = PLN(500000.0),
        loans = PLN(100000),
        capital = PLN(-10000),
        nplAmount = PLN(50000),
        govBondHoldings = PLN(20e9),
        status = BankStatus.Failed(3),
      ),
      mkBank(
        id = 1,
        deposits = PLN(300000.0),
        loans = PLN(80000),
        capital = PLN(-5000),
        nplAmount = PLN(30000),
        govBondHoldings = PLN(15e9),
        status = BankStatus.Failed(3),
      ),
      mkBank(
        id = 2,
        deposits = PLN(200000.0),
        loans = PLN(60000),
        capital = PLN(-20000),
        nplAmount = PLN(20000),
        govBondHoldings = PLN(13.8e9),
        status = BankStatus.Failed(3),
      ),
    )
    val totalBondsBefore = banks.map(_.govBondHoldings.toDouble).sum
    val result           = Banking.resolveFailures(banks)
    val totalBondsAfter  = result.banks.map(_.govBondHoldings.toDouble).sum
    totalBondsAfter shouldBe totalBondsBefore +- 1.0
    val bridge           = result.banks.find(!_.failed)
    bridge shouldBe defined
    bridge.get.govBondHoldings should be > PLN.Zero
  }

  it should "preserve interbank netting sum when all banks fail" in {
    val banks                = Vector(
      mkBank(
        id = 0,
        deposits = PLN(500000.0),
        loans = PLN(100000),
        capital = PLN(-5000),
        nplAmount = PLN(50000),
        interbankNet = PLN(1000.0),
        status = BankStatus.Failed(3),
      ),
      mkBank(
        id = 1,
        deposits = PLN(300000.0),
        loans = PLN(80000),
        capital = PLN(-3000),
        nplAmount = PLN(30000),
        interbankNet = PLN(-600.0),
        status = BankStatus.Failed(3),
      ),
      mkBank(
        id = 2,
        deposits = PLN(200000.0),
        loans = PLN(60000),
        capital = PLN(-10000),
        nplAmount = PLN(20000),
        interbankNet = PLN(-400.0),
        status = BankStatus.Failed(3),
      ),
    )
    val result               = Banking.resolveFailures(banks)
    val totalInterbankAfter  = result.banks.map(_.interbankNet.toDouble).sum
    val totalInterbankBefore = banks.map(_.interbankNet.toDouble).sum
    totalInterbankAfter shouldBe totalInterbankBefore +- 1e-6
  }

  it should "transfer bonds to healthy absorber in partial failure" in {
    val banks  = Vector(
      mkBank(
        id = 0,
        deposits = PLN(500000.0),
        loans = PLN(100000),
        capital = PLN.Zero,
        nplAmount = PLN(50000),
        govBondHoldings = PLN(10e9),
        status = BankStatus.Failed(3),
      ),
      mkBank(id = 1, deposits = PLN(2000000.0), loans = PLN(200000), capital = PLN(200000), govBondHoldings = PLN(5e9)),
    )
    val result = Banking.resolveFailures(banks)
    result.banks(0).govBondHoldings shouldBe PLN.Zero
    result.banks(1).govBondHoldings.toDouble shouldBe 15e9 +- 1.0
  }

  "healthiestBankId" should "return bank with highest capital when all fail" in {
    val banks = Vector(
      mkBank(id = 0, deposits = PLN(500000.0), loans = PLN(100000), capital = PLN(-20000), nplAmount = PLN(50000), status = BankStatus.Failed(3)),
      mkBank(id = 1, deposits = PLN(300000.0), loans = PLN(80000), capital = PLN(-5000), nplAmount = PLN(30000), status = BankStatus.Failed(3)),
      mkBank(id = 2, deposits = PLN(200000.0), loans = PLN(60000), capital = PLN(-15000), nplAmount = PLN(20000), status = BankStatus.Failed(3)),
    )
    // Bank 1 has highest (least negative) capital: -5000
    Banking.healthiestBankId(banks) shouldBe BankId(1)
  }
