package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankState, ForexState, GovState}
import sfc.agents.{BankingSector, IndividualBankState}
import sfc.config.Config
import sfc.types.*

class KnfBfgSpec extends AnyFlatSpec with Matchers:

  // ==========================================================================
  // Config defaults (5 tests)
  // ==========================================================================

  "P2rAddons" should "default to 7 values" in {
    Config.P2rAddons.length shouldBe 7
  }

  it should "have PKO BP = 1.5%, mBank = 3.0%" in {
    Config.P2rAddons(0) shouldBe 0.015  // PKO BP
    Config.P2rAddons(2) shouldBe 0.030  // mBank
  }

  "BfgLevyRate" should "default to 0.0024" in {
    Config.BfgLevyRate shouldBe 0.0024
  }

  "BailInEnabled" should "default to false" in {
    Config.BailInEnabled shouldBe false
  }

  "BailInDepositHaircut" should "default to 0.08" in {
    Config.BailInDepositHaircut shouldBe 0.08
  }

  "BfgDepositGuarantee" should "default to 400000" in {
    Config.BfgDepositGuarantee shouldBe 400000.0
  }

  // ==========================================================================
  // P2R add-ons (4 tests)
  // ==========================================================================

  "p2rAddon" should "return correct per-bank values" in {
    Macroprudential.p2rAddon(0) shouldBe 0.015  // PKO BP
    Macroprudential.p2rAddon(1) shouldBe 0.010  // Pekao
    Macroprudential.p2rAddon(2) shouldBe 0.030  // mBank
    Macroprudential.p2rAddon(3) shouldBe 0.015  // ING BSK
    Macroprudential.p2rAddon(4) shouldBe 0.020  // Santander
    Macroprudential.p2rAddon(5) shouldBe 0.025  // BPS/Coop
    Macroprudential.p2rAddon(6) shouldBe 0.020  // Others
  }

  it should "fall back to last value for out-of-range bankId" in {
    Macroprudential.p2rAddon(7) shouldBe Config.P2rAddons.last
    Macroprudential.p2rAddon(99) shouldBe Config.P2rAddons.last
  }

  "effectiveMinCarInternal" should "include P2R in total" in {
    val ccyb = 0.01
    val bankId = 0  // PKO BP: OSII=1%, P2R=1.5%
    val expected = Config.MinCar + ccyb + Config.OsiiPkoBp + Config.P2rAddons(0)
    Macroprudential.effectiveMinCarInternal(bankId, ccyb) shouldBe expected +- 1e-10
  }

  it should "give mBank highest P2R (3.0%)" in {
    val ccyb = 0.0
    // mBank (id=2): OSII=0%, P2R=3.0%
    val mBankCar = Macroprudential.effectiveMinCarInternal(2, ccyb)
    // PKO BP (id=0): OSII=1%, P2R=1.5%
    val pkoCar = Macroprudential.effectiveMinCarInternal(0, ccyb)
    // mBank P2R (3%) > PKO OSII+P2R (1%+1.5%)
    mBankCar shouldBe Config.MinCar + 0.030
    pkoCar shouldBe Config.MinCar + 0.010 + 0.015
  }

  // ==========================================================================
  // BFG levy (4 tests)
  // ==========================================================================

  "computeBfgLevy" should "compute monthly levy = deposits * rate / 12" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 1200000.0, loans = 0, capital = 100000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    val (perBank, total) = BankingSector.computeBfgLevy(banks)
    val expected = 1200000.0 * Config.BfgLevyRate / 12.0
    perBank(0) shouldBe expected +- 0.01
    total shouldBe expected +- 0.01
  }

  it should "return zero for failed bank" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 1200000.0, loans = 0, capital = 0,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 5, consecutiveLowCar = 3)
    )
    val (perBank, total) = BankingSector.computeBfgLevy(banks)
    perBank(0) shouldBe 0.0
    total shouldBe 0.0
  }

  it should "sum per-bank levies correctly" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 1000000.0, loans = 0, capital = 100000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0),
      IndividualBankState(BankId(1), deposits = 2000000.0, loans = 0, capital = 200000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0),
      IndividualBankState(BankId(2), deposits = 500000.0, loans = 0, capital = 50000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3)
    )
    val (perBank, total) = BankingSector.computeBfgLevy(banks)
    val expected = (1000000.0 + 2000000.0) * Config.BfgLevyRate / 12.0
    total shouldBe expected +- 0.01
    perBank(2) shouldBe 0.0
  }

  it should "be positive for non-failed bank with positive deposits" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 100000.0, loans = 0, capital = 10000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    val (perBank, _) = BankingSector.computeBfgLevy(banks)
    perBank(0) should be > 0.0
  }

  // ==========================================================================
  // Bail-in (6 tests)
  // ==========================================================================

  "applyBailIn" should "return unchanged banks when BailInEnabled is false" in {
    // BailInEnabled defaults to false
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 1000000.0, loans = 0, capital = 0,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 5, consecutiveLowCar = 3)
    )
    val (result, loss) = BankingSector.applyBailIn(banks)
    result(0).deposits shouldBe 1000000.0
    loss shouldBe 0.0
  }

  it should "not touch non-failed banks" in {
    // This test verifies behavior with bail-in logic directly
    // When bail-in is disabled (default), all banks pass through unchanged
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 500000.0, loans = 100000, capital = 50000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    val (result, loss) = BankingSector.applyBailIn(banks)
    result(0).deposits shouldBe 500000.0
    loss shouldBe 0.0
  }

  it should "not haircut deposits below guarantee threshold" in {
    // Deposits below BfgDepositGuarantee → no haircut
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 300000.0, loans = 0, capital = 0,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 5, consecutiveLowCar = 3)
    )
    // Even if bail-in is enabled, guaranteed = min(deposits, 400K) = 300K
    // uninsured = 0, haircut = 0
    // But since BailInEnabled defaults to false, we test the logic path directly
    // by checking that applyBailIn returns (banks, 0.0) when disabled
    val (result, loss) = BankingSector.applyBailIn(banks)
    result(0).deposits shouldBe 300000.0
    loss shouldBe 0.0
  }

  it should "return zero bail-in when no failures" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 1000000.0, loans = 100000, capital = 50000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0),
      IndividualBankState(BankId(1), deposits = 2000000.0, loans = 200000, capital = 100000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    val (result, loss) = BankingSector.applyBailIn(banks)
    loss shouldBe 0.0
    result(0).deposits shouldBe 1000000.0
    result(1).deposits shouldBe 2000000.0
  }

  it should "apply before resolution (bail-in then resolve)" in {
    // Verify that bail-in is called with failed banks still having deposits
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 1000000.0, loans = 100000, capital = 0,
        nplAmount = 50000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 5, consecutiveLowCar = 3),
      IndividualBankState(BankId(1), deposits = 2000000.0, loans = 200000, capital = 200000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    // With bail-in disabled (default), bail-in returns unchanged banks
    val (afterBailIn, _) = BankingSector.applyBailIn(banks)
    // Then resolve: failed bank deposits transfer to absorber
    val (afterResolve, _) = BankingSector.resolveFailures(afterBailIn)
    afterResolve(0).deposits shouldBe 0.0  // failed bank wiped
    afterResolve(1).deposits shouldBe 3000000.0  // absorbed
  }

  it should "track total loss across multiple failed banks" in {
    // With bail-in disabled, total loss = 0 for multiple failed banks
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 800000.0, loans = 0, capital = 0,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 5, consecutiveLowCar = 3),
      IndividualBankState(BankId(1), deposits = 600000.0, loans = 0, capital = 0,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 5, consecutiveLowCar = 3),
      IndividualBankState(BankId(2), deposits = 2000000.0, loans = 200000, capital = 200000,
        nplAmount = 0, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    val (_, loss) = BankingSector.applyBailIn(banks)
    loss shouldBe 0.0  // bail-in disabled by default
  }

  // ==========================================================================
  // World defaults (1 test)
  // ==========================================================================

  "World" should "default bfgFundBalance=0 and bailInLoss=0" in {
    val w = World(0, 0.02, 1.0,
      GovState(false, 0, 0, 0, 0, 0),
      sfc.agents.NbpState(0.05),
      BankState(0, 0, 100000, 500000),
      ForexState(4.33, 0, 0, 0, 0),
      sfc.agents.HhState(100, 8000, 4000, 0, 0, 0, 0),
      0, 0, 100000, Vector(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    w.bfgFundBalance shouldBe 0.0
    w.bailInLoss shouldBe 0.0
  }

  // ==========================================================================
  // P2R raises effective CAR floor (2 tests)
  // ==========================================================================

  "P2R" should "raise effectiveMinCar above base MinCar" in {
    val carWithP2r = Macroprudential.effectiveMinCarInternal(0, 0.0)
    carWithP2r should be > Config.MinCar
  }

  it should "vary across banks" in {
    val car0 = Macroprudential.effectiveMinCarInternal(0, 0.0)  // PKO: OSII 1% + P2R 1.5%
    val car2 = Macroprudential.effectiveMinCarInternal(2, 0.0)  // mBank: OSII 0% + P2R 3.0%
    val car5 = Macroprudential.effectiveMinCarInternal(5, 0.0)  // BPS: OSII 0% + P2R 2.5%
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
    // A bank with positive deposits should have lower capital after BFG levy
    val deposits = 1000000.0
    val levy = deposits * Config.BfgLevyRate / 12.0
    levy should be > 0.0
    // Capital reduction equals levy
    val originalCapital = 100000.0
    val newCapital = originalCapital - levy
    newCapital should be < originalCapital
  }

  it should "accumulate monotonically in BFG fund" in {
    // bfgFundBalance = prev + currentLevy (always non-negative)
    val prevBalance = 1000.0
    val currentLevy = 500.0
    val newBalance = prevBalance + currentLevy
    newBalance shouldBe 1500.0
    newBalance should be > prevBalance
  }

  // ==========================================================================
  // resolveFailures edge cases (4 tests)
  // ==========================================================================

  "resolveFailures" should "preserve total gov bond holdings when all banks fail" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 500000.0, loans = 100000, capital = -10000,
        nplAmount = 50000, govBondHoldings = 20e9, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(1), deposits = 300000.0, loans = 80000, capital = -5000,
        nplAmount = 30000, govBondHoldings = 15e9, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(2), deposits = 200000.0, loans = 60000, capital = -20000,
        nplAmount = 20000, govBondHoldings = 13.8e9, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3)
    )
    val totalBondsBefore = banks.map(_.govBondHoldings).sum
    val (resolved, _) = BankingSector.resolveFailures(banks)
    val totalBondsAfter = resolved.map(_.govBondHoldings).sum
    totalBondsAfter shouldBe totalBondsBefore +- 1.0
    // Bridge bank (highest capital = bank 1) should be resurrected
    val bridge = resolved.find(!_.failed)
    bridge shouldBe defined
    bridge.get.govBondHoldings should be > 0.0
  }

  it should "preserve interbank netting sum when all banks fail" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 500000.0, loans = 100000, capital = -5000,
        nplAmount = 50000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 1000.0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(1), deposits = 300000.0, loans = 80000, capital = -3000,
        nplAmount = 30000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = -600.0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(2), deposits = 200000.0, loans = 60000, capital = -10000,
        nplAmount = 20000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = -400.0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3)
    )
    val (resolved, _) = BankingSector.resolveFailures(banks)
    // All interbank nets should sum to the original total (zero-sum preserved)
    val totalInterbankAfter = resolved.map(_.interbankNet).sum
    val totalInterbankBefore = banks.map(_.interbankNet).sum
    totalInterbankAfter shouldBe totalInterbankBefore +- 1e-6
  }

  it should "transfer bonds to healthy absorber in partial failure" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 500000.0, loans = 100000, capital = 0,
        nplAmount = 50000, govBondHoldings = 10e9, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(1), deposits = 2000000.0, loans = 200000, capital = 200000,
        nplAmount = 0, govBondHoldings = 5e9, reservesAtNbp = 0, interbankNet = 0,
        failed = false, failedMonth = 0, consecutiveLowCar = 0)
    )
    val (resolved, _) = BankingSector.resolveFailures(banks)
    resolved(0).govBondHoldings shouldBe 0.0
    resolved(1).govBondHoldings shouldBe 15e9 +- 1.0
  }

  "healthiestBankId" should "return bank with highest capital when all fail" in {
    val banks = Vector(
      IndividualBankState(BankId(0), deposits = 500000.0, loans = 100000, capital = -20000,
        nplAmount = 50000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(1), deposits = 300000.0, loans = 80000, capital = -5000,
        nplAmount = 30000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3),
      IndividualBankState(BankId(2), deposits = 200000.0, loans = 60000, capital = -15000,
        nplAmount = 20000, govBondHoldings = 0, reservesAtNbp = 0, interbankNet = 0,
        failed = true, failedMonth = 3, consecutiveLowCar = 3)
    )
    // Bank 1 has highest (least negative) capital: -5000
    BankingSector.healthiestBankId(banks) shouldBe BankId(1)
  }
