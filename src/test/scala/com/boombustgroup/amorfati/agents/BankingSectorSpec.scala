package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.Generators
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class BankingSectorSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val td      = ComputationBoundary
  private val configs = Banking.DefaultConfigs

  private def mkBank(
      id: Int = 0,
      deposits: PLN = PLN(1e6),
      loans: PLN = PLN(1e6),
      capital: PLN = PLN(2e5),
      nplAmount: PLN = PLN.Zero,
      govBondHoldings: PLN = PLN.Zero,
      reservesAtNbp: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
      demandDeposits: PLN = PLN.Zero,
      termDeposits: PLN = PLN.Zero,
      loansShort: PLN = PLN.Zero,
      loansMedium: PLN = PLN.Zero,
      loansLong: PLN = PLN.Zero,
      consumerLoans: PLN = PLN.Zero,
      consumerNpl: PLN = PLN.Zero,
  ): Banking.BankState = Banking.BankState(
    id = BankId(id),
    deposits = deposits,
    loans = loans,
    capital = capital,
    nplAmount = nplAmount,
    afsBonds = govBondHoldings * Share(0.40),
    htmBonds = govBondHoldings * Share(0.60),
    htmBookYield = Rate(0.055),
    reservesAtNbp = reservesAtNbp,
    interbankNet = interbankNet,
    status = status,
    demandDeposits = demandDeposits,
    termDeposits = termDeposits,
    loansShort = loansShort,
    loansMedium = loansMedium,
    loansLong = loansLong,
    consumerLoans = consumerLoans,
    consumerNpl = consumerNpl,
  )

  // ---- initialize ----

  "Banking.initialize" should "create 7 banks with correct deposit/capital shares" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    bs.banks.length shouldBe 7
    bs.banks.map(b => td.toDouble(b.deposits)).sum shouldBe 1000000.0 +- 0.01
    bs.banks.map(b => td.toDouble(b.capital)).sum shouldBe 100000.0 +- 0.01
  }

  it should "set all banks as not failed initially" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    bs.banks.forall(!_.failed) shouldBe true
  }

  it should "set deposits proportional to market share" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    td.toDouble(bs.banks(0).deposits) shouldBe (1000000.0 * 0.175) +- 0.01 // PKO BP
    td.toDouble(bs.banks(5).deposits) shouldBe (1000000.0 * 0.050) +- 0.01 // BPS/Coop
  }

  // ---- assignBank ----

  "Banking.assignBank" should "return valid bank index" in {
    val rng = RandomStream.seeded(42)
    for _ <- 0 until 100 do
      val bId = Banking.assignBank(SectorIdx(0), configs, rng)
      bId.toInt should be >= 0
      bId.toInt should be < 7
  }

  it should "favor BPS/Coop for agriculture firms" in {
    val rng         = RandomStream.seeded(42)
    val assignments = (0 until 10000).map(_ => Banking.assignBank(SectorIdx(5), configs, rng))
    val bpsCount    = assignments.count(_ == BankId(5))
    // BPS has 0.65 agriculture affinity vs others ~0.15, so it should get disproportionate share
    bpsCount.toDouble / 10000 should be > 0.10
  }

  // ---- lendingRate ----

  "Banking.lendingRate" should "return high spread for failed bank" in {
    val bank = mkBank(status = BankStatus.Failed(ExecutionMonth(30)))
    val rate = Banking.lendingRate(bank, configs(0), Rate(0.05), Rate.Zero, PLN.Zero)
    td.toDouble(rate) shouldBe (0.05 + 0.50) +- 0.001
  }

  it should "increase with NPL ratio" in {
    val bankLowNpl  = mkBank(nplAmount = PLN(1e4))
    val bankHighNpl = mkBank(nplAmount = PLN(2e5))
    val rateLow     = Banking.lendingRate(bankLowNpl, configs(0), Rate(0.05), Rate.Zero, PLN.Zero)
    val rateHigh    = Banking.lendingRate(bankHighNpl, configs(0), Rate(0.05), Rate.Zero, PLN.Zero)
    rateHigh should be > rateLow
  }

  it should "include bank-specific spread" in {
    val bank    = mkBank(nplAmount = PLN.Zero)
    val ratePko = Banking.lendingRate(bank, configs(0), Rate(0.05), Rate.Zero, PLN.Zero) // spread = -0.002
    val rateBps = Banking.lendingRate(bank, configs(5), Rate(0.05), Rate.Zero, PLN.Zero) // spread = +0.003
    rateBps should be > ratePko
  }

  // ---- canLend ----

  "Banking.canLend" should "return false for failed bank" in {
    val bank = mkBank(capital = PLN(1e5), status = BankStatus.Failed(ExecutionMonth(30)))
    Banking.canLend(bank, PLN(1000.0), RandomStream.seeded(42), Multiplier.Zero, PLN.Zero) shouldBe false
  }

  it should "reject when projected CAR too low" in {
    // capital=8000, loans=100000, existing CAR=0.08
    // Adding 10000 loan -> projected = 8000/110000 = 0.0727 < 0.08
    val bank    = mkBank(loans = PLN(100000.0), capital = PLN(8000.0))
    val rng     = RandomStream.seeded(42L)
    // Need to test multiple times since there's a stochastic element
    val results = (0 until 100).map(_ => Banking.canLend(bank, PLN(10000.0), rng, Multiplier.Zero, PLN.Zero))
    results.forall(_ == false) shouldBe true
  }

  // ---- interbankRate ----

  "Banking.interbankRate" should "return deposit rate when NPL is zero" in {
    val banks = Vector(
      mkBank(id = 0),
      mkBank(id = 1),
    )
    val rate  = Banking.interbankRate(banks, Rate(0.05))
    td.toDouble(rate) shouldBe (0.05 - 0.01) +- 0.001 // deposit rate
  }

  it should "approach lombard rate when NPL is high" in {
    val banks = Vector(
      mkBank(id = 0, nplAmount = PLN(1e5)), // 10% NPL
      mkBank(id = 1, nplAmount = PLN(1e5)),
    )
    val rate  = Banking.interbankRate(banks, Rate(0.05))
    // stress = 0.10 / 0.05 = 2.0, clipped to 1.0
    td.toDouble(rate) shouldBe (0.05 + 0.01) +- 0.001 // lombard rate
  }

  // ---- clearInterbank ----

  "Banking.clearInterbank" should "produce interbankNet that sums to zero" in {
    val banks   = Vector(
      mkBank(id = 0, loans = PLN(3e5), govBondHoldings = PLN(1e5)),
      mkBank(id = 1, deposits = PLN(5e5), loans = PLN(8e5), capital = PLN(1e5)),
      mkBank(id = 2, deposits = PLN(8e5), loans = PLN(2e5), capital = PLN(1.5e5), govBondHoldings = PLN(5e4)),
    )
    val cleared = Banking.clearInterbank(banks, configs.take(3))
    val netSum  = cleared.map(b => td.toDouble(b.interbankNet)).sum
    netSum shouldBe 0.0 +- 0.01
  }

  it should "produce exact zero interbankNet sum in raw PLN units" in {
    val banks   = Vector(
      mkBank(id = 0, deposits = PLN(900001.0), loans = PLN(100000.0), capital = PLN(1e5)),
      mkBank(id = 1, deposits = PLN(500000.0), loans = PLN(850000.0), capital = PLN(1e5)),
      mkBank(id = 2, deposits = PLN(700000.0), loans = PLN(250000.0), capital = PLN(1e5)),
      mkBank(id = 3, deposits = PLN(600000.0), loans = PLN(950000.0), capital = PLN(1e5)),
    )
    val cleared = Banking.clearInterbank(banks, configs.take(4))
    cleared.map(_.interbankNet.toLong).sum shouldBe 0L
  }

  it should "set failed banks' interbankNet to zero" in {
    val banks   = Vector(
      mkBank(id = 0, loans = PLN(3e5)),
      mkBank(id = 1, deposits = PLN(5e5), loans = PLN(8e5), capital = PLN(1e5), status = BankStatus.Failed(ExecutionMonth(30))),
    )
    val cleared = Banking.clearInterbank(banks, configs.take(2))
    cleared(1).interbankNet shouldBe PLN.Zero
  }

  // ---- checkFailures ----

  "Banking.checkFailures" should "not trigger when disabled" in {
    val banks = Vector(
      mkBank(capital = PLN(1000.0), status = BankStatus.Active(5)), // Very low CAR
    )
    val result = Banking.checkFailures(banks, ExecutionMonth(30), enabled = false, Multiplier.Zero)
    result.anyFailed shouldBe false
    result.banks(0).failed shouldBe false
  }

  it should "trigger after 3 consecutive months of low CAR" in {
    val bank   = mkBank(capital = PLN(1000.0), status = BankStatus.Active(2))
    val result = Banking.checkFailures(Vector(bank), ExecutionMonth(30), enabled = true, Multiplier.Zero)
    result.anyFailed shouldBe true
    result.banks(0).failed shouldBe true
    result.banks(0).capital shouldBe PLN.Zero // Shareholders wiped
  }

  it should "reset consecutive counter when CAR recovers" in {
    val bank   = mkBank(status = BankStatus.Active(2)) // CAR = 0.20 > MinCar
    val result = Banking.checkFailures(Vector(bank), ExecutionMonth(30), enabled = true, Multiplier.Zero)
    result.anyFailed shouldBe false
    result.banks(0).consecutiveLowCar shouldBe 0
  }

  // ---- resolveFailures ----

  "Banking.resolveFailures" should "transfer deposits to healthiest bank" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(500000.0), loans = PLN(100000.0), capital = PLN(50000.0), govBondHoldings = PLN(10000.0)),
      mkBank(
        id = 1,
        deposits = PLN(300000.0),
        loans = PLN(80000.0),
        capital = PLN(0.0),
        govBondHoldings = PLN(5000.0),
        status = BankStatus.Failed(ExecutionMonth(30)),
      ),
    )
    val result = Banking.resolveFailures(banks, Vector(PLN(1000.0), PLN(250.0)))
    td.toDouble(result.banks(0).deposits) shouldBe 800000.0 +- 0.01 // absorbed 300k
    result.banks(1).deposits shouldBe PLN.Zero
    result.bankCorpBondHoldings shouldBe Vector(PLN(1250.0), PLN.Zero)
  }

  // ---- bond allocation ----

  "Banking.allocateBondIssuance" should "distribute proportional to deposits" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(600000.0), loans = PLN.Zero, capital = PLN(1e5)),
      mkBank(id = 1, deposits = PLN(400000.0), loans = PLN.Zero, capital = PLN(1e5)),
    )
    val result = Banking.allocateBondIssuance(banks, PLN(10000.0), Rate(0.05))
    td.toDouble(result(0).govBondHoldings) shouldBe 6000.0 +- 0.01
    td.toDouble(result(1).govBondHoldings) shouldBe 4000.0 +- 0.01
  }

  "Banking.allocateBondRedemption" should "handle government surplus as explicit redemption" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(500000.0), loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(8000.0)),
      mkBank(id = 1, deposits = PLN(500000.0), loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(2000.0)),
    )
    val result = Banking.allocateBondRedemption(banks, PLN(4000.0), Rate(0.05))
    td.toDouble(result(0).govBondHoldings) shouldBe 6000.0 +- 0.01 // 8000 + (-4000 * 0.5)
    td.toDouble(result(1).govBondHoldings) shouldBe 0.0 +- 0.01    // 2000 + (-4000 * 0.5)
  }

  "Banking.allocateBondIssuance" should "have per-bank deltas summing to exactly issuance (residual-based)" in {
    // 7 banks with irrational deposit ratios -> FP rounding inevitable without residual
    val banks    = (0 until 7)
      .map(i =>
        mkBank(
          id = i,
          deposits = PLN(1e6 / 7.0 * (i + 1)),
          loans = PLN.Zero,
          capital = PLN(1e5),
          govBondHoldings = PLN(1000.0 * (i + 1)),
        ),
      )
      .toVector
    val issuance = PLN(123456.789)
    val result   = Banking.allocateBondIssuance(banks, issuance, Rate(0.05))
    val deltas   = result.zip(banks).map((a, b) => td.toDouble(a.govBondHoldings) - td.toDouble(b.govBondHoldings))
    deltas.sum shouldBe td.toDouble(issuance) +- 1e-6
  }

  it should "keep aggregate within tight tolerance with large issuance (1e13)" in {
    val banks    = Generators.testBankingSector(totalDeposits = PLN(1e9), totalCapital = PLN(1e8), totalLoans = PLN.Zero, configs = configs).banks
    val issuance = PLN(1e13)
    val before   = banks.map(b => td.toDouble(b.govBondHoldings)).sum
    val result   = Banking.allocateBondIssuance(banks, issuance, Rate(0.05))
    val after    = result.map(b => td.toDouble(b.govBondHoldings)).sum
    (after - before) shouldBe td.toDouble(issuance) +- 0.01 // well within SFC tolerance of 1.0
  }

  // ---- sellToBuyer ----

  "Banking.sellToBuyer" should "sell proportional to bond holdings" in {
    val banks  = Vector(
      mkBank(id = 0, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(6000.0)),
      mkBank(id = 1, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(4000.0)),
    )
    val result = Banking.sellToBuyer(banks, PLN(5000.0)).banks
    td.toDouble(result(0).govBondHoldings) shouldBe 3000.0 +- 0.01 // 6000 - 5000*0.6
    td.toDouble(result(1).govBondHoldings) shouldBe 2000.0 +- 0.01 // 4000 - 5000*0.4
  }

  it should "sell exact requested amount when rounded shares leave residual" in {
    val banks     = Vector(
      mkBank(id = 0, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(1000.0)),
      mkBank(id = 1, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(1000.0)),
      mkBank(id = 2, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(1000.0)),
    )
    val sale      = Banking.sellToBuyer(banks, PLN(2.0))
    sale.actualSold shouldBe PLN(2.0)
    val remaining = sale.banks.map(_.govBondHoldings.toLong).sum
    remaining shouldBe banks.map(_.govBondHoldings.toLong).sum - PLN(2.0).toLong
  }

  it should "not change banks when qeTotal is zero" in {
    val banks  = Vector(
      mkBank(loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(5000.0)),
    )
    val result = Banking.sellToBuyer(banks, PLN.Zero).banks
    result(0).govBondHoldings shouldBe PLN(5000.0)
  }

  // ---- reassignBankId ----

  "Banking.reassignBankId" should "keep valid bank unchanged" in {
    val banks = Vector(mkBank(id = 0), mkBank(id = 1, capital = PLN(1e5)))
    Banking.reassignBankId(BankId(0), banks) shouldBe BankId(0)
  }

  it should "route to healthiest bank when current bank failed" in {
    val banks = Vector(
      mkBank(id = 0),
      mkBank(id = 1, capital = PLN(1e5), status = BankStatus.Failed(ExecutionMonth(30))),
    )
    Banking.reassignBankId(BankId(1), banks) shouldBe BankId(0)
  }

  // ---- aggregate ----

  "Banking.aggregateFromBanks" should "sum all individual bank values" in {
    val bs  = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    val agg = Banking.aggregateFromBanks(bs.banks)
    agg.deposits shouldBe PLN(1000000.0)
    agg.capital shouldBe PLN(100000.0)
    agg.totalLoans shouldBe PLN.Zero
  }

  // ---- AFS/HTM split ----

  "BankState.govBondHoldings" should "equal afsBonds + htmBonds" in {
    val b = mkBank(govBondHoldings = PLN(10000.0))
    b.govBondHoldings shouldBe PLN(10000.0)
    b.afsBonds shouldBe PLN(4000.0)
    b.htmBonds shouldBe PLN(6000.0)
  }

  "allocateBondIssuance" should "split new issuance per htmShare" in {
    val banks  = Vector(mkBank(id = 0, deposits = PLN(1e6)))
    val result = Banking.allocateBondIssuance(banks, PLN(10000.0), Rate(0.06))
    // 60% HTM, 40% AFS
    td.toDouble(result(0).htmBonds) shouldBe 6000.0 +- 0.01
    td.toDouble(result(0).afsBonds) shouldBe 4000.0 +- 0.01
  }

  it should "update htmBookYield as weighted average on issuance" in {
    val b0     = Banking.BankState(
      id = BankId(0),
      deposits = PLN(1e6),
      loans = PLN.Zero,
      capital = PLN(1e5),
      nplAmount = PLN.Zero,
      afsBonds = PLN.Zero,
      htmBonds = PLN(6000.0),
      htmBookYield = Rate(0.05),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN.Zero,
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val result = Banking.allocateBondIssuance(Vector(b0), PLN(10000.0), Rate(0.08))
    // new HTM = 10000 * 0.6 = 6000, total HTM = 12000
    // weighted yield = (6000*0.05 + 6000*0.08) / 12000 = 0.065
    td.toDouble(result(0).htmBookYield) shouldBe 0.065 +- 0.001
  }

  // ---- sellToBuyer AFS-first ----

  "sellToBuyer" should "sell AFS before HTM" in {
    val b0     = Banking.BankState(
      id = BankId(0),
      deposits = PLN(1e6),
      loans = PLN.Zero,
      capital = PLN(1e5),
      nplAmount = PLN.Zero,
      afsBonds = PLN(4000.0),
      htmBonds = PLN(6000.0),
      htmBookYield = Rate(0.05),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN.Zero,
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val result = Banking.sellToBuyer(Vector(b0), PLN(3000.0)).banks
    td.toDouble(result(0).afsBonds) shouldBe 1000.0 +- 0.01 // 4000 - 3000
    td.toDouble(result(0).htmBonds) shouldBe 6000.0 +- 0.01 // unchanged
  }

  it should "spill into HTM when AFS exhausted" in {
    val b0     = Banking.BankState(
      id = BankId(0),
      deposits = PLN(1e6),
      loans = PLN.Zero,
      capital = PLN(1e5),
      nplAmount = PLN.Zero,
      afsBonds = PLN(2000.0),
      htmBonds = PLN(8000.0),
      htmBookYield = Rate(0.05),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN.Zero,
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val result = Banking.sellToBuyer(Vector(b0), PLN(5000.0)).banks
    td.toDouble(result(0).afsBonds) shouldBe 0.0 +- 0.01    // 2000 fully sold
    td.toDouble(result(0).htmBonds) shouldBe 5000.0 +- 0.01 // 8000 - 3000 spill
  }

  // ---- processHtmForcedSale ----

  "processHtmForcedSale" should "trigger only when LCR < threshold" in {
    // Bank with high LCR — no reclassification
    val healthyBank = Banking.BankState(
      id = BankId(0),
      deposits = PLN(1e6),
      loans = PLN(1e5),
      capital = PLN(1e5),
      nplAmount = PLN.Zero,
      afsBonds = PLN(4e5),
      htmBonds = PLN(6e5),
      htmBookYield = Rate(0.04),
      reservesAtNbp = PLN(1e6), // large reserves → high LCR
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN(6e5),
      termDeposits = PLN(4e5),
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val result      = Banking.processHtmForcedSale(Vector(healthyBank), Rate(0.08))
    result.totalRealizedLoss shouldBe PLN.Zero
    result.banks(0).htmBonds shouldBe healthyBank.htmBonds
  }

  it should "reclassify HTM to AFS and realize loss when LCR breached" in {
    // Bank with very low LCR: HQLA ~ 0 (no reserves, no AFS/HTM counted in HQLA via govBondHoldings)
    // LCR = HQLA / (demandDep * runoff) = (reserves + govBonds) / (demandDep * 0.10)
    // Need LCR < 0.75. Set reserves = 0, govBonds = afsBonds+htmBonds = 1.1e8, demandDep = 2e9
    // LCR = 1.1e8 / (2e9 * 0.10) = 1.1e8 / 2e8 = 0.55 < 0.75 ✓
    val stressedBank = Banking.BankState(
      id = BankId(0),
      deposits = PLN(2e9),
      loans = PLN(1e8),
      capital = PLN(1e8),
      nplAmount = PLN.Zero,
      afsBonds = PLN(1e7),
      htmBonds = PLN(1e8),
      htmBookYield = Rate(0.04),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN(2e9),
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val currentYield = Rate(0.08)
    val result       = Banking.processHtmForcedSale(Vector(stressedBank), currentYield)
    // reclassified = 1e8 * 0.10 = 1e7
    val reclassified = 1e7
    val yieldGap     = 0.08 - 0.04
    val duration     = 4.5
    val expectedLoss = reclassified * duration * yieldGap // 1e7 * 4.5 * 0.04 = 1.8e6
    result.totalRealizedLoss shouldBe PLN(expectedLoss)
    // Total govBondHoldings unchanged
    td.toDouble(result.banks(0).govBondHoldings) shouldBe td.toDouble(stressedBank.govBondHoldings) +- 0.01
    // HTM decreased, AFS increased
    td.toDouble(result.banks(0).htmBonds) shouldBe (1e8 - reclassified) +- 0.01
    td.toDouble(result.banks(0).afsBonds) shouldBe (1e7 + reclassified) +- 0.01
    // Capital reduced by realized loss
    td.toDouble(result.banks(0).capital) shouldBe (1e8 - expectedLoss) +- 1.0
  }

  it should "be no-op when htmBonds is zero" in {
    val bank   = Banking.BankState(
      id = BankId(0),
      deposits = PLN(1e9),
      loans = PLN(1e8),
      capital = PLN(1e8),
      nplAmount = PLN.Zero,
      afsBonds = PLN(1e8),
      htmBonds = PLN.Zero,
      htmBookYield = Rate.Zero,
      reservesAtNbp = PLN(1e4),
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN(6e8),
      termDeposits = PLN(4e8),
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val result = Banking.processHtmForcedSale(Vector(bank), Rate(0.10))
    result.totalRealizedLoss shouldBe PLN.Zero
  }

  it should "realize zero loss when currentYield <= htmBookYield" in {
    // LCR = (0 + 1.1e8) / (2e9 * 0.10) = 0.55 < 0.75 → triggers
    val bank   = Banking.BankState(
      id = BankId(0),
      deposits = PLN(2e9),
      loans = PLN(1e8),
      capital = PLN(1e8),
      nplAmount = PLN.Zero,
      afsBonds = PLN(1e7),
      htmBonds = PLN(1e8),
      htmBookYield = Rate(0.08),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = PLN(2e9),
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
    )
    val result = Banking.processHtmForcedSale(Vector(bank), Rate(0.05))
    result.totalRealizedLoss shouldBe PLN.Zero
    // HTM still reclassified to AFS (just no loss since yield dropped)
    td.toDouble(result.banks(0).htmBonds) should be < td.toDouble(bank.htmBonds)
  }
