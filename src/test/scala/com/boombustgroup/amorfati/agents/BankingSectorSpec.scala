package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.Generators
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class BankingSectorSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val configs = Banking.DefaultConfigs

  private case class BankRow(bank: Banking.BankState, stocks: Banking.BankFinancialStocks)

  private def mkBankRow(
      id: Int = 0,
      deposits: PLN = PLN(1000000),
      loans: PLN = PLN(1000000),
      capital: PLN = PLN(200000),
      nplAmount: PLN = PLN.Zero,
      govBondHoldings: PLN = PLN.Zero,
      govBondAfsShare: Share = Share.decimal(40, 2),
      htmBookYield: Rate = Rate.decimal(55, 3),
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
  ): BankRow =
    BankRow(
      Banking.BankState(
        id = BankId(id),
        capital = capital,
        nplAmount = nplAmount,
        htmBookYield = htmBookYield,
        status = status,
        loansShort = loansShort,
        loansMedium = loansMedium,
        loansLong = loansLong,
        consumerNpl = consumerNpl,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = deposits,
        firmLoan = loans,
        govBondAfs = govBondHoldings * govBondAfsShare,
        govBondHtm = govBondHoldings * (Share.One - govBondAfsShare),
        reserve = reservesAtNbp,
        interbankLoan = interbankNet,
        demandDeposit = demandDeposits,
        termDeposit = termDeposits,
        consumerLoan = consumerLoans,
      ),
    )

  private def banks(rows: Vector[BankRow]): Vector[Banking.BankState] =
    rows.map(_.bank)

  private def stocks(rows: Vector[BankRow]): Vector[Banking.BankFinancialStocks] =
    rows.map(_.stocks)

  private def govBonds(stocks: Banking.BankFinancialStocks): PLN =
    Banking.govBondHoldings(stocks)

  "Generators.testBankingSector" should "create 7 banks with explicit financial stocks preserving totals" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000), totalCapital = PLN(100000), totalLoans = PLN.Zero, configs = configs)

    bs.banks.length shouldBe 7
    bs.financialStocks.map(s => decimal(s.totalDeposits)).sum shouldBe BigDecimal("1000000.0") +- BigDecimal("0.01")
    bs.banks.map(b => decimal(b.capital)).sum shouldBe BigDecimal("100000.0") +- BigDecimal("0.01")
    decimal(bs.financialStocks(0).totalDeposits) shouldBe (BigDecimal("1000000.0") * BigDecimal("0.175")) +- BigDecimal("0.01")
    decimal(bs.financialStocks(5).totalDeposits) shouldBe (BigDecimal("1000000.0") * BigDecimal("0.050")) +- BigDecimal("0.01")
    bs.banks.forall(!_.failed) shouldBe true
  }

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
    decimal(assignments.count(_ == BankId(5))) / BigDecimal(10000) should be > BigDecimal("0.10")
  }

  "Banking.lendingRate" should "price failed banks, NPL stress, and bank-specific spread from explicit stocks" in {
    val failed = mkBankRow(status = BankStatus.Failed(ExecutionMonth(30)))
    Banking.lendingRate(failed.bank, failed.stocks, configs(0), Rate.decimal(5, 2), Rate.Zero, PLN.Zero) shouldBe Rate.decimal(55, 2)

    val lowNpl  = mkBankRow(nplAmount = PLN(10000))
    val highNpl = mkBankRow(nplAmount = PLN(200000))
    Banking.lendingRate(highNpl.bank, highNpl.stocks, configs(0), Rate.decimal(5, 2), Rate.Zero, PLN.Zero) should be >
      Banking.lendingRate(lowNpl.bank, lowNpl.stocks, configs(0), Rate.decimal(5, 2), Rate.Zero, PLN.Zero)

    val base = mkBankRow(nplAmount = PLN.Zero)
    Banking.lendingRate(base.bank, base.stocks, configs(5), Rate.decimal(5, 2), Rate.Zero, PLN.Zero) should be >
      Banking.lendingRate(base.bank, base.stocks, configs(0), Rate.decimal(5, 2), Rate.Zero, PLN.Zero)
  }

  "Banking.canLend" should "reject failed banks and low projected CAR" in {
    val failed = mkBankRow(capital = PLN(100000), status = BankStatus.Failed(ExecutionMonth(30)))
    Banking.canLend(failed.bank, failed.stocks, PLN(1000), RandomStream.seeded(42), Multiplier.Zero, PLN.Zero) shouldBe false

    val weak = mkBankRow(loans = PLN(100000), capital = PLN(8000))
    val rng  = RandomStream.seeded(42L)
    (0 until 100).forall(_ => !Banking.canLend(weak.bank, weak.stocks, PLN(10000), rng, Multiplier.Zero, PLN.Zero)) shouldBe true
  }

  "Banking.interbankRate" should "use explicit financial stocks" in {
    val healthy  = Vector(mkBankRow(id = 0), mkBankRow(id = 1))
    val stressed = Vector(
      mkBankRow(id = 0, nplAmount = PLN(100000)),
      mkBankRow(id = 1, nplAmount = PLN(100000)),
    )

    decimal(Banking.interbankRate(banks(healthy), stocks(healthy), Rate.decimal(5, 2))) shouldBe (BigDecimal("0.05") - BigDecimal("0.01")) +- BigDecimal(
      "0.001",
    )
    decimal(Banking.interbankRate(banks(stressed), stocks(stressed), Rate.decimal(5, 2))) shouldBe (BigDecimal("0.05") + BigDecimal("0.01")) +- BigDecimal(
      "0.001",
    )
  }

  "Banking.clearInterbank" should "clear net interbank positions on financial stocks" in {
    val rows    = Vector(
      mkBankRow(id = 0, loans = PLN(300000), govBondHoldings = PLN(100000)),
      mkBankRow(id = 1, deposits = PLN(500000), loans = PLN(800000), capital = PLN(100000)),
      mkBankRow(id = 2, deposits = PLN(800000), loans = PLN(200000), capital = PLN(150000), govBondHoldings = PLN(50000)),
    )
    val cleared = Banking.clearInterbank(banks(rows), stocks(rows), configs.take(3))

    cleared.financialStocks.map(s => decimal(s.interbankLoan)).sum shouldBe BigDecimal("0.0") +- BigDecimal("0.01")
    cleared.financialStocks.foreach(_.reserve should be >= PLN.Zero)
  }

  it should "set failed banks' interbank position to zero" in {
    val rows = Vector(
      mkBankRow(id = 0, loans = PLN(300000)),
      mkBankRow(id = 1, deposits = PLN(500000), loans = PLN(800000), capital = PLN(100000), status = BankStatus.Failed(ExecutionMonth(30))),
    )
    Banking.clearInterbank(banks(rows), stocks(rows), configs.take(2)).financialStocks(1).interbankLoan shouldBe PLN.Zero
  }

  "Banking.checkFailures" should "respect disabled mode, trigger failures, and reset recovered counters" in {
    val weak     = mkBankRow(capital = PLN(1000), status = BankStatus.Active(5))
    val disabled = Banking.checkFailures(Vector(weak.bank), Vector(weak.stocks), ExecutionMonth(30), enabled = false, Multiplier.Zero)
    disabled.anyFailed shouldBe false
    disabled.banks.head.failed shouldBe false

    val preFailed = mkBankRow(capital = PLN(1000), status = BankStatus.Active(2))
    val failed    = Banking.checkFailures(Vector(preFailed.bank), Vector(preFailed.stocks), ExecutionMonth(30), enabled = true, Multiplier.Zero)
    failed.anyFailed shouldBe true
    failed.banks.head.failed shouldBe true
    failed.banks.head.capital shouldBe PLN.Zero

    val recovered = mkBankRow(status = BankStatus.Active(2))
    Banking
      .checkFailures(Vector(recovered.bank), Vector(recovered.stocks), ExecutionMonth(30), enabled = true, Multiplier.Zero)
      .banks
      .head
      .consecutiveLowCar shouldBe 0
  }

  it should "fail negative-capital banks immediately" in {
    val insolvent = mkBankRow(capital = PLN(-1))
    val result    = Banking.checkFailures(Vector(insolvent.bank), Vector(insolvent.stocks), ExecutionMonth(30), enabled = true, Multiplier.Zero)

    result.anyFailed shouldBe true
    result.banks.head.failed shouldBe true
    result.banks.head.capital shouldBe PLN.Zero
  }

  "Banking.resolveFailures" should "move failed-bank stocks to the healthiest survivor" in {
    val rows   = Vector(
      mkBankRow(id = 0, deposits = PLN(500000), loans = PLN(100000), capital = PLN(50000), govBondHoldings = PLN(10000)),
      mkBankRow(
        id = 1,
        deposits = PLN(300000),
        loans = PLN(80000),
        capital = PLN.Zero,
        govBondHoldings = PLN(5000),
        status = BankStatus.Failed(ExecutionMonth(30)),
      ),
    )
    val result = Banking.resolveFailures(banks(rows), stocks(rows), Vector(PLN(1000), PLN(250)))

    decimal(result.financialStocks(0).totalDeposits) shouldBe BigDecimal("800000.0") +- BigDecimal("0.01")
    result.financialStocks(1).totalDeposits shouldBe PLN.Zero
    result.bankCorpBondHoldings shouldBe Vector(PLN(1250), PLN.Zero)
  }

  "Banking.allocateBondIssuance" should "distribute new bonds by deposits and update HTM yield" in {
    val rows   = Vector(
      mkBankRow(id = 0, deposits = PLN(600000), loans = PLN.Zero, capital = PLN(100000)),
      mkBankRow(id = 1, deposits = PLN(400000), loans = PLN.Zero, capital = PLN(100000)),
    )
    val result = Banking.allocateBondIssuance(banks(rows), stocks(rows), PLN(10000), Rate.decimal(5, 2))

    decimal(govBonds(result.financialStocks(0))) shouldBe BigDecimal("6000.0") +- BigDecimal("0.01")
    decimal(govBonds(result.financialStocks(1))) shouldBe BigDecimal("4000.0") +- BigDecimal("0.01")

    val htmRow    = mkBankRow(
      id = 0,
      deposits = PLN(1000000),
      loans = PLN.Zero,
      capital = PLN(100000),
      govBondHoldings = PLN(6000),
      govBondAfsShare = Share.Zero,
      htmBookYield = Rate.decimal(5, 2),
    )
    val htmResult = Banking.allocateBondIssuance(Vector(htmRow.bank), Vector(htmRow.stocks), PLN(10000), Rate.decimal(8, 2))
    decimal(htmResult.banks.head.htmBookYield) shouldBe BigDecimal("0.065") +- BigDecimal("0.001")
  }

  "Banking.allocateBondRedemption" should "reduce bond stocks proportionally" in {
    val rows   = Vector(
      mkBankRow(id = 0, deposits = PLN(500000), loans = PLN.Zero, capital = PLN(100000), govBondHoldings = PLN(8000)),
      mkBankRow(id = 1, deposits = PLN(500000), loans = PLN.Zero, capital = PLN(100000), govBondHoldings = PLN(2000)),
    )
    val result = Banking.allocateBondRedemption(banks(rows), stocks(rows), PLN(4000), Rate.decimal(5, 2))

    decimal(govBonds(result.financialStocks(0))) shouldBe BigDecimal("6000.0") +- BigDecimal("0.01")
    decimal(govBonds(result.financialStocks(1))) shouldBe BigDecimal("0.0") +- BigDecimal("0.01")
  }

  "Banking.sellToBuyer" should "sell AFS first and preserve exact requested sale where available" in {
    val rows = Vector(
      mkBankRow(id = 0, loans = PLN.Zero, capital = PLN(100000), govBondHoldings = PLN(6000)),
      mkBankRow(id = 1, loans = PLN.Zero, capital = PLN(100000), govBondHoldings = PLN(4000)),
    )
    val sale = Banking.sellToBuyer(banks(rows), stocks(rows), PLN(5000))

    sale.actualSold shouldBe PLN(5000)
    decimal(govBonds(sale.financialStocks(0))) shouldBe BigDecimal("3000.0") +- BigDecimal("0.01")
    decimal(govBonds(sale.financialStocks(1))) shouldBe BigDecimal("2000.0") +- BigDecimal("0.01")

    val afsFirst = mkBankRow(id = 0, loans = PLN.Zero, capital = PLN(100000), govBondHoldings = PLN(10000))
    val afterAfs = Banking.sellToBuyer(Vector(afsFirst.bank), Vector(afsFirst.stocks), PLN(3000)).financialStocks.head
    decimal(afterAfs.govBondAfs) shouldBe BigDecimal("1000.0") +- BigDecimal("0.01")
    decimal(afterAfs.govBondHtm) shouldBe BigDecimal("6000.0") +- BigDecimal("0.01")
  }

  "Banking.reassignBankId" should "route failed-bank clients to the healthiest survivor" in {
    val rows = Vector(
      mkBankRow(id = 0),
      mkBankRow(id = 1, capital = PLN(100000), status = BankStatus.Failed(ExecutionMonth(30))),
    )

    Banking.reassignBankId(BankId(0), banks(rows), stocks(rows)) shouldBe BankId(0)
    Banking.reassignBankId(BankId(1), banks(rows), stocks(rows)) shouldBe BankId(0)
  }

  "Banking.aggregateFromBankStocks" should "sum operational state and explicit financial stocks" in {
    val bs  = Generators.testBankingSector(totalDeposits = PLN(1000000), totalCapital = PLN(100000), totalLoans = PLN.Zero, configs = configs)
    val agg = Banking.aggregateFromBankStocks(bs.banks, bs.financialStocks)

    agg.deposits shouldBe PLN(1000000)
    agg.capital shouldBe PLN(100000)
    agg.totalLoans shouldBe PLN.Zero
  }

  "Banking.processHtmForcedSale" should "reclassify HTM to AFS and realize loss only under LCR stress" in {
    val healthy       = mkBankRow(
      id = 0,
      deposits = PLN(1000000),
      loans = PLN(100000),
      capital = PLN(100000),
      govBondHoldings = PLN(1000000),
      htmBookYield = Rate.decimal(4, 2),
      reservesAtNbp = PLN(1000000),
      demandDeposits = PLN(600000),
      termDeposits = PLN(400000),
    )
    val healthyResult = Banking.processHtmForcedSale(Vector(healthy.bank), Vector(healthy.stocks), Rate.decimal(8, 2))
    healthyResult.totalRealizedLoss shouldBe PLN.Zero
    healthyResult.financialStocks.head.govBondHtm shouldBe healthy.stocks.govBondHtm

    val stressed       = mkBankRow(
      id = 0,
      deposits = PLN(2000000000),
      loans = PLN(100000000),
      capital = PLN(100000000),
      htmBookYield = Rate.decimal(4, 2),
      demandDeposits = PLN(2000000000),
      termDeposits = PLN.Zero,
    )
    val stressedStocks = stressed.stocks.copy(govBondAfs = PLN(10000000), govBondHtm = PLN(100000000))
    val result         = Banking.processHtmForcedSale(Vector(stressed.bank), Vector(stressedStocks), Rate.decimal(8, 2))
    val expectedLoss   = PLN(1800000)

    result.totalRealizedLoss shouldBe expectedLoss
    govBonds(result.financialStocks.head) shouldBe govBonds(stressedStocks)
    result.financialStocks.head.govBondHtm shouldBe PLN(90000000)
    result.financialStocks.head.govBondAfs shouldBe PLN(20000000)
    decimal(result.banks.head.capital) shouldBe (BigDecimal("1e8") - decimal(expectedLoss)) +- BigDecimal("1.0")
  }
