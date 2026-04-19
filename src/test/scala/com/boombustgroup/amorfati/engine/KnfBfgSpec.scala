package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.Generators
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

class KnfBfgSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private case class BankRow(bank: Banking.BankState, stocks: Banking.BankFinancialStocks)

  private def mkBankRow(
      id: Int = 0,
      deposits: PLN,
      loans: PLN = PLN.Zero,
      capital: PLN = PLN(100000),
      nplAmount: PLN = PLN.Zero,
      govBondHoldings: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
  ): BankRow =
    BankRow(
      Banking.BankState(
        id = BankId(id),
        capital = capital,
        nplAmount = nplAmount,
        htmBookYield = Rate(0.055),
        status = status,
        loansShort = PLN.Zero,
        loansMedium = PLN.Zero,
        loansLong = PLN.Zero,
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = deposits,
        firmLoan = loans,
        govBondAfs = govBondHoldings * Share(0.40),
        govBondHtm = govBondHoldings * Share(0.60),
        reserve = PLN.Zero,
        interbankLoan = interbankNet,
        demandDeposit = PLN.Zero,
        termDeposit = PLN.Zero,
        consumerLoan = PLN.Zero,
      ),
    )

  private def banks(rows: Vector[BankRow]): Vector[Banking.BankState] =
    rows.map(_.bank)

  private def stocks(rows: Vector[BankRow]): Vector[Banking.BankFinancialStocks] =
    rows.map(_.stocks)

  "KNF banking params" should "expose calibrated P2R, BFG levy, bail-in, and guarantee defaults" in {
    p.banking.p2rAddons.length shouldBe 7
    p.banking.p2rAddons(0) shouldBe Multiplier(0.015)
    p.banking.p2rAddons(2) shouldBe Multiplier(0.030)
    p.banking.bfgLevyRate shouldBe Rate(0.0024)
    p.banking.bailInDepositHaircut shouldBe Share(0.08)
    p.banking.bfgDepositGuarantee shouldBe PLN(400000.0)
  }

  "Macroprudential P2R" should "return per-bank add-ons and affect effective minimum CAR" in {
    Macroprudential.p2rAddon(0) shouldBe Multiplier(0.015)
    Macroprudential.p2rAddon(2) shouldBe Multiplier(0.030)
    Macroprudential.p2rAddon(7) shouldBe p.banking.p2rAddons.last

    val ccyb        = Multiplier(0.01)
    val expectedPko = td.toDouble(p.banking.minCar) + td.toDouble(ccyb) + td.toDouble(p.banking.osiiPkoBp) + td.toDouble(p.banking.p2rAddons(0))
    td.toDouble(Macroprudential.effectiveMinCarImpl(0, ccyb)) shouldBe expectedPko +- 1e-10
    Macroprudential.effectiveMinCarImpl(0, Multiplier.Zero) should be > p.banking.minCar
    Macroprudential.p2rAddon(2) should be > Macroprudential.p2rAddon(0)
  }

  "Banking.computeBfgLevy" should "compute monthly levy from explicit deposit stocks" in {
    val rows     = Vector(
      mkBankRow(id = 0, deposits = PLN(1000000.0)),
      mkBankRow(id = 1, deposits = PLN(2000000.0), capital = PLN(200000)),
      mkBankRow(id = 2, deposits = PLN(500000.0), capital = PLN(50000), status = BankStatus.Failed(ExecutionMonth(3))),
    )
    val result   = Banking.computeBfgLevy(banks(rows), stocks(rows))
    val expected = (1000000.0 + 2000000.0) * td.toDouble(p.banking.bfgLevyRate) / 12.0

    td.toDouble(result.total) shouldBe expected +- 0.01
    result.perBank(2) shouldBe PLN.Zero
    result.perBank(0) should be > PLN.Zero
  }

  "Banking.applyBailIn" should "haircut only failed-bank uninsured deposit stocks" in {
    val safe       = Vector(mkBankRow(deposits = PLN(500000.0), loans = PLN(100000), capital = PLN(50000)))
    val safeResult = Banking.applyBailIn(banks(safe), stocks(safe))
    safeResult.financialStocks.head.totalDeposits shouldBe PLN(500000.0)
    safeResult.totalLoss shouldBe PLN.Zero

    val guaranteed       = Vector(mkBankRow(deposits = PLN(300000.0), capital = PLN.Zero, status = BankStatus.Failed(ExecutionMonth(5))))
    val guaranteedResult = Banking.applyBailIn(banks(guaranteed), stocks(guaranteed))
    guaranteedResult.financialStocks.head.totalDeposits shouldBe PLN(300000.0)
    guaranteedResult.totalLoss shouldBe PLN.Zero

    val failed       = Vector(mkBankRow(deposits = PLN(1000000.0), capital = PLN.Zero, status = BankStatus.Failed(ExecutionMonth(5))))
    val failedResult = Banking.applyBailIn(banks(failed), stocks(failed))
    val haircut      = (PLN(1000000.0) - p.banking.bfgDepositGuarantee) * p.banking.bailInDepositHaircut
    failedResult.financialStocks.head.totalDeposits shouldBe PLN(1000000.0) - haircut
    failedResult.totalLoss shouldBe haircut
  }

  "Banking.resolveFailures" should "transfer failed-bank deposits, bonds, and interbank stock to the survivor" in {
    val rows   = Vector(
      mkBankRow(
        id = 0,
        deposits = PLN(500000.0),
        loans = PLN(100000),
        capital = PLN.Zero,
        nplAmount = PLN(50000),
        govBondHoldings = PLN(10e9),
        interbankNet = PLN(1000.0),
        status = BankStatus.Failed(ExecutionMonth(3)),
      ),
      mkBankRow(id = 1, deposits = PLN(2000000.0), loans = PLN(200000), capital = PLN(200000), govBondHoldings = PLN(5e9), interbankNet = PLN(-1000.0)),
    )
    val result = Banking.resolveFailures(banks(rows), stocks(rows))

    result.financialStocks(0).totalDeposits shouldBe PLN.Zero
    result.financialStocks(0).firmLoan shouldBe PLN.Zero
    result.financialStocks(0).interbankLoan shouldBe PLN.Zero
    td.toDouble(Banking.govBondHoldings(result.financialStocks(1))) shouldBe 15e9 +- 1.0
    td.toDouble(result.financialStocks.map(_.interbankLoan).sum) shouldBe 0.0 +- 1e-6
  }

  it should "create a bridge bank when all banks fail and preserve aggregate bond stock" in {
    val rows   = Vector(
      mkBankRow(
        id = 0,
        deposits = PLN(500000.0),
        loans = PLN(100000),
        capital = PLN(-10000),
        nplAmount = PLN(50000),
        govBondHoldings = PLN(20e9),
        status = BankStatus.Failed(ExecutionMonth(3)),
      ),
      mkBankRow(
        id = 1,
        deposits = PLN(300000.0),
        loans = PLN(80000),
        capital = PLN(-5000),
        nplAmount = PLN(30000),
        govBondHoldings = PLN(15e9),
        status = BankStatus.Failed(ExecutionMonth(3)),
      ),
      mkBankRow(
        id = 2,
        deposits = PLN(200000.0),
        loans = PLN(60000),
        capital = PLN(-20000),
        nplAmount = PLN(20000),
        govBondHoldings = PLN(13.8e9),
        status = BankStatus.Failed(ExecutionMonth(3)),
      ),
    )
    val before = stocks(rows).map(s => td.toDouble(Banking.govBondHoldings(s))).sum
    val result = Banking.resolveFailures(banks(rows), stocks(rows))
    val after  = result.financialStocks.map(s => td.toDouble(Banking.govBondHoldings(s))).sum

    after shouldBe before +- 1.0
    result.banks.exists(!_.failed) shouldBe true
  }

  "Banking.healthiestBankId" should "return bank with highest capital when all fail" in {
    val rows = Vector(
      mkBankRow(id = 0, deposits = PLN(500000.0), loans = PLN(100000), capital = PLN(-20000), status = BankStatus.Failed(ExecutionMonth(3))),
      mkBankRow(id = 1, deposits = PLN(300000.0), loans = PLN(80000), capital = PLN(-5000), status = BankStatus.Failed(ExecutionMonth(3))),
      mkBankRow(id = 2, deposits = PLN(200000.0), loans = PLN(60000), capital = PLN(-15000), status = BankStatus.Failed(ExecutionMonth(3))),
    )

    Banking.healthiestBankId(banks(rows), stocks(rows)) shouldBe BankId(1)
  }

  "World defaults" should "keep bfgFundBalance and bailInLoss at zero" in {
    val w = Generators.testWorld(
      monthlyGdpProxy = PLN(100000.0),
      currentSigmas = Vector(Sigma(1.0), Sigma(1.0), Sigma(1.0), Sigma(1.0), Sigma(1.0), Sigma(1.0)),
      marketWage = PLN(8000),
      reservationWage = PLN(4000),
    )
    w.mechanisms.bfgFundBalance shouldBe PLN.Zero
    w.flows.bailInLoss shouldBe PLN.Zero
  }
