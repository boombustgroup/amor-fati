package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FirmEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
  private val w    = init.world
  private val rng  = RandomStream.seeded(42)

  private val s1 = FiscalConstraintEconomics.compute(w, init.banks, init.ledgerFinancialState, ExecutionMonth.First)
  private val s2 = LaborEconomics.compute(w, init.firms, init.households, s1)
  private val s3 =
    HouseholdIncomeEconomics.compute(w, init.firms, init.households, init.banks, init.ledgerFinancialState, s1.lendingBaseRate, s1.resWage, s2.newWage, rng)
  private val s4 = DemandEconomics.compute(w, s2.employed, s2.living, s3.domesticCons)

  private val result              = FirmEconomics.runStep(w, init.firms, init.households, init.banks, init.ledgerFinancialState, s1, s2, s3, s4, RandomStream.seeded(42))
  private val ManufacturingSector = 1

  private case class FirmScenario(
      firms: Vector[Firm.State],
      financialStocks: Vector[Firm.FinancialStocks],
  )

  private def initialFirmStocks: Vector[Firm.FinancialStocks] =
    init.ledgerFinancialState.firms.map(LedgerFinancialState.projectFirmFinancialStocks)

  private def runStepFor(world: World, firms: Vector[Firm.State], financialStocks: Vector[Firm.FinancialStocks])(using SimParams): FirmEconomics.StepOutput =
    val s1                   = FiscalConstraintEconomics.compute(world, init.banks, init.ledgerFinancialState, ExecutionMonth.First)
    val s2                   = LaborEconomics.compute(world, firms, init.households, s1)
    val ledgerFinancialState = init.ledgerFinancialState.copy(
      firms = LedgerFinancialState.refreshFirmFinancialBalances(financialStocks, init.ledgerFinancialState.firms),
    )
    val s3                   =
      HouseholdIncomeEconomics.compute(
        world,
        firms,
        init.households,
        init.banks,
        ledgerFinancialState,
        s1.lendingBaseRate,
        s1.resWage,
        s2.newWage,
        RandomStream.seeded(42),
      )
    val s4                   = DemandEconomics.compute(world, s2.employed, s2.living, s3.domesticCons)
    FirmEconomics.runStep(world, firms, init.households, init.banks, ledgerFinancialState, s1, s2, s3, s4, RandomStream.seeded(43))

  private def manufacturingScenario(stateOwned: Boolean, cashRich: Boolean = false): FirmScenario =
    val firms  = init.firms.map { firm =>
      val base =
        if firm.sector.toInt == ManufacturingSector && cashRich then firm.copy(capitalStock = PLN.Zero, greenCapital = PLN.Zero)
        else firm
      if base.sector.toInt == ManufacturingSector then base.copy(stateOwned = stateOwned) else base.copy(stateOwned = false)
    }
    val stocks = initialFirmStocks
      .zip(firms)
      .map: (stock, firm) =>
        if firm.sector.toInt == ManufacturingSector && cashRich then stock.copy(cash = PLN(500e6))
        else stock
    FirmScenario(firms, stocks)

  private def manufacturingOutputs(step: FirmEconomics.StepOutput): Vector[Firm.State] =
    step.ioFirms.filter(_.sector.toInt == ManufacturingSector)

  private def manufacturingById(step: FirmEconomics.StepOutput): Map[FirmId, Firm.State] =
    manufacturingOutputs(step).map(f => f.id -> f).toMap

  "FirmEconomics.runStep" should "produce flows that close at SFC == 0L" in {
    val flows = FirmFlows.emit(
      FirmFlows.Input(
        wages = s3.totalIncome,
        cit = result.sumTax,
        loanRepayment = result.sumFirmPrincipal,
        newLoans = result.sumNewLoans,
        interestPaid = result.intIncome,
        capex = result.sumCapex,
        equityIssuance = result.sumEquityIssuance,
        ioPayments = result.totalIoPaid,
        nplDefault = result.nplLoss,
        profitShifting = result.sumProfitShifting,
        fdiRepatriation = result.sumFdiRepatriation,
        grossInvestment = result.sumGrossInvestment,
      ),
    )
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
  }

  it should "increase manufacturing capital accumulation when strategic firms are state-owned" in {
    val privateScenario     = manufacturingScenario(stateOwned = false, cashRich = true)
    val stateOwnedScenario  = manufacturingScenario(stateOwned = true, cashRich = true)
    val privateRun          = runStepFor(w, privateScenario.firms, privateScenario.financialStocks)
    val stateOwnedRun       = runStepFor(w, stateOwnedScenario.firms, stateOwnedScenario.financialStocks)
    val privateManufactured = manufacturingOutputs(privateRun)
    val soeManufactured     = manufacturingOutputs(stateOwnedRun)
    val privateById         = manufacturingById(privateRun)
    val soeById             = manufacturingById(stateOwnedRun)

    privateManufactured should not be empty
    soeManufactured.size shouldBe privateManufactured.size
    soeById.keySet shouldBe privateById.keySet
    soeById.keys.foreach { id =>
      soeById(id).capitalStock should be > privateById(id).capitalStock
    }
    stateOwnedRun.sumGrossInvestment should be > privateRun.sumGrossInvestment
  }

  it should "reduce manufacturing markup pass-through for state-owned firms under a commodity shock" in {
    val privateScenario       = manufacturingScenario(stateOwned = false)
    val stateOwnedScenario    = manufacturingScenario(stateOwned = true)
    val shockedWorld          = w.copy(external = w.external.copy(gvc = w.external.gvc.copy(commodityPriceIndex = PriceIndex(1.20))))
    val baselinePrivateRun    = runStepFor(w, privateScenario.firms, privateScenario.financialStocks)
    val baselineStateOwnedRun = runStepFor(w, stateOwnedScenario.firms, stateOwnedScenario.financialStocks)
    val shockedPrivateRun     = runStepFor(shockedWorld, privateScenario.firms, privateScenario.financialStocks)
    val shockedStateOwnedRun  = runStepFor(shockedWorld, stateOwnedScenario.firms, stateOwnedScenario.financialStocks)
    val baselinePrivateById   = manufacturingById(baselinePrivateRun)
    val baselineSoeById       = manufacturingById(baselineStateOwnedRun)
    val shockedPrivateById    = manufacturingById(shockedPrivateRun)
    val shockedSoeById        = manufacturingById(shockedStateOwnedRun)

    shockedSoeById.keySet shouldBe shockedPrivateById.keySet
    baselineSoeById.keySet shouldBe baselinePrivateById.keySet
    baselineSoeById.keys.foreach { id =>
      baselineSoeById(id).markup shouldBe baselinePrivateById(id).markup
    }
    shockedStateOwnedRun.markupInflation should be <= shockedPrivateRun.markupInflation
    shockedSoeById.keys.foreach { id =>
      shockedSoeById(id).markup should be <= shockedPrivateById(id).markup
    }
    shockedSoeById.keys.exists(id => shockedSoeById(id).markup < shockedPrivateById(id).markup) shouldBe true
  }
