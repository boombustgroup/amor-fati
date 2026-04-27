package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.ForeignRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpenEconFlowsSpec extends AnyFlatSpec with Matchers:

  private val runtimeTopologies = Vector(
    "zeroPopulation"    -> RuntimeLedgerTopology.zeroPopulation,
    "nonZeroPopulation" -> RuntimeLedgerTopology.nonZeroPopulation,
  )

  private val baseInput = OpenEconFlows.Input(
    exports = PLN(20000000),
    imports = PLN(18000000),
    tourismExport = PLN(1000000),
    tourismImport = PLN(800000),
    fdi = PLN(2000000),
    portfolioFlows = PLN(500000),
    carryTradeFlow = PLN(100000),
    primaryIncome = PLN(-300000),
    euFunds = PLN(1500000),
    diasporaInflow = PLN(400000),
    capitalFlightOutflow = PLN(200000),
  )

  "OpenEconFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = OpenEconFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have correct domestic balance (CA + KA)" in {
    val flows    = OpenEconFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val expected = baseInput.exports - baseInput.imports +
      baseInput.tourismExport - baseInput.tourismImport +
      baseInput.fdi + baseInput.portfolioFlows + baseInput.carryTradeFlow +
      baseInput.primaryIncome + baseInput.euFunds + baseInput.diasporaInflow -
      baseInput.capitalFlightOutflow

    balances(OpenEconFlows.DOMESTIC_ACCOUNT) shouldBe expected.toLong
  }

  it should "handle negative portfolio flows (outflow)" in {
    val outflow  = baseInput.copy(portfolioFlows = PLN(-1000000))
    val flows    = OpenEconFlows.emit(outflow)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
    // Portfolio outflow debits domestic
    flows.filter(_.mechanism == FlowMechanism.PortfolioFlow.toInt).head.from shouldBe OpenEconFlows.DOMESTIC_ACCOUNT
  }

  it should "handle negative carry trade flows (unwind)" in {
    val outflow  = baseInput.copy(carryTradeFlow = PLN(-250000))
    val flows    = OpenEconFlows.emit(outflow)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
    flows.filter(_.mechanism == FlowMechanism.CarryTradeFlow.toInt).head.from shouldBe OpenEconFlows.DOMESTIC_ACCOUNT
  }

  it should "handle negative primary income (NFA payment)" in {
    val payment  = baseInput.copy(primaryIncome = PLN(-500000))
    val flows    = OpenEconFlows.emit(payment)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "route trade, income, capital, and transfer channels through distinct foreign settlement shells" in
    runtimeTopologies.foreach:
      case (label, topology) =>
        withClue(s"$label: ") {
          val batches                                      = OpenEconFlows.emitBatches(baseInput.copy(primaryIncome = PLN(300000)))(using topology)
          def onlyFromIndex(mechanism: MechanismId): Int   =
            batches.find(_.mechanism == mechanism).get match
              case broadcast: BatchedFlow.Broadcast => broadcast.fromIndex
              case other                            => fail(s"expected broadcast batch for $mechanism but got $other")
          def onlyTargetIndex(mechanism: MechanismId): Int =
            batches.find(_.mechanism == mechanism).get match
              case broadcast: BatchedFlow.Broadcast => broadcast.targetIndices.head
              case other                            => fail(s"expected broadcast batch for $mechanism but got $other")

          onlyFromIndex(FlowMechanism.TradeExports) shouldBe ForeignRuntimeContract.TradeSettlement.index
          onlyTargetIndex(FlowMechanism.TradeImports) shouldBe ForeignRuntimeContract.TradeSettlement.index
          onlyFromIndex(FlowMechanism.TourismExport) shouldBe ForeignRuntimeContract.TradeSettlement.index
          onlyTargetIndex(FlowMechanism.TourismImport) shouldBe ForeignRuntimeContract.TradeSettlement.index

          onlyFromIndex(FlowMechanism.Fdi) shouldBe ForeignRuntimeContract.CapitalSettlement.index
          onlyFromIndex(FlowMechanism.PortfolioFlow) shouldBe ForeignRuntimeContract.CapitalSettlement.index
          onlyFromIndex(FlowMechanism.CarryTradeFlow) shouldBe ForeignRuntimeContract.CapitalSettlement.index
          onlyTargetIndex(FlowMechanism.CapitalFlight) shouldBe ForeignRuntimeContract.CapitalSettlement.index

          onlyFromIndex(FlowMechanism.PrimaryIncome) shouldBe ForeignRuntimeContract.IncomeSettlement.index
          onlyFromIndex(FlowMechanism.EuFunds) shouldBe ForeignRuntimeContract.TransferSettlement.index
          onlyFromIndex(FlowMechanism.DiasporaInflow) shouldBe ForeignRuntimeContract.TransferSettlement.index
        }
