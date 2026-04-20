package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.{ForeignRuntimeContract, TreasuryRuntimeContract}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Open economy / Balance of Payments emitting flows.
  *
  * Trade (exports/imports), FDI, portfolio flows, carry trade, primary income
  * (NFA return), secondary income (EU funds, diaspora), tourism, capital
  * flight.
  *
  * Remittances (HH→Foreign) already emitted by HouseholdFlows.
  *
  * Account IDs: 0=Domestic, 1=Foreign, 2=NBP (reserves)
  */
object OpenEconFlows:

  val DOMESTIC_ACCOUNT: Int = 0
  val FOREIGN_ACCOUNT: Int  = 1
  val NBP_ACCOUNT: Int      = 2

  case class Input(
      exports: PLN,
      imports: PLN,
      tourismExport: PLN,
      tourismImport: PLN,
      fdi: PLN,
      portfolioFlows: PLN,      // ordinary signed portfolio allocation
      carryTradeFlow: PLN,      // signed carry accumulation or unwind
      primaryIncome: PLN,
      euFunds: PLN,
      diasporaInflow: PLN,
      capitalFlightOutflow: PLN, // positive stress/confidence outflow
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.TradeSettlement.index,
        EntitySector.Firms,
        topology.firms.domesticDemand,
        input.exports,
        AssetType.Cash,
        FlowMechanism.TradeExports,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.domesticDemand,
        EntitySector.Foreign,
        ForeignRuntimeContract.TradeSettlement.index,
        input.imports,
        AssetType.Cash,
        FlowMechanism.TradeImports,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.TradeSettlement.index,
        EntitySector.Firms,
        topology.firms.domesticDemand,
        input.tourismExport,
        AssetType.Cash,
        FlowMechanism.TourismExport,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.domesticDemand,
        EntitySector.Foreign,
        ForeignRuntimeContract.TradeSettlement.index,
        input.tourismImport,
        AssetType.Cash,
        FlowMechanism.TourismImport,
      ),
      AggregateBatchedEmission
        .transfer(
          EntitySector.Foreign,
          ForeignRuntimeContract.CapitalSettlement.index,
          EntitySector.Firms,
          topology.firms.aggregate,
          input.fdi,
          AssetType.Cash,
          FlowMechanism.Fdi,
        ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.CapitalSettlement.index,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.portfolioFlows,
        AssetType.Cash,
        FlowMechanism.PortfolioFlow,
      ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.CapitalSettlement.index,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.carryTradeFlow,
        AssetType.Cash,
        FlowMechanism.CarryTradeFlow,
      ),
      if input.primaryIncome > PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Foreign,
          ForeignRuntimeContract.IncomeSettlement.index,
          EntitySector.Firms,
          topology.firms.aggregate,
          input.primaryIncome,
          AssetType.Cash,
          FlowMechanism.PrimaryIncome,
        )
      else if input.primaryIncome < PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Firms,
          topology.firms.aggregate,
          EntitySector.Foreign,
          ForeignRuntimeContract.IncomeSettlement.index,
          -input.primaryIncome,
          AssetType.Cash,
          FlowMechanism.PrimaryIncome,
        )
      else Vector.empty,
      AggregateBatchedEmission.transfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.TransferSettlement.index,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.euFunds,
        AssetType.Cash,
        FlowMechanism.EuFunds,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.TransferSettlement.index,
        EntitySector.Households,
        topology.households.aggregate,
        input.diasporaInflow,
        AssetType.Cash,
        FlowMechanism.DiasporaInflow,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Foreign,
        ForeignRuntimeContract.CapitalSettlement.index,
        input.capitalFlightOutflow,
        AssetType.Cash,
        FlowMechanism.CapitalFlight,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    if input.exports > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.exports.toLong, FlowMechanism.TradeExports.toInt)
    if input.imports > PLN.Zero then flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.imports.toLong, FlowMechanism.TradeImports.toInt)
    if input.tourismExport > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.tourismExport.toLong, FlowMechanism.TourismExport.toInt)
    if input.tourismImport > PLN.Zero then flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.tourismImport.toLong, FlowMechanism.TourismImport.toInt)

    if input.fdi > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.fdi.toLong, FlowMechanism.Fdi.toInt)

    // Portfolio and carry flows: positive = inflow (Foreign→Domestic), negative = outflow
    if input.portfolioFlows > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.portfolioFlows.toLong, FlowMechanism.PortfolioFlow.toInt)
    else if input.portfolioFlows < PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.portfolioFlows).toLong, FlowMechanism.PortfolioFlow.toInt)
    if input.carryTradeFlow > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.carryTradeFlow.toLong, FlowMechanism.CarryTradeFlow.toInt)
    else if input.carryTradeFlow < PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.carryTradeFlow).toLong, FlowMechanism.CarryTradeFlow.toInt)

    // Primary income: positive = NFA earning (Foreign→Domestic), negative = payment
    if input.primaryIncome > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.primaryIncome.toLong, FlowMechanism.PrimaryIncome.toInt)
    else if input.primaryIncome < PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.primaryIncome).toLong, FlowMechanism.PrimaryIncome.toInt)

    if input.euFunds > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.euFunds.toLong, FlowMechanism.EuFunds.toInt)
    if input.diasporaInflow > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.diasporaInflow.toLong, FlowMechanism.DiasporaInflow.toInt)
    if input.capitalFlightOutflow > PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.capitalFlightOutflow.toLong, FlowMechanism.CapitalFlight.toInt)

    flows.result()
