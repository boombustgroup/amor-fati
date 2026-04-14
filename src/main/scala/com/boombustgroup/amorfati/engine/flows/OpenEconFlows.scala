package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.{ForeignRuntimeContract, TreasuryRuntimeContract}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Open economy / Balance of Payments emitting flows.
  *
  * Trade (exports/imports), FDI, portfolio flows, primary income (NFA return),
  * secondary income (EU funds, diaspora), tourism, capital flight.
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
      portfolioFlows: PLN,
      primaryIncome: PLN,
      euFunds: PLN,
      diasporaInflow: PLN,
      capitalFlightOutflow: PLN,
  )

  def emitBatches(input: Input): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Foreign,
        ForeignRuntimeContract.TradeSettlement.index,
        EntitySector.Firms,
        FirmIndex.DomesticDemand,
        input.exports,
        AssetType.Cash,
        FlowMechanism.TradeExports,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        FirmIndex.DomesticDemand,
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
        FirmIndex.DomesticDemand,
        input.tourismExport,
        AssetType.Cash,
        FlowMechanism.TourismExport,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        FirmIndex.DomesticDemand,
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
          FirmIndex.Aggregate,
          input.fdi,
          AssetType.Cash,
          FlowMechanism.Fdi,
        ),
      if input.portfolioFlows > PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Foreign,
          ForeignRuntimeContract.CapitalSettlement.index,
          EntitySector.Firms,
          FirmIndex.Aggregate,
          input.portfolioFlows,
          AssetType.Cash,
          FlowMechanism.PortfolioFlow,
        )
      else if input.portfolioFlows < PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Firms,
          FirmIndex.Aggregate,
          EntitySector.Foreign,
          ForeignRuntimeContract.CapitalSettlement.index,
          -input.portfolioFlows,
          AssetType.Cash,
          FlowMechanism.PortfolioFlow,
        )
      else Vector.empty,
      if input.primaryIncome > PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Foreign,
          ForeignRuntimeContract.IncomeSettlement.index,
          EntitySector.Firms,
          FirmIndex.Aggregate,
          input.primaryIncome,
          AssetType.Cash,
          FlowMechanism.PrimaryIncome,
        )
      else if input.primaryIncome < PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Firms,
          FirmIndex.Aggregate,
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
        HouseholdIndex.Aggregate,
        input.diasporaInflow,
        AssetType.Cash,
        FlowMechanism.DiasporaInflow,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        FirmIndex.Aggregate,
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

    // Portfolio flows: positive = inflow (Foreign→Domestic), negative = outflow
    if input.portfolioFlows > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.portfolioFlows.toLong, FlowMechanism.PortfolioFlow.toInt)
    else if input.portfolioFlows < PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.portfolioFlows).toLong, FlowMechanism.PortfolioFlow.toInt)

    // Primary income: positive = NFA earning (Foreign→Domestic), negative = payment
    if input.primaryIncome > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.primaryIncome.toLong, FlowMechanism.PrimaryIncome.toInt)
    else if input.primaryIncome < PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.primaryIncome).toLong, FlowMechanism.PrimaryIncome.toInt)

    if input.euFunds > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.euFunds.toLong, FlowMechanism.EuFunds.toInt)
    if input.diasporaInflow > PLN.Zero then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.diasporaInflow.toLong, FlowMechanism.DiasporaInflow.toInt)
    if input.capitalFlightOutflow > PLN.Zero then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.capitalFlightOutflow.toLong, FlowMechanism.CapitalFlight.toInt)

    flows.result()
