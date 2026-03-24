package com.boombustgroup.amorfati.engine.flows

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

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    if input.exports.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.exports.toLong, FlowMechanism.TradeExports.toInt)
    if input.imports.toLong > 0L then flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.imports.toLong, FlowMechanism.TradeImports.toInt)
    if input.tourismExport.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.tourismExport.toLong, FlowMechanism.TourismExport.toInt)
    if input.tourismImport.toLong > 0L then flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.tourismImport.toLong, FlowMechanism.TourismImport.toInt)

    if input.fdi.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.fdi.toLong, FlowMechanism.Fdi.toInt)

    // Portfolio flows: positive = inflow (Foreign→Domestic), negative = outflow
    if input.portfolioFlows.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.portfolioFlows.toLong, FlowMechanism.PortfolioFlow.toInt)
    else if input.portfolioFlows.toLong < 0L then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.portfolioFlows).toLong, FlowMechanism.PortfolioFlow.toInt)

    // Primary income: positive = NFA earning (Foreign→Domestic), negative = payment
    if input.primaryIncome.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.primaryIncome.toLong, FlowMechanism.PrimaryIncome.toInt)
    else if input.primaryIncome.toLong < 0L then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, (-input.primaryIncome).toLong, FlowMechanism.PrimaryIncome.toInt)

    if input.euFunds.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.euFunds.toLong, FlowMechanism.EuFunds.toInt)
    if input.diasporaInflow.toLong > 0L then flows += Flow(FOREIGN_ACCOUNT, DOMESTIC_ACCOUNT, input.diasporaInflow.toLong, FlowMechanism.DiasporaInflow.toInt)
    if input.capitalFlightOutflow.toLong > 0L then
      flows += Flow(DOMESTIC_ACCOUNT, FOREIGN_ACCOUNT, input.capitalFlightOutflow.toLong, FlowMechanism.CapitalFlight.toInt)

    flows.result()
