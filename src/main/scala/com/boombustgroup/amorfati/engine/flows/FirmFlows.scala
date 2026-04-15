package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.{ForeignRuntimeContract, TreasuryRuntimeContract}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Firm sector emitting flows from aggregate data.
  *
  * Aggregate level: total wages, CIT, loans, investment, I-O, NPL, FDI flows.
  * Per-agent BatchedFlow.Scatter will replace when new pipeline is built.
  *
  * Account IDs: 0=Firm, 1=HH (wages), 2=Gov (CIT), 3=Bank (loans/interest/NPL),
  * 4=Capital (investment), 5=Foreign (FDI/profit shifting), 6=BondMarket
  */
object FirmFlows:

  val FIRM_ACCOUNT: Int       = 0
  val HH_ACCOUNT: Int         = 1
  val GOV_ACCOUNT: Int        = 2
  val BANK_ACCOUNT: Int       = 3
  val CAPITAL_ACCOUNT: Int    = 4
  val FOREIGN_ACCOUNT: Int    = 5
  val BONDMARKET_ACCOUNT: Int = 6

  case class Input(
      wages: PLN,
      cit: PLN,
      loanRepayment: PLN,
      newLoans: PLN,
      interestPaid: PLN,
      capex: PLN,
      equityIssuance: PLN,
      bondIssuance: PLN,
      ioPayments: PLN,
      nplDefault: PLN,
      profitShifting: PLN,
      fdiRepatriation: PLN,
      grossInvestment: PLN,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Households,
        topology.households.aggregate,
        input.wages,
        AssetType.Cash,
        FlowMechanism.FirmWages,
      ),
      AggregateBatchedEmission
        .transfer(
          EntitySector.Firms,
          topology.firms.aggregate,
          EntitySector.Government,
          TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
          input.cit,
          AssetType.Cash,
          FlowMechanism.FirmCit,
        ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.loanRepayment,
        AssetType.FirmLoan,
        FlowMechanism.FirmLoanRepayment,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.newLoans,
        AssetType.FirmLoan,
        FlowMechanism.FirmNewLoan,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.interestPaid,
        AssetType.Cash,
        FlowMechanism.FirmInterestPaid,
      ),
      AggregateBatchedEmission
        .transfer(
          EntitySector.Firms,
          topology.firms.aggregate,
          EntitySector.Firms,
          topology.firms.capitalGoods,
          input.capex,
          AssetType.Cash,
          FlowMechanism.FirmCapex,
        ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.investors,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.equityIssuance,
        AssetType.Equity,
        FlowMechanism.FirmEquityIssuance,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.bondMarket,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.bondIssuance,
        AssetType.CorpBond,
        FlowMechanism.FirmBondIssuance,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Firms,
        topology.firms.ioCounterparty,
        input.ioPayments,
        AssetType.Cash,
        FlowMechanism.FirmIoPayment,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.nplDefault,
        AssetType.FirmLoan,
        FlowMechanism.FirmNplDefault,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Foreign,
        ForeignRuntimeContract.IncomeSettlement.index,
        input.profitShifting,
        AssetType.Cash,
        FlowMechanism.FirmProfitShifting,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Foreign,
        ForeignRuntimeContract.IncomeSettlement.index,
        input.fdiRepatriation,
        AssetType.Cash,
        FlowMechanism.FirmFdiRepatriation,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Firms,
        topology.firms.capitalGoods,
        input.grossInvestment,
        AssetType.Cash,
        FlowMechanism.FirmGrossInvestment,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    if input.wages > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HH_ACCOUNT, input.wages.toLong, FlowMechanism.FirmWages.toInt)
    if input.cit > PLN.Zero then flows += Flow(FIRM_ACCOUNT, GOV_ACCOUNT, input.cit.toLong, FlowMechanism.FirmCit.toInt)
    if input.loanRepayment > PLN.Zero then flows += Flow(FIRM_ACCOUNT, BANK_ACCOUNT, input.loanRepayment.toLong, FlowMechanism.FirmLoanRepayment.toInt)
    if input.newLoans > PLN.Zero then flows += Flow(BANK_ACCOUNT, FIRM_ACCOUNT, input.newLoans.toLong, FlowMechanism.FirmNewLoan.toInt)
    if input.interestPaid > PLN.Zero then flows += Flow(FIRM_ACCOUNT, BANK_ACCOUNT, input.interestPaid.toLong, FlowMechanism.FirmInterestPaid.toInt)
    if input.capex > PLN.Zero then flows += Flow(FIRM_ACCOUNT, FOREIGN_ACCOUNT, input.capex.toLong, FlowMechanism.FirmCapex.toInt)
    if input.equityIssuance > PLN.Zero then flows += Flow(HH_ACCOUNT, FIRM_ACCOUNT, input.equityIssuance.toLong, FlowMechanism.FirmEquityIssuance.toInt)
    if input.bondIssuance > PLN.Zero then flows += Flow(BONDMARKET_ACCOUNT, FIRM_ACCOUNT, input.bondIssuance.toLong, FlowMechanism.FirmBondIssuance.toInt)
    if input.ioPayments > PLN.Zero then flows += Flow(FIRM_ACCOUNT, FIRM_ACCOUNT + 100, input.ioPayments.toLong, FlowMechanism.FirmIoPayment.toInt)
    if input.nplDefault > PLN.Zero then flows += Flow(FIRM_ACCOUNT, BANK_ACCOUNT, input.nplDefault.toLong, FlowMechanism.FirmNplDefault.toInt)
    if input.profitShifting > PLN.Zero then flows += Flow(FIRM_ACCOUNT, FOREIGN_ACCOUNT, input.profitShifting.toLong, FlowMechanism.FirmProfitShifting.toInt)
    if input.fdiRepatriation > PLN.Zero then flows += Flow(FIRM_ACCOUNT, FOREIGN_ACCOUNT, input.fdiRepatriation.toLong, FlowMechanism.FirmFdiRepatriation.toInt)
    if input.grossInvestment > PLN.Zero then flows += Flow(FIRM_ACCOUNT, CAPITAL_ACCOUNT, input.grossInvestment.toLong, FlowMechanism.FirmGrossInvestment.toInt)

    flows.result()
