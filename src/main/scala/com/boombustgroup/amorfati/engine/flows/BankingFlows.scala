package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.{NbpRuntimeContract, TreasuryRuntimeContract}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Banking sector P&L emitting flows.
  *
  * Aggregate level: all income and loss items that affect bank capital. Deposit
  * interest and consumer credit flows already in HouseholdFlows. Firm loan
  * interest and NPL already in FirmFlows. Mortgage flows already in
  * MortgageFlows. Corporate bond principal flows are in CorpBondFlows; bank
  * loss recognition is capital P&L here.
  *
  * This mechanism captures the REMAINING bank-specific flows: gov bond income,
  * reserve/standing facility/interbank interest, BFG levy, unrealized bond
  * losses, bail-in, NBP remittance.
  *
  * Account IDs: 0=Bank, 1=NBP, 2=Gov (BFG levy), 3=Depositors (bail-in), 4=Corp
  * bond loss settlement
  */
object BankingFlows:

  val BANK_ACCOUNT: Int      = 0
  val NBP_ACCOUNT: Int       = 1
  val GOV_ACCOUNT: Int       = 2
  val DEPOSITOR_ACCOUNT: Int = 3
  val LOSS_ACCOUNT: Int      = 4

  case class Input(
      govBondIncome: PLN,
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      interbankInterest: PLN,
      corpBondDefaultLoss: PLN,
      bfgLevy: PLN,
      unrealizedBondLoss: PLN,
      bailInLoss: PLN,
      nbpRemittance: PLN,
      fxReserveSettlement: PLN,
      standingFacilityBackstop: PLN,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.govBondIncome,
        AssetType.Capital,
        FlowMechanism.BankGovBondIncome,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.NBP,
        NbpRuntimeContract.ReserveSettlementLiability.index,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.reserveInterest,
        NbpRuntimeContract.ReserveSettlementLiability.asset,
        FlowMechanism.BankReserveInterest,
      ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.NBP,
        NbpRuntimeContract.ReserveSettlementLiability.index,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.standingFacilityIncome,
        NbpRuntimeContract.ReserveSettlementLiability.asset,
        FlowMechanism.BankStandingFacility,
      ),
      if input.interbankInterest > PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.NBP,
          NbpRuntimeContract.ReserveSettlementLiability.index,
          EntitySector.Banks,
          topology.banks.aggregate,
          input.interbankInterest,
          NbpRuntimeContract.ReserveSettlementLiability.asset,
          FlowMechanism.BankInterbankInterest,
        )
      else if input.interbankInterest < PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Banks,
          topology.banks.aggregate,
          EntitySector.NBP,
          NbpRuntimeContract.ReserveSettlementLiability.index,
          -input.interbankInterest,
          NbpRuntimeContract.ReserveSettlementLiability.asset,
          FlowMechanism.BankInterbankInterest,
        )
      else Vector.empty,
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Funds,
        topology.funds.bondholders,
        input.corpBondDefaultLoss,
        AssetType.Capital,
        FlowMechanism.BankCorpBondLoss,
      ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.NBP,
        NbpRuntimeContract.ReserveSettlementLiability.index,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.fxReserveSettlement,
        NbpRuntimeContract.ReserveSettlementLiability.asset,
        FlowMechanism.NbpFxSettlement,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.NBP,
        NbpRuntimeContract.StandingFacilityBackstop.index,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.standingFacilityBackstop,
        NbpRuntimeContract.StandingFacilityBackstop.asset,
        FlowMechanism.BankStandingFacilityBackstop,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.bfgLevy,
        AssetType.Capital,
        FlowMechanism.BankBfgLevy,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.unrealizedBondLoss,
        AssetType.Capital,
        FlowMechanism.BankUnrealizedLoss,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.depositors,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.bailInLoss,
        AssetType.DemandDeposit,
        FlowMechanism.BankBailIn,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.nbpRemittance,
        AssetType.Capital,
        FlowMechanism.BankNbpRemittance,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    // Income
    if input.govBondIncome > PLN.Zero then flows += Flow(GOV_ACCOUNT, BANK_ACCOUNT, input.govBondIncome.toLong, FlowMechanism.BankGovBondIncome.toInt)
    if input.reserveInterest > PLN.Zero then flows += Flow(NBP_ACCOUNT, BANK_ACCOUNT, input.reserveInterest.toLong, FlowMechanism.BankReserveInterest.toInt)
    if input.standingFacilityIncome > PLN.Zero then
      flows += Flow(NBP_ACCOUNT, BANK_ACCOUNT, input.standingFacilityIncome.toLong, FlowMechanism.BankStandingFacility.toInt)
    else if input.standingFacilityIncome < PLN.Zero then
      flows += Flow(BANK_ACCOUNT, NBP_ACCOUNT, (-input.standingFacilityIncome).toLong, FlowMechanism.BankStandingFacility.toInt)

    // Interbank: signed — positive = net income, negative = net cost
    if input.interbankInterest > PLN.Zero then
      flows += Flow(NBP_ACCOUNT, BANK_ACCOUNT, input.interbankInterest.toLong, FlowMechanism.BankInterbankInterest.toInt)
    else if input.interbankInterest < PLN.Zero then
      flows += Flow(BANK_ACCOUNT, NBP_ACCOUNT, (-input.interbankInterest).toLong, FlowMechanism.BankInterbankInterest.toInt)
    if input.corpBondDefaultLoss > PLN.Zero then
      flows += Flow(BANK_ACCOUNT, LOSS_ACCOUNT, input.corpBondDefaultLoss.toLong, FlowMechanism.BankCorpBondLoss.toInt)
    if input.fxReserveSettlement > PLN.Zero then flows += Flow(NBP_ACCOUNT, BANK_ACCOUNT, input.fxReserveSettlement.toLong, FlowMechanism.NbpFxSettlement.toInt)
    else if input.fxReserveSettlement < PLN.Zero then
      flows += Flow(BANK_ACCOUNT, NBP_ACCOUNT, (-input.fxReserveSettlement).toLong, FlowMechanism.NbpFxSettlement.toInt)
    if input.standingFacilityBackstop > PLN.Zero then
      flows += Flow(NBP_ACCOUNT, BANK_ACCOUNT, input.standingFacilityBackstop.toLong, FlowMechanism.BankStandingFacilityBackstop.toInt)

    // Losses / outflows
    if input.bfgLevy > PLN.Zero then flows += Flow(BANK_ACCOUNT, GOV_ACCOUNT, input.bfgLevy.toLong, FlowMechanism.BankBfgLevy.toInt)
    if input.unrealizedBondLoss > PLN.Zero then
      flows += Flow(BANK_ACCOUNT, GOV_ACCOUNT, input.unrealizedBondLoss.toLong, FlowMechanism.BankUnrealizedLoss.toInt)
    if input.bailInLoss > PLN.Zero then flows += Flow(DEPOSITOR_ACCOUNT, BANK_ACCOUNT, input.bailInLoss.toLong, FlowMechanism.BankBailIn.toInt)
    if input.nbpRemittance > PLN.Zero then flows += Flow(BANK_ACCOUNT, GOV_ACCOUNT, input.nbpRemittance.toLong, FlowMechanism.BankNbpRemittance.toInt)

    flows.result()
