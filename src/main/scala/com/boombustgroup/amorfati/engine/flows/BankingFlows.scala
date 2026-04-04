package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Banking sector P&L emitting flows.
  *
  * Aggregate level: all income and loss items that affect bank capital. Deposit
  * interest and consumer credit flows already in HouseholdFlows. Firm loan
  * interest and NPL already in FirmFlows. Mortgage flows already in
  * MortgageFlows. Corp bond flows already in CorpBondFlows.
  *
  * This mechanism captures the REMAINING bank-specific flows: gov bond income,
  * reserve/standing facility/interbank interest, BFG levy, unrealized bond
  * losses, bail-in, NBP remittance.
  *
  * Account IDs: 0=Bank, 1=NBP, 2=Gov (BFG levy), 3=Depositors (bail-in)
  */
object BankingFlows:

  val BANK_ACCOUNT: Int      = 0
  val NBP_ACCOUNT: Int       = 1
  val GOV_ACCOUNT: Int       = 2
  val DEPOSITOR_ACCOUNT: Int = 3

  case class Input(
      govBondIncome: PLN,
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      interbankInterest: PLN,
      bfgLevy: PLN,
      unrealizedBondLoss: PLN,
      bailInLoss: PLN,
      nbpRemittance: PLN,
  )

  def emitBatches(input: Input): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        GovernmentIndex.Budget,
        EntitySector.Banks,
        BankIndex.Aggregate,
        input.govBondIncome,
        AssetType.Cash,
        FlowMechanism.BankGovBondIncome,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.NBP,
        NbpIndex.Aggregate,
        EntitySector.Banks,
        BankIndex.Aggregate,
        input.reserveInterest,
        AssetType.Cash,
        FlowMechanism.BankReserveInterest,
      ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.NBP,
        NbpIndex.Aggregate,
        EntitySector.Banks,
        BankIndex.Aggregate,
        input.standingFacilityIncome,
        AssetType.Cash,
        FlowMechanism.BankStandingFacility,
      ),
      if input.interbankInterest > PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.NBP,
          NbpIndex.Aggregate,
          EntitySector.Banks,
          BankIndex.Aggregate,
          input.interbankInterest,
          AssetType.Cash,
          FlowMechanism.BankInterbankInterest,
        )
      else if input.interbankInterest < PLN.Zero then
        AggregateBatchedEmission.transfer(
          EntitySector.Banks,
          BankIndex.Aggregate,
          EntitySector.NBP,
          NbpIndex.Aggregate,
          -input.interbankInterest,
          AssetType.Cash,
          FlowMechanism.BankInterbankInterest,
        )
      else Vector.empty,
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        BankIndex.Aggregate,
        EntitySector.Government,
        GovernmentIndex.Budget,
        input.bfgLevy,
        AssetType.Cash,
        FlowMechanism.BankBfgLevy,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        BankIndex.Aggregate,
        EntitySector.Government,
        GovernmentIndex.Budget,
        input.unrealizedBondLoss,
        AssetType.Cash,
        FlowMechanism.BankUnrealizedLoss,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        HouseholdIndex.Depositors,
        EntitySector.Banks,
        BankIndex.Aggregate,
        input.bailInLoss,
        AssetType.DemandDeposit,
        FlowMechanism.BankBailIn,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        BankIndex.Aggregate,
        EntitySector.Government,
        GovernmentIndex.Budget,
        input.nbpRemittance,
        AssetType.Cash,
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

    // Losses / outflows
    if input.bfgLevy > PLN.Zero then flows += Flow(BANK_ACCOUNT, GOV_ACCOUNT, input.bfgLevy.toLong, FlowMechanism.BankBfgLevy.toInt)
    if input.unrealizedBondLoss > PLN.Zero then
      flows += Flow(BANK_ACCOUNT, GOV_ACCOUNT, input.unrealizedBondLoss.toLong, FlowMechanism.BankUnrealizedLoss.toInt)
    if input.bailInLoss > PLN.Zero then flows += Flow(DEPOSITOR_ACCOUNT, BANK_ACCOUNT, input.bailInLoss.toLong, FlowMechanism.BankBailIn.toInt)
    if input.nbpRemittance > PLN.Zero then flows += Flow(BANK_ACCOUNT, GOV_ACCOUNT, input.nbpRemittance.toLong, FlowMechanism.BankNbpRemittance.toInt)

    flows.result()
