package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Housing/mortgage market emitting flows.
  *
  * Origination increases household mortgage liabilities, while repayment and
  * default reduce them. The bank-side P&L effects are emitted separately by
  * BankingFlows, so `AssetType.MortgageLoan` stays inside the persisted
  * household borrower book instead of creating a bank-side mirror stock.
  *
  * Account IDs: 0=HH, 1=Bank
  */
object MortgageFlows:

  val HH_ACCOUNT: Int   = 0
  val BANK_ACCOUNT: Int = 1

  case class Input(
      origination: PLN,
      principalRepayment: PLN,
      interest: PLN,
      defaultAmount: PLN,
      householdMortgageBalances: Vector[PLN] = Vector.empty,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val mortgageAllocations = HouseholdMortgageAllocations.from(input)
    Vector.concat(
      mortgageIncrease(input.origination, mortgageAllocations.origination, FlowMechanism.MortgageOrigination),
      mortgageDecrease(input.principalRepayment, mortgageAllocations.repayment, FlowMechanism.MortgageRepayment),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.interest,
        AssetType.Cash,
        FlowMechanism.MortgageInterest,
      ),
      mortgageDecrease(input.defaultAmount, mortgageAllocations.defaultAmount, FlowMechanism.MortgageDefault),
    )

  private case class HouseholdMortgageAllocations(
      origination: Array[Long],
      repayment: Array[Long],
      defaultAmount: Array[Long],
  )

  private object HouseholdMortgageAllocations:
    def from(input: Input)(using topology: RuntimeLedgerTopology): HouseholdMortgageAllocations =
      val count = topology.households.persistedCount
      if count == 0 then
        HouseholdMortgageAllocations(
          origination = Array.empty[Long],
          repayment = Array.empty[Long],
          defaultAmount = Array.empty[Long],
        )
      else
        val opening     = normalizedOpeningBalances(input.householdMortgageBalances, count)
        val origination = distributeOrigination(input.origination, opening)
        val afterOrig   = add(opening, origination)
        val repayment   = distributeReduction(input.principalRepayment, afterOrig, "principalRepayment")
        val afterRepay  = subtract(afterOrig, repayment)
        val defaultAmt  = distributeReduction(input.defaultAmount, afterRepay, "defaultAmount")
        HouseholdMortgageAllocations(origination, repayment, defaultAmt)

    private def normalizedOpeningBalances(balances: Vector[PLN], count: Int): Array[Long] =
      if balances.isEmpty then Array.fill(count)(0L)
      else
        require(
          balances.length == count,
          s"MortgageFlows.emitBatches expected $count household mortgage balances, got ${balances.length}",
        )
        balances.map(_.distributeRaw.max(0L)).toArray

    private def distributeOrigination(amount: PLN, opening: Array[Long]): Array[Long] =
      if amount <= PLN.Zero then Array.fill(opening.length)(0L)
      else
        val weights = if opening.exists(_ > 0L) then opening else Array.fill(opening.length)(1L)
        distributeAcrossActive(amount, weights, "origination")

    private def distributeReduction(amount: PLN, balances: Array[Long], fieldName: String): Array[Long] =
      if amount <= PLN.Zero then Array.fill(balances.length)(0L)
      else
        require(
          balances.exists(_ > 0L),
          s"MortgageFlows.emitBatches cannot allocate $fieldName without positive household mortgage balances",
        )
        distributeAcrossActive(amount, balances, fieldName)

    private def distributeAcrossActive(amount: PLN, weights: Array[Long], fieldName: String): Array[Long] =
      val active        = weights.zipWithIndex.filter((weight, _) => weight > 0L)
      require(active.nonEmpty, s"MortgageFlows.emitBatches cannot allocate $fieldName without positive household mortgage weights")
      val activeAmounts = Distribute.distribute(amount.distributeRaw, active.map(_._1))
      val result        = Array.fill(weights.length)(0L)
      active
        .zip(activeAmounts)
        .foreach:
          case ((_, index), allocated) => result(index) = allocated
      result

    private def add(left: Array[Long], right: Array[Long]): Array[Long] =
      left.indices.map(i => left(i) + right(i)).toArray

    private def subtract(left: Array[Long], right: Array[Long]): Array[Long] =
      left.indices.map(i => left(i) - right(i)).toArray

  private def mortgageIncrease(
      amount: PLN,
      persistedAmounts: Array[Long],
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    if amount <= PLN.Zero then Vector.empty
    else if topology.households.persistedCount == 0 then mortgageShellTransfer(amount, mechanism)
    else
      Vector(
        BatchedFlow.Broadcast(
          EntitySector.Households,
          topology.households.aggregate,
          EntitySector.Households,
          persistedAmounts,
          persistedAmounts.indices.toArray,
          AssetType.MortgageLoan,
          mechanism,
        ),
      )

  private def mortgageDecrease(
      amount: PLN,
      persistedAmounts: Array[Long],
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val total = persistedAmounts.iterator.sum
    if amount <= PLN.Zero then Vector.empty
    else if topology.households.persistedCount == 0 then mortgageShellTransfer(amount, mechanism)
    else if total <= 0L then Vector.empty
    else
      Vector(
        BatchedFlow.Scatter(
          EntitySector.Households,
          EntitySector.Households,
          persistedAmounts ++ Array.fill(topology.households.sectorSize - persistedAmounts.length)(0L),
          Array.fill(topology.households.sectorSize)(topology.households.aggregate),
          AssetType.MortgageLoan,
          mechanism,
        ),
      )

  private def mortgageShellTransfer(amount: PLN, mechanism: MechanismId)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    AggregateBatchedEmission.transfer(
      EntitySector.Households,
      topology.households.aggregate,
      EntitySector.Households,
      topology.households.aggregate,
      amount,
      AssetType.MortgageLoan,
      mechanism,
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.origination > PLN.Zero then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.origination.toLong, FlowMechanism.MortgageOrigination.toInt)
    if input.principalRepayment > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.principalRepayment.toLong, FlowMechanism.MortgageRepayment.toInt)
    if input.interest > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.interest.toLong, FlowMechanism.MortgageInterest.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.MortgageDefault.toInt)
    flows.result()
