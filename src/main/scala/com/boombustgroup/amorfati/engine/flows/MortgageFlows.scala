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
  * Account IDs: 0=HH borrower, 1=household mortgage book.
  */
object MortgageFlows:

  val HH_ACCOUNT: Int            = 0
  val MORTGAGE_BOOK_ACCOUNT: Int = 1

  case class Input(
      origination: PLN,
      principalRepayment: PLN,
      interest: PLN,
      defaultAmount: PLN,
      householdMortgageBalances: Vector[PLN] = Vector.empty,
      targetHouseholdMortgageBalances: Vector[PLN] = Vector.empty,
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
        val opening = normalizedOpeningBalances(input.householdMortgageBalances, count)
        if input.targetHouseholdMortgageBalances.nonEmpty then fromTargetClosingBalances(input, opening)
        else
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

    private def normalizedTargetBalances(input: Input, count: Int): Array[Long] =
      require(
        input.targetHouseholdMortgageBalances.length == count,
        s"MortgageFlows.emitBatches expected $count target household mortgage balances, got ${input.targetHouseholdMortgageBalances.length}",
      )
      input.targetHouseholdMortgageBalances.map(_.distributeRaw.max(0L)).toArray

    private def fromTargetClosingBalances(input: Input, opening: Array[Long]): HouseholdMortgageAllocations =
      val target              = normalizedTargetBalances(input, opening.length)
      val originationRaw      = input.origination.distributeRaw.max(0L)
      val repaymentRaw        = input.principalRepayment.distributeRaw.max(0L)
      val defaultRaw          = input.defaultAmount.distributeRaw.max(0L)
      val totalReductionRaw   = repaymentRaw + defaultRaw
      val openingTotal        = opening.iterator.sum
      val targetTotal         = target.iterator.sum
      val expectedTargetTotal = openingTotal + originationRaw - totalReductionRaw

      require(
        targetTotal == expectedTargetTotal,
        s"MortgageFlows.emitBatches target household mortgage total $targetTotal does not match opening $openingTotal + origination $originationRaw - reductions $totalReductionRaw = $expectedTargetTotal",
      )

      val requiredReduction      = opening.zip(target).map((from, to) => (from - to).max(0L))
      val requiredReductionTotal = requiredReduction.iterator.sum
      require(
        requiredReductionTotal <= totalReductionRaw,
        s"MortgageFlows.emitBatches target household mortgage balances require $requiredReductionTotal reduction but only $totalReductionRaw was emitted",
      )

      val extraReduction = distributeTargetResidual(
        totalReductionRaw - requiredReductionTotal,
        opening.zip(target).map((from, to) => from.max(to)),
      )
      val reduction      = add(requiredReduction, extraReduction)
      val origination    = target.indices.map(index => target(index) - opening(index) + reduction(index)).toArray
      val repayment      = allocateWithinCaps(repaymentRaw, reduction, "principalRepayment")
      val defaultAmt     = subtract(reduction, repayment)

      require(origination.iterator.sum == originationRaw, "MortgageFlows.emitBatches failed to preserve mortgage origination total")
      require(repayment.iterator.sum == repaymentRaw, "MortgageFlows.emitBatches failed to preserve mortgage repayment total")
      require(defaultAmt.iterator.sum == defaultRaw, "MortgageFlows.emitBatches failed to preserve mortgage default total")
      HouseholdMortgageAllocations(origination, repayment, defaultAmt)

    private def distributeTargetResidual(total: Long, weights: Array[Long]): Array[Long] =
      if total <= 0L then Array.fill(weights.length)(0L)
      else if weights.exists(_ > 0L) then distributeAcrossActive(PLN.fromRaw(total), weights, "targetResidual")
      else Array.fill(weights.length)(0L)

    private def allocateWithinCaps(total: Long, caps: Array[Long], fieldName: String): Array[Long] =
      require(total >= 0L, s"MortgageFlows.emitBatches cannot allocate negative $fieldName amount $total")
      if total == 0L then Array.fill(caps.length)(0L)
      else
        val capTotal    = caps.iterator.sum
        require(
          total <= capTotal,
          s"MortgageFlows.emitBatches cannot allocate $fieldName amount $total over capped mortgage reductions $capTotal",
        )
        val result      = Array.fill(caps.length)(0L)
        val capTotalBig = BigInt(capTotal)
        var allocated   = 0L
        caps.indices
          .dropRight(1)
          .foreach: index =>
            val amount = ((BigInt(total) * BigInt(caps(index))) / capTotalBig).toLong.min(caps(index))
            result(index) = amount
            allocated += amount
        var remaining   = total - allocated
        caps.indices.reverseIterator.foreach: index =>
          if remaining > 0L then
            val capacity = caps(index) - result(index)
            val amount   = remaining.min(capacity)
            result(index) += amount
            remaining -= amount
        require(remaining == 0L, s"MortgageFlows.emitBatches failed to allocate $fieldName residual $remaining")
        result

    private def distributeOrigination(amount: PLN, opening: Array[Long]): Array[Long] =
      if amount <= PLN.Zero then Array.fill(opening.length)(0L)
      else if opening.exists(_ > 0L) then distributeAcrossActive(amount, opening, "origination")
      else Array.fill(opening.length)(0L)

    private def distributeReduction(amount: PLN, balances: Array[Long], fieldName: String): Array[Long] =
      if amount <= PLN.Zero then Array.fill(balances.length)(0L)
      else if balances.exists(_ > 0L) then distributeAcrossActive(amount, balances, fieldName)
      else Array.fill(balances.length)(0L)

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
    val total = persistedAmounts.iterator.sum
    if amount <= PLN.Zero then Vector.empty
    else if topology.households.persistedCount == 0 then mortgageShellTransfer(amount, mechanism)
    else if total <= 0L then mortgageShellTransfer(amount, mechanism)
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
    else if total <= 0L then mortgageShellTransfer(amount, mechanism)
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
    if input.origination > PLN.Zero then flows += Flow(MORTGAGE_BOOK_ACCOUNT, HH_ACCOUNT, input.origination.toLong, FlowMechanism.MortgageOrigination.toInt)
    if input.principalRepayment > PLN.Zero then
      flows += Flow(HH_ACCOUNT, MORTGAGE_BOOK_ACCOUNT, input.principalRepayment.toLong, FlowMechanism.MortgageRepayment.toInt)
    if input.interest > PLN.Zero then flows += Flow(HH_ACCOUNT, MORTGAGE_BOOK_ACCOUNT, input.interest.toLong, FlowMechanism.MortgageInterest.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(HH_ACCOUNT, MORTGAGE_BOOK_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.MortgageDefault.toInt)
    flows.result()
