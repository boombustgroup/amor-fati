package com.boombustgroup.amorfati.accounting

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household}
import com.boombustgroup.amorfati.engine.ledger.GovernmentBondCircuit
import com.boombustgroup.amorfati.types.*

/** Pure init-time stock validation.
  *
  * SFC validates flow consistency (delta-stock = sum-flows) every month, but
  * never checks that initial stocks are internally consistent. This object
  * provides level checks at t=0: bond clearing, interbank netting, per-bank
  * deposit/loan consistency, and aggregate cross-checks.
  *
  * The caller decides the reaction (throw, log, ignore).
  */
object InitCheck:

  case class InitCheckResult(
      identity: String,
      expected: PLN,
      actual: PLN,
      passed: Boolean,
  )

  class InitValidationException(val errors: Vector[InitCheckResult])
      extends RuntimeException({
        import ComputationBoundary.toDouble
        errors.map(e => f"${e.identity}: expected=${toDouble(e.expected)}%.2f actual=${toDouble(e.actual)}%.2f").mkString("; ")
      })

  /** Validate initial stock identities. Returns only the failing checks (empty =
    * all pass).
    */
  def validate(state: Sfc.RuntimeState): Vector[InitCheckResult] =
    validate(Sfc.snapshot(state), state.banks, state.firms, state.households)

  def validate(
      snapshot: Sfc.StockState,
      banks: Vector[Banking.BankState],
      firms: Vector[Firm.State],
      households: Vector[Household.State],
  ): Vector[InitCheckResult] =
    val levelTol   = PLN(0.01)
    val perBankTol = PLN(1.0)
    val bonds      = GovernmentBondCircuit(
      outstanding = snapshot.bondsOutstanding,
      bankHoldings = snapshot.bankBondHoldings,
      foreignHoldings = snapshot.foreignBondHoldings,
      nbpHoldings = snapshot.nbpBondHoldings,
      insuranceHoldings = snapshot.insuranceGovBondHoldings,
      ppkHoldings = snapshot.ppkBondHoldings,
      tfiHoldings = snapshot.tfiGovBondHoldings,
    )

    // --- Level checks (reuse Sfc formulas) ---

    val bondClearing = check(
      "Bond clearing",
      expected = bonds.outstanding,
      actual = bonds.totalHoldings,
      levelTol,
    )

    val interbankNetting = check(
      "Interbank netting",
      expected = PLN.Zero,
      actual = snapshot.interbankNetSum,
      levelTol,
    )

    // --- Per-bank agent cross-checks ---

    val firmCashByBank   = firms.groupMapReduce(_.bankId.toInt)(_.cash)(_ + _)
    val firmDebtByBank   = firms.groupMapReduce(_.bankId.toInt)(_.debt)(_ + _)
    val hhSavingsByBank  = households.groupMapReduce(_.bankId.toInt)(_.savings)(_ + _)
    val hhConsDebtByBank = households.groupMapReduce(_.bankId.toInt)(_.consumerDebt)(_ + _)

    val perBankChecks = banks.flatMap: bank =>
      val bId = bank.id.toInt

      val expectedDeposits = firmCashByBank.getOrElse(bId, PLN.Zero) + hhSavingsByBank.getOrElse(bId, PLN.Zero)
      val depositCheck     = check(
        s"Deposit consistency (bank $bId)",
        expected = expectedDeposits,
        actual = bank.deposits,
        perBankTol,
      )

      val expectedCorpLoans = firmDebtByBank.getOrElse(bId, PLN.Zero)
      val corpLoanCheck     = check(
        s"Corp loan consistency (bank $bId)",
        expected = expectedCorpLoans,
        actual = bank.loans,
        perBankTol,
      )

      val expectedConsLoans = hhConsDebtByBank.getOrElse(bId, PLN.Zero)
      val consLoanCheck     = check(
        s"Consumer loan consistency (bank $bId)",
        expected = expectedConsLoans,
        actual = bank.consumerLoans,
        perBankTol,
      )

      Vector(depositCheck, corpLoanCheck, consLoanCheck)

    // --- Aggregate cross-checks ---

    val aggDeposits     = PLN.fromRaw(banks.map(_.deposits.toLong).sum)
    val aggDepositCheck = check(
      "Aggregate deposits",
      expected = aggDeposits,
      actual = snapshot.bankDeposits,
      levelTol,
    )

    val aggLoans     = banks.map(_.loans).sum
    val aggLoanCheck = check(
      "Aggregate loans",
      expected = aggLoans,
      actual = snapshot.bankLoans,
      levelTol,
    )

    val all = Vector(bondClearing, interbankNetting) ++ perBankChecks ++ Vector(aggDepositCheck, aggLoanCheck)
    all.filterNot(_.passed)

  private def check(identity: String, expected: PLN, actual: PLN, tolerance: PLN): InitCheckResult =
    InitCheckResult(
      identity = identity,
      expected = expected,
      actual = actual,
      passed = (actual - expected).abs < tolerance,
    )
