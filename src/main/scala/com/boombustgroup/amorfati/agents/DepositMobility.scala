package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** Deposit mobility: delayed deposit-account reassignment after bank stress.
  *
  * Models two channels of household bank switching:
  *
  *   1. '''Health-based flight''' — households observe their bank's health (CAR
  *      ratio). When CAR drops below a threshold, a fraction of depositors
  *      switch to the healthiest bank. Gradual, rational response.
  *   2. '''Panic contagion''' — when any bank fails this month, depositors at
  *      other banks panic-switch with probability `panicRate`. Diamond & Dybvig
  *      (1983) threshold effect. SVB/Credit Suisse/Idea Bank channel.
  *
  * Deposit switching is a delayed boundary-routing contract. This module only
  * updates household `bankId` assignments. It does not emit same-month monetary
  * batches and it does not move bank balance-sheet deposits in month `t`; those
  * assignments affect future household income, consumption, debt-service, and
  * deposit-interest routing from the next boundary onward.
  *
  * Input household `bankId` values must already point to surviving banks in the
  * current bank vector. Failure resolution is responsible for reassigning
  * failed-bank clients before this module runs; unknown or stale bank IDs fail
  * fast instead of receiving generic system-average switching behavior.
  *
  * Pure function — no mutable state. Called from BankingEconomics after failure
  * resolution.
  *
  * Calibration: BFG deposit guarantee data, KNF deposit concentration, NBP
  * Financial Stability Report.
  */
object DepositMobility:

  /** Result of deposit mobility: households with updated `bankId` assignments.
    * No transfer plan is carried here by design. Deposit flows take effect next
    * month when household flows route to the new bank (1-month lag, consistent
    * with account transfer time).
    */
  case class Result(households: Vector[Household.State])

  /** Compute probability of a household switching banks.
    *
    * P(switch) = healthFlight + panicFlight, clamped to [0, maxSwitchRate]
    *
    * healthFlight = sensitivity × max(0, carThreshold − bankCAR) panicFlight =
    * panicRate if any bank failed this month, else 0
    */
  private def switchProbability(
      bankCar: Multiplier,
      anyBankFailed: Boolean,
  )(using p: SimParams): Share =
    val carGap       = (p.banking.depositFlightCarThreshold - bankCar).max(Multiplier.Zero)
    val healthFlight = p.banking.depositFlightSensitivity * carGap // Coefficient * Multiplier → Share
    val panicFlight  = if anyBankFailed then p.banking.depositPanicRate else Share.Zero
    (healthFlight + panicFlight).min(p.banking.maxDepositSwitchRate)

  private def validateHouseholdBankIds(households: Vector[Household.State], banks: Vector[Banking.BankState]): Unit =
    households.foreach: hh =>
      val bankIndex = hh.bankId.toInt
      require(
        bankIndex >= 0 && bankIndex < banks.length,
        s"DepositMobility requires household ${hh.id.toInt} to reference a known bankId, got $bankIndex for ${banks.length} banks",
      )
      val bank      = banks(bankIndex)
      require(
        bank.id == hh.bankId,
        s"DepositMobility requires bankId/index alignment for household ${hh.id.toInt}, got bankId ${hh.bankId.toInt} at index $bankIndex with bank ${bank.id.toInt}",
      )
      require(
        !bank.failed,
        s"DepositMobility requires household ${hh.id.toInt} to be reassigned away from failed bank ${hh.bankId.toInt} before mobility runs",
      )

  /** Run deposit mobility: households may switch from weak banks to the
    * healthiest bank.
    *
    * @param households
    *   current household states (with bankId assignments)
    * @param banks
    *   current bank states (after resolution)
    * @param anyBankFailed
    *   whether any bank failed this month (triggers panic channel)
    * @param rng
    *   deterministic RNG for switch decisions
    */
  def apply(
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      bankFinancialStocks: Vector[Banking.BankFinancialStocks],
      anyBankFailed: Boolean,
      rng: RandomStream,
      bankCorpBondHoldings: Banking.BankCorpBondHoldings = Banking.noBankCorpBondHoldings,
  )(using p: SimParams): Result =
    validateHouseholdBankIds(households, banks)
    val healthiest = Banking.healthiestBankId(banks, bankFinancialStocks, bankCorpBondHoldings)
    val carByIndex = banks
      .zip(bankFinancialStocks)
      .map((b, stocks) => Banking.car(b, stocks, bankCorpBondHoldings(b.id)))

    val updated = households.map: hh =>
      val currentCar = carByIndex(hh.bankId.toInt)
      val prob       = switchProbability(currentCar, anyBankFailed)
      if prob > Share.Zero && prob.sampleBelow(rng) && hh.bankId != healthiest then hh.copy(bankId = healthiest)
      else hh

    Result(updated)
