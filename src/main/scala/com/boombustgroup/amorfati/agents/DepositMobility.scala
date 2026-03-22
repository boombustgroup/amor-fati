package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Deposit mobility: endogenous deposit flight and bank runs.
  *
  * Models three channels of deposit switching:
  *
  *   1. '''Health-based flight''' — households observe their bank's health (CAR
  *      ratio). When CAR drops below a threshold, a fraction of depositors
  *      switch to the healthiest bank. Gradual, rational response.
  *   2. '''Panic contagion''' — when any bank fails this month, depositors at
  *      other banks panic-switch with probability `panicRate`. Diamond & Dybvig
  *      (1983) threshold effect. SVB/Credit Suisse/Idea Bank channel.
  *   3. '''Endogenous LCR runoff''' — bank-level deposit runoff rate becomes
  *      `baseRunoff + panicPremium × (1 − bankCAR/systemCAR)`. Stressed banks
  *      face higher outflows.
  *
  * Deposit switching is a zero-sum operation: total system deposits unchanged,
  * only distribution across banks changes.
  *
  * Pure function — no mutable state. Called from BankUpdateStep after failure
  * resolution.
  *
  * Calibration: BFG deposit guarantee data, KNF deposit concentration, NBP
  * Financial Stability Report.
  */
object DepositMobility:

  /** Result of deposit mobility: households with updated bankId assignments.
    * Deposit flows take effect next month when income/consumption routes to the
    * new bank (1-month lag, consistent with account transfer time).
    */
  case class Result(households: Vector[Household.State])

  /** Compute probability of a household switching banks.
    *
    * P(switch) = healthFlight + panicFlight, clamped to [0, maxSwitchRate]
    *
    * healthFlight = sensitivity × max(0, carThreshold − bankCAR) panicFlight =
    * panicRate if any bank failed this month, else 0
    */
  @computationBoundary
  private def switchProbability(
      bankCar: Multiplier,
      anyBankFailed: Boolean,
  )(using p: SimParams): Share =
    val carGap       = (p.banking.depositFlightCarThreshold - bankCar).max(Multiplier.Zero)
    val healthFlight = p.banking.depositFlightSensitivity * carGap  // Coefficient * Multiplier → Share
    val panicFlight  = if anyBankFailed then p.banking.depositPanicRate else Share.Zero
    (healthFlight + panicFlight).min(p.banking.maxDepositSwitchRate)

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
      anyBankFailed: Boolean,
      rng: Random,
  )(using p: SimParams): Result =
    val healthiest = Banking.healthiestBankId(banks)
    val carByBank  = banks.map(b => b.id.toInt -> b.car).toMap
    val systemCar  =
      if banks.nonEmpty then Multiplier.fromRaw(banks.map(_.car.toLong).sum / banks.length)
      else Multiplier.Zero

    val updated = households.map: hh =>
      val currentCar = carByBank.getOrElse(hh.bankId.toInt, systemCar)
      val prob       = switchProbability(currentCar, anyBankFailed)
      if prob > Share.Zero && prob.sampleBelow(rng) && hh.bankId != healthiest then hh.copy(bankId = healthiest)
      else hh

    Result(updated)
