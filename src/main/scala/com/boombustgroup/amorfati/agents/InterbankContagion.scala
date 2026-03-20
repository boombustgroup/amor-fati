package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Interbank contagion: bilateral exposures, counterparty losses, liquidity
  * hoarding.
  *
  * Models the Lehman/Bear Stearns channel: bank failure → counterparty losses
  * proportional to bilateral exposure → secondary failures → systemic freeze.
  *
  * Three mechanisms:
  *
  *   1. '''Bilateral exposure matrix''' — 7×7 matrix where entry (i,j) = amount
  *      bank i has lent to bank j on the interbank market. Generated from
  *      `clearInterbank` surplus/deficit proportional allocation.
  *   2. '''Contagion loss''' — when bank j fails, bank i loses
  *      `exposure(i→j) × (1 − recoveryRate)`. This reduces bank i capital,
  *      potentially triggering secondary failure (cascade).
  *   3. '''Liquidity hoarding''' — when system-wide NPL ratio exceeds a
  *      threshold, banks cut interbank lending by a hoarding factor. This
  *      reduces available liquidity, amplifying stress.
  *
  * Pure functions — no mutable state. Called from `processInterbankAndFailures`
  * in BankUpdateStep.
  *
  * Calibration: NBP Financial Stability Report, KNF interbank exposure data.
  */
object InterbankContagion:

  /** Bilateral exposure matrix: `exposures(i)(j)` = PLN bank i lent to bank j.
    * Diagonal is always zero (no self-lending).
    */
  type ExposureMatrix = Vector[Vector[PLN]]

  /** Build bilateral exposure matrix from interbank surplus/deficit.
    *
    * Each lender i allocates lending proportionally to each borrower j's
    * deficit share:
    * `exposure(i→j) = lender_i_surplus × (borrower_j_deficit / totalBorrowing)`.
    * This produces a dense matrix where every lender is exposed to every
    * borrower.
    */
  def buildExposureMatrix(banks: Vector[Banking.BankState]): ExposureMatrix =
    val n        = banks.length
    val nets     = banks.map(_.interbankNet.toDouble)
    val totalBor = nets.filter(_ < 0).map(-_).kahanSum
    if totalBor <= 0 then Vector.fill(n)(Vector.fill(n)(PLN.Zero))
    else
      Vector.tabulate(n): i =>
        Vector.tabulate(n): j =>
          if i == j then PLN.Zero
          else if nets(i) > 0 && nets(j) < 0 then PLN(nets(i) * (-nets(j) / totalBor))
          else PLN.Zero

  /** Apply contagion losses from failed banks to their interbank
    * counterparties.
    *
    * For each failed bank j, every lender i loses
    * `exposure(i→j) × (1 − recoveryRate)` from capital. If capital goes
    * negative, bank i may subsequently fail in the next `checkFailures` round.
    */
  def applyContagionLosses(
      banks: Vector[Banking.BankState],
      exposures: ExposureMatrix,
  )(using p: SimParams): Vector[Banking.BankState] =
    val recovery = p.banking.interbankRecoveryRate
    banks.zipWithIndex.map: (b, i) =>
      if b.failed then b
      else
        val loss = banks.zipWithIndex.foldLeft(PLN.Zero):
          case (acc, (counterparty, j)) =>
            if counterparty.failed && i != j then acc + exposures(i)(j) * (Ratio.One - recovery).toDouble
            else acc
        if loss > PLN.Zero then b.copy(capital = b.capital - loss)
        else b

  /** Compute liquidity hoarding factor: reduces interbank lending when system
    * NPL rises above threshold.
    *
    * `hoardingFactor = clamp(1 − sensitivity × (systemNPL − threshold), 0, 1)`
    *
    * At factor = 0, interbank market freezes completely (all banks hoard). At
    * factor = 1, normal interbank activity.
    */
  def hoardingFactor(systemNplRatio: Ratio)(using p: SimParams): Ratio =
    val excess = (systemNplRatio - p.banking.hoardingNplThreshold).max(Ratio.Zero)
    (Ratio.One - Ratio(excess.toDouble * p.banking.hoardingSensitivity)).clamp(Ratio.Zero, Ratio.One)

  /** Total contagion loss across all non-failed banks. */
  def totalContagionLoss(
      before: Vector[Banking.BankState],
      after: Vector[Banking.BankState],
  ): PLN =
    PLN(
      before
        .zip(after)
        .map: (pre, post) =>
          if !pre.failed then (pre.capital - post.capital).max(PLN.Zero).toDouble else 0.0
        .kahanSum,
    )
