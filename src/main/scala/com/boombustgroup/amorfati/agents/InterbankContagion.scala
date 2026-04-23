package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

/** Interbank contagion: bilateral exposures, counterparty losses, and an
  * NPL-driven liquidity-hoarding proxy.
  *
  * Models the Lehman/Bear Stearns channel: bank failure → counterparty losses
  * proportional to bilateral exposure → secondary failures. The hoarding proxy
  * is calibrated from system-wide NPL stress, not directly from realized bank
  * failures.
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
  *      reduces available liquidity, amplifying stress, but it remains separate
  *      from the realized failure/contagion-loss path.
  *
  * Pure functions — no mutable state. Called from `processInterbankAndFailures`
  * in BankingEconomics.
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
  def buildExposureMatrix(banks: Vector[Banking.BankState], financialStocks: Vector[Banking.BankFinancialStocks]): ExposureMatrix =
    require(
      banks.length == financialStocks.length,
      s"InterbankContagion.buildExposureMatrix requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    banks
      .zip(financialStocks)
      .foreach: (bank, stocks) =>
        require(
          !bank.failed || stocks.interbankLoan == PLN.Zero,
          s"InterbankContagion.buildExposureMatrix requires failed bank ${bank.id.toInt} to have zero interbankLoan, got ${stocks.interbankLoan}",
        )
    val n         = banks.length
    val nets      = banks
      .zip(financialStocks)
      .map:
        case (bank, _) if bank.failed => PLN.Zero
        case (_, stocks)              => stocks.interbankLoan
    val borrowers = nets.indices.filter(i => nets(i) < PLN.Zero).toVector
    val weights   = borrowers.map(i => (-nets(i)).toLong)
    if borrowers.isEmpty then Vector.fill(n)(Vector.fill(n)(PLN.Zero))
    else
      Vector.tabulate(n): i =>
        if nets(i) <= PLN.Zero then Vector.fill(n)(PLN.Zero)
        else
          val allocations = Distribute.distribute(nets(i).toLong, weights.toArray)
          val byBorrower  = borrowers.zip(allocations.iterator).toMap
          Vector.tabulate(n): j =>
            if i == j then PLN.Zero
            else byBorrower.get(j).fold(PLN.Zero)(PLN.fromRaw)

  /** Apply contagion losses from failed banks to their interbank
    * counterparties.
    *
    * For each failed bank j, every lender i loses
    * `exposure(i→j) × (1 − recoveryRate)` from capital. If capital goes
    * negative, bank i fails in the subsequent `checkFailures` round.
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
            if counterparty.failed && i != j then acc + exposures(i)(j) * (Share.One - recovery)
            else acc
        if loss > PLN.Zero then b.copy(capital = b.capital - loss)
        else b

  /** Compute liquidity hoarding factor: reduces interbank lending when system
    * NPL rises above threshold.
    *
    * `hoardingFactor = clamp(1 − sensitivity × (systemNPL − threshold), 0, 1)`
    *
    * At factor = 0, interbank market freezes completely (all banks hoard). At
    * factor = 1, normal interbank activity. This proxy is intentionally
    * NPL-only; realized failures and contagion losses are handled by
    * `applyContagionLosses` plus `Banking.checkFailures`.
    */
  def hoardingFactor(systemNplRatio: Share)(using p: SimParams): Share =
    val excess = (systemNplRatio - p.banking.hoardingNplThreshold).max(Share.Zero)
    (Share.One - (excess * p.banking.hoardingSensitivity).toShare).clamp(Share.Zero, Share.One)
