package sfc.init

import sfc.config.SimParams
import sfc.agents.*
import sfc.types.*

/** Factory for banking sector initialization. */
object BankInit:

  /** Initialize multi-bank sector (always 7 banks). Per-bank consumer loan
    * override from actual HH sums.
    */
  def create(firms: Vector[Firm.State], households: Vector[Household.State])(using p: SimParams): Banking.State =
    // Consumer loans passed as 0.0 — per-bank values are set from actual HH sums below
    val bs0 = Banking.initialize(
      p.banking.initDeposits.toDouble,
      p.banking.initCapital.toDouble,
      p.banking.initLoans.toDouble,
      p.banking.initGovBonds.toDouble,
      0.0,
      Banking.DefaultConfigs,
    )

    // Override per-bank consumer loans with actual per-bank HH consumer debt sums
    val perBankCcDebt: Map[Int, Double] =
      households.groupMapReduce(_.bankId.toInt)(_.consumerDebt.toDouble)(_ + _)
    val fixedBanks                      = bs0.banks.map(b => b.copy(consumerLoans = PLN(perBankCcDebt.getOrElse(b.id.toInt, 0.0))))

    bs0.copy(banks = fixedBanks)
