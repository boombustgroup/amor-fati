package sfc.init

import sfc.agents.*
import sfc.config.Config
import sfc.types.*
import sfc.util.KahanSum.*

/** Factory for banking sector initialization. */
object BankInit:

  /** Initialize multi-bank sector (always 7 banks). Per-bank consumer loan override from actual HH sums. */
  def create(firms: Vector[Firm.State], households: Vector[Household.State]): Banking.State =
    val initConsumerLoans = households.kahanSumBy(_.consumerDebt.toDouble)
    val bs0 = Banking.initialize(
      Config.InitBankDeposits,
      Config.InitBankCapital,
      Config.InitBankLoans,
      Config.InitBankGovBonds,
      initConsumerLoans,
      Banking.DefaultConfigs,
    )

    // Override per-bank consumer loans with actual per-bank HH consumer debt sums
    val nBanks = bs0.banks.length
    val perBankCcDebt = new Array[Double](nBanks)
    households.foreach(h =>
      if h.bankId.toInt >= 0 && h.bankId.toInt < nBanks then perBankCcDebt(h.bankId.toInt) += h.consumerDebt.toDouble,
    )
    val fixedBanks = bs0.banks.map(b => b.copy(consumerLoans = PLN(perBankCcDebt(b.id.toInt))))

    bs0.copy(banks = fixedBanks)
