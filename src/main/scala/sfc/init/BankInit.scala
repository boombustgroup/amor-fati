package sfc.init

import sfc.agents.*
import sfc.config.Config
import sfc.types.*
import sfc.util.KahanSum.*

/** Factory for banking sector initialization. */
object BankInit:

  /** Initialize multi-bank sector with per-bank consumer loan override.
    * Returns None in single-bank mode.
    */
  def create(firms: Array[Firm.State], households: Option[Vector[Household.State]]): Option[Banking.State] =
    if !Config.BankMulti then return None

    val initConsumerLoans = households.map(_.kahanSumBy(_.consumerDebt.toDouble)).getOrElse(Config.InitConsumerLoans)
    val bs0 = Banking.initialize(Config.InitBankDeposits, Config.InitBankCapital,
      Config.InitBankLoans, Config.InitBankGovBonds, initConsumerLoans,
      Banking.DefaultConfigs)

    // Override per-bank consumer loans with actual per-bank HH consumer debt sums
    val fixedBanks = households match
      case Some(hhs) =>
        val nBanks = bs0.banks.length
        val perBankCcDebt = new Array[Double](nBanks)
        hhs.foreach(h => if h.bankId.toInt >= 0 && h.bankId.toInt < nBanks then perBankCcDebt(h.bankId.toInt) += h.consumerDebt.toDouble)
        bs0.banks.map(b => b.copy(consumerLoans = PLN(perBankCcDebt(b.id.toInt))))
      case None => bs0.banks

    Some(bs0.copy(banks = fixedBanks))
