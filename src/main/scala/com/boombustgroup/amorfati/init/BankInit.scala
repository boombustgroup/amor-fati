package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Banking sector initialization from actual agent populations.
  *
  * Computes per-bank balances from firm and household populations, ensuring the
  * initial bank state is consistent with agent-level bank assignments.
  */
object BankInit:

  def create(firms: Vector[Firm.State], households: Vector[Household.State])(using p: SimParams): Banking.State =
    val perBankCorpLoans  = firms.groupMapReduce(_.bankId.toInt)(_.debt)(_ + _)
    val perBankCash       = firms.groupMapReduce(_.bankId.toInt)(_.cash)(_ + _)
    val perBankConsLoans  = households.groupMapReduce(_.bankId.toInt)(_.consumerDebt)(_ + _)
    val perBankHhDeposits = households.groupMapReduce(_.bankId.toInt)(_.savings)(_ + _)

    val totalCapital  = p.banking.initCapital
    val totalGovBonds = p.banking.initGovBonds

    val banks = Banking.DefaultConfigs.map: cfg =>
      val bId          = cfg.id.toInt
      val corpLoans    = perBankCorpLoans.getOrElse(bId, PLN.Zero)
      val consLoans    = perBankConsLoans.getOrElse(bId, PLN.Zero)
      val firmDeposits = perBankCash.getOrElse(bId, PLN.Zero)
      val hhDeposits   = perBankHhDeposits.getOrElse(bId, PLN.Zero)
      Banking.BankState(
        id = cfg.id,
        deposits = firmDeposits + hhDeposits,
        loans = corpLoans,
        capital = totalCapital * cfg.initMarketShare,
        nplAmount = PLN.Zero,
        govBondHoldings = totalGovBonds * cfg.initMarketShare,
        reservesAtNbp = PLN.Zero,
        interbankNet = PLN.Zero,
        status = Banking.BankStatus.Active(0),
        demandDeposits = PLN.Zero,
        termDeposits = PLN.Zero,
        loansShort = PLN.Zero,
        loansMedium = PLN.Zero,
        loansLong = PLN.Zero,
        consumerLoans = consLoans,
        consumerNpl = PLN.Zero,
        corpBondHoldings = PLN.Zero,
      )

    Banking.State(banks, Rate.Zero, Banking.DefaultConfigs, None)
