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

  case class Result(
      banks: Vector[Banking.BankState],
      market: Banking.MarketState,
  )

  def create(
      firms: Vector[Firm.State],
      firmFinancialStocks: Vector[Firm.FinancialStocks],
      households: Vector[Household.State],
      householdFinancialStocks: Vector[Household.FinancialStocks],
  )(using p: SimParams): Result =
    require(
      firms.length == firmFinancialStocks.length,
      s"BankInit.create requires aligned firms and financial stocks, got ${firms.length} firms and ${firmFinancialStocks.length} stock rows",
    )
    require(
      households.length == householdFinancialStocks.length,
      s"BankInit.create requires aligned households and financial stocks, got ${households.length} households and ${householdFinancialStocks.length} stock rows",
    )
    val firmRows          = firms.zip(firmFinancialStocks)
    val perBankCorpLoans  = firmRows.groupMapReduce(_._1.bankId.toInt)(_._2.firmLoan)(_ + _)
    val perBankCash       = firmRows.groupMapReduce(_._1.bankId.toInt)(_._2.cash)(_ + _)
    val householdRows     = households.zip(householdFinancialStocks)
    val perBankConsLoans  = householdRows.groupMapReduce(_._1.bankId.toInt)(_._2.consumerLoan)(_ + _)
    val perBankHhDeposits = householdRows.groupMapReduce(_._1.bankId.toInt)(_._2.demandDeposit)(_ + _)

    val totalCapital  = p.banking.initCapital
    val totalGovBonds = p.banking.initGovBonds
    val bondAlloc     = com.boombustgroup.ledger.Distribute.distribute(
      totalGovBonds.toLong,
      Banking.DefaultConfigs.map(_.initMarketShare.toLong).toArray,
    )

    val banks = Banking.DefaultConfigs
      .zip(bondAlloc)
      .map { case (cfg, bankBondRaw) =>
        val bId          = cfg.id.toInt
        val corpLoans    = perBankCorpLoans.getOrElse(bId, PLN.Zero)
        val consLoans    = perBankConsLoans.getOrElse(bId, PLN.Zero)
        val firmDeposits = perBankCash.getOrElse(bId, PLN.Zero)
        val hhDeposits   = perBankHhDeposits.getOrElse(bId, PLN.Zero)
        val bankBonds    = PLN.fromRaw(bankBondRaw)
        Banking.BankState(
          id = cfg.id,
          financial = Banking.BankFinancialStocks(
            totalDeposits = firmDeposits + hhDeposits,
            firmLoan = corpLoans,
            govBondAfs = bankBonds * (Share.One - p.banking.htmShare),
            govBondHtm = bankBonds * p.banking.htmShare,
            reserve = PLN.Zero,
            interbankLoan = PLN.Zero,
            demandDeposit = PLN.Zero,
            termDeposit = PLN.Zero,
            consumerLoan = consLoans,
          ),
          capital = totalCapital * cfg.initMarketShare,
          nplAmount = PLN.Zero,
          htmBookYield = p.banking.initHtmBookYield,
          status = Banking.BankStatus.Active(0),
          loansShort = PLN.Zero,
          loansMedium = PLN.Zero,
          loansLong = PLN.Zero,
          consumerNpl = PLN.Zero,
        )
      }

    Result(
      banks = banks,
      market = Banking.MarketState(
        interbankRate = Rate.Zero,
        configs = Banking.DefaultConfigs,
        interbankCurve = None,
      ),
    )
