package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.types.*

/** Temporary bridge between old World state and new flow mechanisms.
  *
  * Translates LaborEconomics.Result + World into XxxFlows.Input for each
  * mechanism. Deleted in Phase 4 when new pipeline replaces old steps entirely.
  *
  * Performance is irrelevant here — correctness only. This runs alongside old
  * pipeline to prove equivalence.
  */
object StateAdapter:

  /** Build ZUS flow input from labor economics result. */
  def zusInput(labor: LaborEconomics.Result): ZusFlows.ZusInput =
    ZusFlows.ZusInput(
      employed = labor.employed,
      wage = labor.wage,
      nRetirees = labor.demographics.retirees,
    )

  /** Build NFZ flow input from labor economics result. */
  def nfzInput(labor: LaborEconomics.Result): NfzFlows.NfzInput =
    NfzFlows.NfzInput(
      employed = labor.employed,
      wage = labor.wage,
      workingAge = labor.demographics.workingAgePop,
      nRetirees = labor.demographics.retirees,
    )

  /** Build PPK flow input from labor economics result. */
  def ppkInput(labor: LaborEconomics.Result): PpkFlows.PpkInput =
    PpkFlows.PpkInput(employed = labor.employed, wage = labor.wage)

  /** Build Earmarked flow input from labor economics result. */
  def earmarkedInput(labor: LaborEconomics.Result): EarmarkedFlows.Input =
    EarmarkedFlows.Input(
      employed = labor.employed,
      wage = labor.wage,
      unempBenefitSpend = PLN.Zero, // computed later in HH step
      nBankruptFirms = labor.nBankruptFirms,
      avgFirmWorkers = labor.avgFirmWorkers,
    )

  /** Build HH flow input from HH aggregates. */
  def hhInput(agg: Household.Aggregates): HouseholdFlows.Input =
    HouseholdFlows.Input(
      consumption = agg.consumption,
      rent = agg.totalRent,
      pit = agg.totalPit,
      debtService = agg.totalDebtService,
      depositInterest = agg.totalDepositInterest,
      remittances = agg.totalRemittances,
      ccOrigination = agg.totalConsumerOrigination,
      ccDebtService = agg.totalConsumerDebtService,
      ccDefault = agg.totalConsumerDefault,
    )

  /** Build Insurance flow input from world state + ledger-backed financial
    * mirrors.
    */
  def insuranceInput(w: World, ledgerFinancialState: LedgerFinancialState, labor: LaborEconomics.Result): InsuranceFlows.Input =
    val ins = ledgerFinancialState.insurance
    InsuranceFlows.Input(
      employed = labor.employed,
      wage = labor.wage,
      unempRate = w.unemploymentRate(labor.employed),
      currentLifeReserves = ins.lifeReserve,
      currentNonLifeReserves = ins.nonLifeReserve,
      prevGovBondHoldings = ins.govBondHoldings,
      prevCorpBondHoldings = ins.corpBondHoldings,
      prevEquityHoldings = ins.equityHoldings,
      govBondYield = w.gov.bondYield,
      corpBondYield = w.financial.corporateBonds.corpBondYield,
      equityReturn = w.financial.equity.monthlyReturn,
    )

  /** Build Firm flow input from FirmEconomics result. */
  def firmInput(firm: FirmEconomics.Result, totalIncome: PLN): FirmFlows.Input =
    FirmFlows.Input(
      wages = totalIncome,
      cit = firm.tax,
      loanRepayment = firm.firmPrincipal,
      newLoans = firm.newLoans,
      interestPaid = firm.intIncome,
      capex = firm.capex,
      equityIssuance = firm.equityIssuance,
      bondIssuance = firm.actualBondIssuance,
      ioPayments = firm.ioPayments,
      nplDefault = firm.nplLoss,
      profitShifting = firm.profitShifting,
      fdiRepatriation = firm.fdiRepatriation,
      grossInvestment = firm.grossInvestment,
    )
