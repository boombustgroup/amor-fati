package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** New flow-based simulation pipeline.
  *
  * Contract-First Design: shaped by the 14 flow mechanism Input contracts, not
  * by old step code.
  *
  * Three stages per month:
  *   1. CALCULUS — pure economics (CES, Phillips, Taylor, Meen, Calvo)
  *   2. TRANSLATION — map calculus results to flow mechanism inputs
  *   3. PLUMBING — emit flows, apply through verified interpreter
  *
  * SFC == 0L by construction (verified interpreter). No post-hoc validation
  * needed.
  */
object FlowSimulation:

  /** All calculus results needed to feed flow mechanisms. */
  case class MonthlyCalculus(
      // Stage 1: Fiscal constraint
      month: Int,
      resWage: PLN,
      lendingBaseRate: Rate,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      // Stage 2: Labor market
      wage: PLN,
      employed: Int,
      laborDemand: Int,
      retirees: Int,
      workingAgePop: Int,
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
      // Stage 3: HH income (aggregates)
      totalIncome: PLN,
      consumption: PLN,
      domesticConsumption: PLN,
      importConsumption: PLN,
      totalRent: PLN,
      totalPit: PLN,
      totalDebtService: PLN,
      totalDepositInterest: PLN,
      totalRemittances: PLN,
      totalUnempBenefits: PLN,
      totalSocialTransfers: PLN,
      totalCcOrigination: PLN,
      totalCcDebtService: PLN,
      totalCcDefault: PLN,
      // Stage 4: Demand
      govPurchases: PLN,
      // Stage 5: Firm
      firmTax: PLN,
      firmNewLoans: PLN,
      firmPrincipal: PLN,
      firmInterestIncome: PLN,
      firmCapex: PLN,
      firmEquityIssuance: PLN,
      firmBondIssuance: PLN,
      firmIoPayments: PLN,
      firmNplLoss: PLN,
      firmProfitShifting: PLN,
      firmFdiRepatriation: PLN,
      firmGrossInvestment: PLN,
      // Stage 7: Price / Equity
      gdp: PLN,
      inflation: Rate,
      equityDomDividends: PLN,
      equityForDividends: PLN,
      equityDivTax: PLN,
      equityIssuance: PLN,
      equityReturn: Rate,
      // Stage 8: Open economy
      exports: PLN,
      totalImports: PLN,
      tourismExport: PLN,
      tourismImport: PLN,
      fdi: PLN,
      portfolioFlows: PLN,
      primaryIncome: PLN,
      euFunds: PLN,
      diasporaInflow: PLN,
      // Stage 8: Corp bonds
      corpBondCoupon: PLN,
      corpBondDefaultLoss: PLN,
      corpBondIssuance: PLN,
      corpBondAmortization: PLN,
      // Stage 8: Mortgage
      mortgageOrigination: PLN,
      mortgageRepayment: PLN,
      mortgageInterest: PLN,
      mortgageDefault: PLN,
      // Stage 9: Banking
      bankGovBondIncome: PLN,
      bankReserveInterest: PLN,
      bankStandingFacility: PLN,
      bankInterbankInterest: PLN,
      bankBfgLevy: PLN,
      bankUnrealizedLoss: PLN,
      bankBailIn: PLN,
      bankNbpRemittance: PLN,
      // Stage 8: Gov budget
      govTaxRevenue: PLN,
      govDebtService: PLN,
      govEuCofinancing: PLN,
      govCapitalSpend: PLN,
      // Insurance
      insurancePrevGovBonds: PLN,
      insurancePrevCorpBonds: PLN,
      insurancePrevEquity: PLN,
      govBondYield: Rate,
      corpBondYield: Rate,
  )

  /** Emit ALL flows from calculus results. Pure translation — no economics
    * here.
    */
  def emitAllFlows(c: MonthlyCalculus)(using p: SimParams): Vector[Flow] =
    Vector.concat(
      // Tier 1: Social funds
      ZusFlows.emit(ZusFlows.ZusInput(c.employed, c.wage, c.retirees)),
      NfzFlows.emit(NfzFlows.NfzInput(c.employed, c.wage, c.workingAgePop, c.retirees)),
      PpkFlows.emit(PpkFlows.PpkInput(c.employed, c.wage)),
      EarmarkedFlows.emit(EarmarkedFlows.Input(c.employed, c.wage, c.totalUnempBenefits, c.nBankruptFirms, c.avgFirmWorkers)),
      JstFlows.emit(JstFlows.Input(c.govTaxRevenue, c.totalIncome, c.gdp, c.laborDemand, c.totalPit)),
      // Tier 2: Agents
      HouseholdFlows.emit(
        HouseholdFlows.Input(
          c.consumption,
          c.totalRent,
          c.totalPit,
          c.totalDebtService,
          c.totalDepositInterest,
          c.totalRemittances,
          c.totalCcOrigination,
          c.totalCcDebtService,
          c.totalCcDefault,
        ),
      ),
      FirmFlows.emit(
        FirmFlows.Input(
          c.totalIncome,
          c.firmTax,
          c.firmPrincipal,
          c.firmNewLoans,
          c.firmInterestIncome,
          c.firmCapex,
          c.firmEquityIssuance,
          c.firmBondIssuance,
          c.firmIoPayments,
          c.firmNplLoss,
          c.firmProfitShifting,
          c.firmFdiRepatriation,
          c.firmGrossInvestment,
        ),
      ),
      GovBudgetFlows.emit(
        GovBudgetFlows.Input(
          c.govTaxRevenue,
          c.govPurchases,
          c.govDebtService,
          c.totalUnempBenefits,
          c.totalSocialTransfers,
          c.govEuCofinancing,
          c.govCapitalSpend,
        ),
      ),
      InsuranceFlows.emit(
        InsuranceFlows.Input(
          c.employed,
          c.wage,
          Share.One - Share.fraction(c.employed, (c.employed + 1).max(1)),
          c.insurancePrevGovBonds,
          c.insurancePrevCorpBonds,
          c.insurancePrevEquity,
          c.govBondYield,
          c.corpBondYield,
          c.equityReturn,
        ),
      ),
      // Tier 3: Financial markets
      EquityFlows.emit(EquityFlows.Input(c.equityDomDividends, c.equityForDividends, c.equityDivTax, c.equityIssuance)),
      CorpBondFlows.emit(CorpBondFlows.Input(c.corpBondCoupon, c.corpBondDefaultLoss, c.corpBondIssuance, c.corpBondAmortization)),
      MortgageFlows.emit(MortgageFlows.Input(c.mortgageOrigination, c.mortgageRepayment, c.mortgageInterest, c.mortgageDefault)),
      OpenEconFlows.emit(
        OpenEconFlows.Input(
          c.exports,
          c.totalImports,
          c.tourismExport,
          c.tourismImport,
          c.fdi,
          c.portfolioFlows,
          c.primaryIncome,
          c.euFunds,
          c.diasporaInflow,
          PLN.Zero,
        ),
      ),
      BankingFlows.emit(
        BankingFlows.Input(
          c.bankGovBondIncome,
          c.bankReserveInterest,
          c.bankStandingFacility,
          c.bankInterbankInterest,
          c.bankBfgLevy,
          c.bankUnrealizedLoss,
          c.bankBailIn,
          c.bankNbpRemittance,
        ),
      ),
    )
