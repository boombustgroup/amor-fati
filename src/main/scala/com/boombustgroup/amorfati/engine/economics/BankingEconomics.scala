package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Banking sector economics — aggregate values for MonthlyCalculus.
  *
  * Internally delegates to BankUpdateStep (deposit routing, bond waterfall,
  * failure cascade — 700 lines). Own Input takes raw values where possible,
  * Step.Output types where unavoidable (s3, s5, s7, s8 are too complex to
  * decompose without rewriting BankUpdateStep).
  *
  * Full decoupling happens when BankUpdateStep is rewritten as micro-pipeline
  * (#131).
  */
object BankingEconomics:

  case class Input(
      w: World,
      // Raw values from earlier calculus (avoids Step.Output dependency)
      month: Int,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      employed: Int,
      newWage: PLN,
      laborDemand: Int,
      wageGrowth: Coefficient,
      govPurchases: PLN,
      sectorMults: Vector[Double],
      avgDemandMult: Double,
      sectorCap: Vector[Double],
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      // Step outputs too complex to decompose (will vanish with #131)
      laborOutput: LaborDemographicsStep.Output,
      hhOutput: HouseholdIncomeStep.Output,
      firmOutput: FirmProcessingStep.Output,
      hhFinancialOutput: HouseholdFinancialStep.Output,
      priceEquityOutput: PriceEquityStep.Output,
      openEconOutput: OpenEconomyStep.Output,
      depositRng: Random,
  )

  case class Result(
      govBondIncome: PLN,
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      interbankInterest: PLN,
      bfgLevy: PLN,
      unrealizedBondLoss: PLN,
      bailInLoss: PLN,
      nbpRemittance: PLN,
      mortgageInterestIncome: PLN,
      mortgagePrincipal: PLN,
      mortgageDefaultLoss: PLN,
      mortgageDefaultAmount: PLN,
      // Non-monetary outputs needed by WorldAssembly
      effectiveShadowShare: Share,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      actualBondChange: PLN,
      eclProvisionChange: PLN,
      htmRealizedLoss: PLN,
  )

  def compute(in: Input)(using p: SimParams): Result =
    val s1 = FiscalConstraintStep.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)
    val s4 = DemandStep.Output(in.govPurchases, in.sectorMults, in.avgDemandMult, in.sectorCap, in.laggedInvestDemand, in.fiscalRuleStatus)

    val s9 = BankUpdateStep.run(
      BankUpdateStep.Input(
        in.w,
        s1,
        in.laborOutput,
        in.hhOutput,
        s4,
        in.firmOutput,
        in.hhFinancialOutput,
        in.priceEquityOutput,
        in.openEconOutput,
        in.depositRng,
      ),
    )

    Result(
      govBondIncome = in.w.bank.govBondHoldings * in.openEconOutput.monetary.newBondYield.monthly,
      reserveInterest = in.openEconOutput.banking.totalReserveInterest,
      standingFacilityIncome = in.openEconOutput.banking.totalStandingFacilityIncome,
      interbankInterest = in.openEconOutput.banking.totalInterbankInterest,
      bfgLevy = s9.bfgLevy,
      unrealizedBondLoss = s9.unrealizedBondLoss,
      bailInLoss = s9.bailInLoss,
      nbpRemittance = in.openEconOutput.banking.nbpRemittance,
      mortgageInterestIncome = s9.mortgageInterestIncome,
      mortgagePrincipal = s9.mortgagePrincipal,
      mortgageDefaultLoss = s9.mortgageDefaultLoss,
      mortgageDefaultAmount = s9.mortgageDefaultAmount,
      effectiveShadowShare = s9.effectiveShadowShare,
      jstDepositChange = s9.jstDepositChange,
      investNetDepositFlow = s9.investNetDepositFlow,
      actualBondChange = s9.actualBondChange,
      eclProvisionChange = s9.eclProvisionChange,
      htmRealizedLoss = s9.htmRealizedLoss,
    )
