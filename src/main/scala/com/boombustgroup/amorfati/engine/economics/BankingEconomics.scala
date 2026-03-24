package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Pure economic results from banking sector — no state mutation, no flows.
  *
  * Wraps BankUpdateStep.run() and extracts aggregate monetary values needed by
  * flow mechanisms. Per-bank routing, interbank clearing, bond waterfall,
  * failure cascade stay in old step for now.
  */
object BankingEconomics:

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
  )

  def compute(
      w: com.boombustgroup.amorfati.engine.World,
      @scala.annotation.unused firms: Vector[com.boombustgroup.amorfati.agents.Firm.State],
      @scala.annotation.unused households: Vector[com.boombustgroup.amorfati.agents.Household.State],
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
      depositRng: Random,
  )(using SimParams): Result =
    val s9 = BankUpdateStep.run(BankUpdateStep.Input(w, s1, s2, s3, s4, s5, s6, s7, s8, depositRng))
    Result(
      govBondIncome = w.bank.govBondHoldings * w.gov.bondYield.monthly,
      reserveInterest = s9.monAgg.map(_ => w.plumbing.reserveInterestTotal).getOrElse(PLN.Zero),
      standingFacilityIncome = w.plumbing.standingFacilityNet,
      interbankInterest = w.plumbing.interbankInterestNet,
      bfgLevy = s9.bfgLevy,
      unrealizedBondLoss = s9.unrealizedBondLoss,
      bailInLoss = s9.bailInLoss,
      nbpRemittance = PLN.Zero,
      mortgageInterestIncome = s9.mortgageInterestIncome,
      mortgagePrincipal = s9.mortgagePrincipal,
      mortgageDefaultLoss = s9.mortgageDefaultLoss,
      mortgageDefaultAmount = s9.mortgageDefaultAmount,
    )
