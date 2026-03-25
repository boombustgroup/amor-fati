package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Firm sector economics — aggregate monetary values for flow mechanisms.
  *
  * Internally delegates to FirmProcessingStep (which runs per-firm CES
  * production, CAPEX decisions, financing splits, I-O market, labor matching,
  * NPL detection).
  *
  * Own Input contract — does not depend on Step.Output types in its public API.
  * FirmProcessingStep.Input is constructed internally from raw values.
  */
object FirmEconomics:

  /** Raw inputs needed from previous calculus stages. */
  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      // From FiscalConstraint
      month: Int,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      // From Labor
      newWage: PLN,
      employed: Int,
      laborDemand: Int,
      wageGrowth: Coefficient,
      immigration: Immigration.State,
      netMigration: Int,
      demographics: SocialSecurity.DemographicsState,
      zusState: SocialSecurity.ZusState,
      nfzState: SocialSecurity.NfzState,
      ppkState: SocialSecurity.PpkState,
      rawPpkBondPurchase: PLN,
      earmarked: EarmarkedFunds.State,
      living: Vector[Firm.State],
      regionalWages: Map[Region, PLN],
      // From HouseholdIncome
      hhOutput: HouseholdIncomeStep.Output,
      // From Demand
      sectorMults: Vector[Double],
      avgDemandMult: Double,
      sectorCap: Vector[Double],
      govPurchases: PLN,
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      rng: Random,
  )

  case class Result(
      tax: PLN,
      capex: PLN,
      newLoans: PLN,
      equityIssuance: PLN,
      grossInvestment: PLN,
      bondIssuance: PLN,
      actualBondIssuance: PLN,
      profitShifting: PLN,
      fdiRepatriation: PLN,
      ioPayments: PLN,
      nplLoss: PLN,
      intIncome: PLN,
      firmPrincipal: PLN,
      greenInvestment: PLN,
      energyCost: PLN,
      firmDeaths: Int,
      nplNew: PLN,
      totalBondDefault: PLN,
      corpBondAbsorption: Share,
      // Non-monetary outputs needed by later stages
      markupInflation: Rate,
      ioFirms: Vector[Firm.State],
      updatedHouseholds: Vector[Household.State],
      crossSectorHires: Int,
      techImports: PLN,
      inventoryChange: PLN,
      citEvasion: PLN,
      perBankWorkers: Vector[Int],
  )

  def compute(in: Input)(using p: SimParams): Result =
    // Construct old Step inputs from raw values
    val s1 = FiscalConstraintStep.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)
    val s2 = LaborDemographicsStep.Output(
      in.newWage,
      in.employed,
      in.laborDemand,
      in.wageGrowth,
      in.immigration,
      in.netMigration,
      in.demographics,
      in.zusState,
      in.nfzState,
      in.ppkState,
      in.rawPpkBondPurchase,
      in.earmarked,
      in.living,
      in.regionalWages,
    )
    val s4 = DemandStep.Output(in.govPurchases, in.sectorMults, in.avgDemandMult, in.sectorCap, in.laggedInvestDemand, in.fiscalRuleStatus)

    val s5 = FirmProcessingStep.run(FirmProcessingStep.Input(in.w, in.firms, in.households, s1, s2, in.hhOutput, s4), in.rng)

    Result(
      tax = s5.sumTax,
      capex = s5.sumCapex,
      newLoans = s5.sumNewLoans,
      equityIssuance = s5.sumEquityIssuance,
      grossInvestment = s5.sumGrossInvestment,
      bondIssuance = s5.sumBondIssuance,
      actualBondIssuance = s5.actualBondIssuance,
      profitShifting = s5.sumProfitShifting,
      fdiRepatriation = s5.sumFdiRepatriation,
      ioPayments = s5.totalIoPaid,
      nplLoss = s5.nplLoss,
      intIncome = s5.intIncome,
      firmPrincipal = s5.sumFirmPrincipal,
      greenInvestment = s5.sumGreenInvestment,
      energyCost = s5.sumEnergyCost,
      firmDeaths = s5.firmDeaths,
      nplNew = s5.nplNew,
      totalBondDefault = s5.totalBondDefault,
      corpBondAbsorption = s5.corpBondAbsorption,
      markupInflation = s5.markupInflation,
      ioFirms = s5.ioFirms,
      updatedHouseholds = s5.households,
      crossSectorHires = s5.postFirmCrossSectorHires,
      techImports = s5.sumTechImp,
      inventoryChange = s5.sumInventoryChange,
      citEvasion = s5.sumCitEvasion,
      perBankWorkers = s5.perBankWorkers,
    )
