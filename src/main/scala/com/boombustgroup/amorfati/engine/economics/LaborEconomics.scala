package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.{LaborMarket, RegionalClearing}
import com.boombustgroup.amorfati.types.*

/** Pure economic logic for labor market — no state mutation, no flows.
  *
  * Computes wages (Phillips curve + expectations + union rigidity), employment,
  * demographics, immigration. Output feeds into flow mechanisms.
  *
  * Extracted from LaborDemographicsStep (Calculus vs Accounting split).
  */
object LaborEconomics:

  private[amorfati] def aggregateHiringSlackFactor(laborDemand: Int, availableLabor: Int)(using p: SimParams): Double =
    if laborDemand <= 0 then 1.0
    else
      val buffered = availableLabor.toDouble * ComputationBoundary.toDouble(p.firm.aggregateLaborSlackBuffer)
      val raw      = buffered / laborDemand.toDouble
      raw.max(ComputationBoundary.toDouble(p.firm.aggregateLaborSlackFloor)).min(1.0)

  case class Result(
      wage: PLN,
      employed: Int,
      laborDemand: Int,
      wageGrowth: Coefficient,
      aggregateHiringSlack: Double = 1.0,
      demographics: SocialSecurity.DemographicsState,
      immigration: Immigration.State,
      netMigration: Int,
      living: Vector[Firm.State],
      regionalWages: Map[Region, PLN],
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
  )

  /** Bridge type — same fields as the deleted LaborDemographicsStep.Output. */
  case class Output(
      newWage: PLN,
      employed: Int,
      laborDemand: Int,
      wageGrowth: Coefficient,
      aggregateHiringSlack: Double = 1.0,
      newImmig: Immigration.State,
      netMigration: Int,
      newDemographics: SocialSecurity.DemographicsState,
      newZus: SocialSecurity.ZusState,
      newNfz: SocialSecurity.NfzState,
      newPpk: SocialSecurity.PpkState,
      rawPpkBondPurchase: PLN,
      newEarmarked: EarmarkedFunds.State,
      living: Vector[Firm.State],
      regionalWages: Map[Region, PLN],
  )

  @boundaryEscape
  def compute(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintEconomics.Output,
  )(using p: SimParams): Result =
    import ComputationBoundary.toDouble
    val living      = firms.filter(Firm.isAlive)
    val laborDemand = living.map(f => Firm.workerCount(f)).sum

    // Regional clearing: 6 independent Phillips curves → national aggregate
    val (rawWage, rawEmployed, regWages) =
      if p.flags.regionalLabor then
        val rc          = RegionalClearing.clear(w.regionalWages, s1.resWage, laborDemand, w.totalPopulation)
        val natEmployed = LaborMarket.employmentAtWage(rc.nationalWage, s1.resWage, laborDemand, w.totalPopulation)
        (toDouble(rc.nationalWage), natEmployed, rc.regionalWages)
      else
        val wageResult = LaborMarket.updateLaborMarket(w.hhAgg.marketWage, s1.resWage, laborDemand, w.totalPopulation)
        (toDouble(wageResult.wage), wageResult.employed, w.regionalWages)

    // Channel 1: Expectations-augmented wage Phillips curve
    val wageAfterExp = if p.flags.expectations then
      val target          = toDouble(p.monetary.targetInfl)
      val expWagePressure = toDouble(p.labor.expWagePassthrough) *
        Math.max(0.0, toDouble(w.mechanisms.expectations.expectedInflation) - target) / 12.0
      Math.max(toDouble(s1.resWage), rawWage * (1.0 + expWagePressure))
    else rawWage

    // Union downward wage rigidity
    val newWage = if p.flags.unions && wageAfterExp < toDouble(w.hhAgg.marketWage) then
      val aggDensity =
        p.sectorDefs.zipWithIndex.map((s, i) => toDouble(s.share) * toDouble(p.labor.unionDensity(i))).sum
      val decline    = toDouble(w.hhAgg.marketWage) - wageAfterExp
      Math.max(toDouble(s1.resWage), wageAfterExp + decline * toDouble(p.labor.unionRigidity) * aggDensity)
    else wageAfterExp

    // Demographics caps employment
    val employed =
      if p.flags.demographics then Math.min(rawEmployed, w.social.demographics.workingAgePop)
      else rawEmployed

    val availableLabor       = LaborMarket.laborSupplyAtWage(PLN(newWage), s1.resWage, w.totalPopulation)
    val aggregateHiringSlack = aggregateHiringSlackFactor(laborDemand, availableLabor)

    // Immigration
    val unempRateForImmig = 1.0 - employed.toDouble / w.totalPopulation
    val newImmig          = Immigration.step(w.external.immigration, households, PLN(newWage), unempRateForImmig)
    val netMigration      = newImmig.monthlyInflow - newImmig.monthlyOutflow

    // Demographics
    val newDemographics = SocialSecurity.demographicsStep(w.social.demographics, employed, netMigration)

    // Wage growth
    val wageGrowth = if toDouble(w.hhAgg.marketWage) > 0 then newWage / toDouble(w.hhAgg.marketWage) - 1.0 else 0.0

    val nBankrupt  = firms.length - living.length
    val avgWorkers = if living.nonEmpty then laborDemand / living.length else 0

    Result(
      wage = PLN(newWage),
      employed = employed,
      laborDemand = laborDemand,
      wageGrowth = Coefficient(wageGrowth),
      aggregateHiringSlack = aggregateHiringSlack,
      demographics = newDemographics,
      immigration = newImmig,
      netMigration = netMigration,
      living = living,
      regionalWages = regWages,
      nBankruptFirms = nBankrupt,
      avgFirmWorkers = avgWorkers,
    )
