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

  private case class ClearedLaborMarket(
      wage: PLN,
      employed: Int,
      regionalWages: Map[Region, PLN],
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
    val living               = firms.filter(Firm.isAlive)
    val laborDemand          = living.map(f => Firm.workerCount(f)).sum
    val cleared              = clearLaborMarket(w, s1.resWage, laborDemand)
    val availableLabor       = LaborMarket.laborSupplyAtWage(cleared.wage, s1.resWage, w.derivedTotalPopulation)
    val aggregateHiringSlack = aggregateHiringSlackFactor(laborDemand, availableLabor)

    // Immigration
    val unempRateForImmig = 1.0 - cleared.employed.toDouble / w.derivedTotalPopulation
    val newImmig          = Immigration.step(w.external.immigration, households, cleared.wage, unempRateForImmig)
    val netMigration      = newImmig.monthlyInflow - newImmig.monthlyOutflow

    // Demographics
    val newDemographics = SocialSecurity.demographicsStep(w.social.demographics, cleared.employed, netMigration)

    // Wage growth
    val wageGrowth = wageGrowthFrom(w.hhAgg.marketWage, cleared.wage)

    val nBankrupt  = firms.length - living.length
    val avgWorkers = if living.nonEmpty then laborDemand / living.length else 0

    Result(
      wage = cleared.wage,
      employed = cleared.employed,
      laborDemand = laborDemand,
      wageGrowth = Coefficient(wageGrowth),
      aggregateHiringSlack = aggregateHiringSlack,
      demographics = newDemographics,
      immigration = newImmig,
      netMigration = netMigration,
      living = living,
      regionalWages = cleared.regionalWages,
      nBankruptFirms = nBankrupt,
      avgFirmWorkers = avgWorkers,
    )

  /** Reconcile labor outputs after firm-side separations and matching so
    * downstream blocks use effective post-firm labor demand rather than stale
    * inherited headcount.
    */
  @boundaryEscape
  def reconcilePostFirmStep(
      w: World,
      s1: FiscalConstraintEconomics.Output,
      pre: Output,
      postLiving: Vector[Firm.State],
      postHouseholds: Vector[Household.State],
  )(using p: SimParams): Output =
    val postLaborDemand    = postLiving.map(Firm.workerCount).sum
    val cleared            = clearLaborMarket(w, s1.resWage, postLaborDemand)
    val realizedEmployment = postHouseholds.count:
      _.status match
        case HhStatus.Employed(_, _, _) => true
        case _                          => false
    val employedCap        =
      if p.flags.demographics then Math.min(realizedEmployment, pre.newDemographics.workingAgePop)
      else realizedEmployment
    val postAvailableLabor = LaborMarket.laborSupplyAtWage(cleared.wage, s1.resWage, w.derivedTotalPopulation)
    pre.copy(
      newWage = cleared.wage,
      employed = employedCap,
      laborDemand = postLaborDemand,
      wageGrowth = Coefficient(wageGrowthFrom(w.hhAgg.marketWage, cleared.wage)),
      aggregateHiringSlack = aggregateHiringSlackFactor(postLaborDemand, postAvailableLabor),
      living = postLiving,
      regionalWages = cleared.regionalWages,
    )

  @boundaryEscape
  private def clearLaborMarket(
      w: World,
      resWage: PLN,
      laborDemand: Int,
  )(using p: SimParams): ClearedLaborMarket =
    import ComputationBoundary.toDouble
    val (rawWage, rawEmployed, regWages) =
      if p.flags.regionalLabor then
        val rc          = RegionalClearing.clear(w.regionalWages, resWage, laborDemand, w.derivedTotalPopulation)
        val natEmployed = LaborMarket.employmentAtWage(rc.nationalWage, resWage, laborDemand, w.derivedTotalPopulation)
        (toDouble(rc.nationalWage), natEmployed, rc.regionalWages)
      else
        val wageResult = LaborMarket.updateLaborMarket(w.hhAgg.marketWage, resWage, laborDemand, w.derivedTotalPopulation)
        (toDouble(wageResult.wage), wageResult.employed, w.regionalWages)

    val wageAfterExp =
      if p.flags.expectations then
        val target          = toDouble(p.monetary.targetInfl)
        val expWagePressure = toDouble(p.labor.expWagePassthrough) *
          Math.max(0.0, toDouble(w.mechanisms.expectations.expectedInflation) - target) / 12.0
        Math.max(toDouble(resWage), rawWage * (1.0 + expWagePressure))
      else rawWage

    val newWage =
      if p.flags.unions && wageAfterExp < toDouble(w.hhAgg.marketWage) then
        val aggDensity =
          p.sectorDefs.zipWithIndex.map((s, i) => toDouble(s.share) * toDouble(p.labor.unionDensity(i))).sum
        val decline    = toDouble(w.hhAgg.marketWage) - wageAfterExp
        Math.max(toDouble(resWage), wageAfterExp + decline * toDouble(p.labor.unionRigidity) * aggDensity)
      else wageAfterExp

    val employed =
      if p.flags.demographics then Math.min(rawEmployed, w.social.demographics.workingAgePop)
      else rawEmployed

    ClearedLaborMarket(PLN(newWage), employed, regWages)

  private def wageGrowthFrom(prevWage: PLN, newWage: PLN): Double =
    import ComputationBoundary.toDouble
    if toDouble(prevWage) > 0 then toDouble(newWage) / toDouble(prevWage) - 1.0 else 0.0
