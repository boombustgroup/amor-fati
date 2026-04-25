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

  private[amorfati] def operationalHiringSlackFactor(laborDemand: Int, availableLabor: Int)(using p: SimParams): Share =
    if laborDemand <= 0 then Share.One
    else
      val raw = Share.fraction(availableLabor, laborDemand) * p.firm.aggregateLaborSlackBuffer
      raw.clamp(p.firm.aggregateLaborSlackFloor, Share.One)

  private case class ClearedLaborMarket(
      wage: PLN,
      employed: Int,
      regionalWages: Map[Region, PLN],
  )

  case class Output(
      newWage: PLN,
      employed: Int,
      laborDemand: Int,
      wageGrowth: Coefficient,
      operationalHiringSlack: Share = Share.One,
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

  def compute(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintEconomics.Output,
  )(using p: SimParams): Output =
    val living                 = firms.filter(Firm.isAlive)
    val laborDemand            = living.map(f => Firm.workerCount(f)).sum
    val cleared                = clearLaborMarket(w, s1.resWage, laborDemand)
    val availableLabor         = LaborMarket.laborSupplyAtWage(cleared.wage, s1.resWage, w.derivedTotalPopulation)
    val operationalHiringSlack = operationalHiringSlackFactor(laborDemand, availableLabor)

    // Immigration
    val unempRateForImmig = w.unemploymentRate(cleared.employed)
    val newImmig          = Immigration.step(w.external.immigration, households, cleared.wage, unempRateForImmig)
    val netMigration      = newImmig.monthlyInflow - newImmig.monthlyOutflow

    // Demographics
    val newDemographics = SocialSecurity.demographicsStep(w.social.demographics, cleared.employed, netMigration)

    // Wage growth
    val wageGrowth = wageGrowthFrom(w.householdMarket.marketWage, cleared.wage)
    val newNfz     = SocialSecurity.nfzStep(
      cleared.employed,
      cleared.wage,
      newDemographics.workingAgePop,
      newDemographics.retirees,
    )

    Output(
      newWage = cleared.wage,
      employed = cleared.employed,
      laborDemand = laborDemand,
      wageGrowth = wageGrowth,
      operationalHiringSlack = operationalHiringSlack,
      newImmig = newImmig,
      netMigration = netMigration,
      newDemographics = newDemographics,
      newZus = SocialSecurity.ZusState.zero,
      newNfz = newNfz,
      newPpk = SocialSecurity.PpkState.zero,
      rawPpkBondPurchase = PLN.Zero,
      newEarmarked = EarmarkedFunds.State.zero,
      living = living,
      regionalWages = cleared.regionalWages,
    )

  /** Reconcile labor outputs after firm-side separations and matching so
    * downstream blocks use effective post-firm labor demand rather than stale
    * inherited headcount.
    */
  def reconcilePostFirmStep(
      w: World,
      s1: FiscalConstraintEconomics.Output,
      pre: Output,
      postLiving: Vector[Firm.State],
      postHouseholds: Vector[Household.State],
  )(using p: SimParams): Output =
    val postLaborDemand    = postLiving.map(Firm.workerCount).sum
    val cleared            = clearLaborMarket(w, s1.resWage, postLaborDemand)
    val realizedEmployment = Household.countEmployed(postHouseholds)
    val employedCap        = Math.min(realizedEmployment, pre.newDemographics.workingAgePop)
    val postAvailableLabor = LaborMarket.laborSupplyAtWage(cleared.wage, s1.resWage, w.derivedTotalPopulation)
    val newNfz             = SocialSecurity.nfzStep(
      employedCap,
      cleared.wage,
      pre.newDemographics.workingAgePop,
      pre.newDemographics.retirees,
    )
    pre.copy(
      newWage = cleared.wage,
      employed = employedCap,
      laborDemand = postLaborDemand,
      wageGrowth = wageGrowthFrom(w.householdMarket.marketWage, cleared.wage),
      operationalHiringSlack = operationalHiringSlackFactor(postLaborDemand, postAvailableLabor),
      newNfz = newNfz,
      living = postLiving,
      regionalWages = cleared.regionalWages,
    )

  private def clearLaborMarket(
      w: World,
      resWage: PLN,
      laborDemand: Int,
  )(using p: SimParams): ClearedLaborMarket =
    val (rawWage, rawEmployed, regWages) =
      val rc          = RegionalClearing.clear(w.regionalWages, resWage, laborDemand, w.derivedTotalPopulation)
      val natEmployed = LaborMarket.employmentAtWage(rc.nationalWage, resWage, laborDemand, w.derivedTotalPopulation)
      (rc.nationalWage, natEmployed, rc.regionalWages)

    val wageAfterExp =
      val expInflGap      = (w.mechanisms.expectations.expectedInflation - p.monetary.targetInfl).max(Rate.Zero).toCoefficient
      val expWagePressure = (p.labor.expWagePassthrough * expInflGap) / 12
      resWage.max(rawWage * expWagePressure.growthMultiplier)

    val newWage =
      val aggDensity =
        p.sectorDefs.zipWithIndex.map((s, i) => s.share * p.labor.unionDensity(i)).foldLeft(Share.Zero)(_ + _)
      val decline    = w.householdMarket.marketWage - wageAfterExp
      resWage.max(wageAfterExp + decline * p.labor.unionRigidity * aggDensity)

    val employed = Math.min(rawEmployed, w.social.demographics.workingAgePop)

    ClearedLaborMarket(newWage, employed, regWages)

  private def wageGrowthFrom(prevWage: PLN, newWage: PLN): Coefficient =
    if prevWage > PLN.Zero then (newWage / prevWage - Scalar.One).toCoefficient else Coefficient.Zero
