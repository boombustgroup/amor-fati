package sfc.engine.steps

import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.World
import sfc.engine.markets.LaborMarket
import sfc.types.*
import sfc.util.KahanSum.*

object LaborDemographicsStep:

  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintStep.Output,
  )

  case class Output(
      newWage: Double,
      employed: Int,
      laborDemand: Int,
      wageGrowth: Double,
      newImmig: Immigration.State,
      netMigration: Int,
      newDemographics: SocialSecurity.DemographicsState,
      newZus: SocialSecurity.ZusState,
      newPpk: SocialSecurity.PpkState,
      rawPpkBondPurchase: Double,
      living: Vector[Firm.State],
  )

  def run(in: Input)(using p: SimParams): Output =
    val living                 = in.firms.filter(Firm.isAlive)
    val laborDemand            = living.kahanSumBy(f => Firm.workerCount(f).toDouble).toInt
    val wageResult             =
      LaborMarket.updateLaborMarket(in.w.hhAgg.marketWage, PLN(in.s1.resWage), laborDemand, in.w.totalPopulation)
    val (rawWage, rawEmployed) = (wageResult.wage.toDouble, wageResult.employed)

    // Channel 1: Expectations-augmented wage Phillips curve
    val wageAfterExp = if p.flags.expectations then
      val target          = p.monetary.targetInfl.toDouble
      val expWagePressure = p.labor.expWagePassthrough.toDouble *
        Math.max(0.0, in.w.mechanisms.expectations.expectedInflation.toDouble - target) / 12.0
      Math.max(in.s1.resWage, rawWage * (1.0 + expWagePressure))
    else rawWage

    // Union downward wage rigidity (#44)
    val newWage = if p.flags.unions && wageAfterExp < in.w.hhAgg.marketWage.toDouble then
      val aggDensity =
        p.sectorDefs.zipWithIndex.map((s, i) => s.share.toDouble * p.labor.unionDensity.map(_.toDouble)(i)).sum
      val decline    = in.w.hhAgg.marketWage.toDouble - wageAfterExp
      Math.max(in.s1.resWage, wageAfterExp + decline * p.labor.unionRigidity.toDouble * aggDensity)
    else wageAfterExp

    // Demographics caps employment at working-age population
    val employed =
      if p.flags.demographics then Math.min(rawEmployed, in.w.social.demographics.workingAgePop)
      else rawEmployed

    // Immigration
    val unempRateForImmig = 1.0 - employed.toDouble / in.w.totalPopulation
    val newImmig          = Immigration.step(
      in.w.external.immigration,
      in.households,
      PLN(newWage),
      unempRateForImmig,
      in.w.social.demographics.workingAgePop.max(in.w.totalPopulation),
      in.s1.m,
    )
    val netMigration      = newImmig.monthlyInflow - newImmig.monthlyOutflow

    val newDemographics = SocialSecurity.demographicsStep(in.w.social.demographics, employed, netMigration)

    val newZus             = SocialSecurity.zusStep(in.w.social.zus.fusBalance, employed, PLN(newWage), newDemographics.retirees)
    val newPpk             = SocialSecurity.ppkStep(in.w.social.ppk.bondHoldings, employed, PLN(newWage))
    val rawPpkBondPurchase = SocialSecurity.ppkBondPurchase(newPpk).toDouble

    val wageGrowth = if in.w.hhAgg.marketWage.toDouble > 0 then newWage / in.w.hhAgg.marketWage.toDouble - 1.0 else 0.0

    Output(
      newWage,
      employed,
      laborDemand,
      wageGrowth,
      newImmig,
      netMigration,
      newDemographics,
      newZus,
      newPpk,
      rawPpkBondPurchase,
      living,
    )
