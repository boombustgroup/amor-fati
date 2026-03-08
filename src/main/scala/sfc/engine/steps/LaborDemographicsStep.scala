package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, RunConfig, SectorDefs}
import sfc.engine.{LaborMarket, World}
import sfc.types.*
import sfc.util.KahanSum.*

object LaborDemographicsStep:

  case class Input(
    w: World,
    rc: RunConfig,
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

  def run(in: Input): Output =
    val living = in.firms.filter(Firm.isAlive)
    val laborDemand = living.kahanSumBy(f => Firm.workers(f).toDouble).toInt
    val (rawWage, rawEmployed) = LaborMarket.updateLaborMarket(in.w.hh.marketWage.toDouble, in.s1.resWage, laborDemand)

    // Channel 1: Expectations-augmented wage Phillips curve
    val wageAfterExp = if Config.ExpEnabled then
      val target = Config.NbpTargetInfl
      val expWagePressure = Config.ExpWagePassthrough *
        Math.max(0.0, in.w.expectations.expectedInflation.toDouble - target) / 12.0
      Math.max(in.s1.resWage, rawWage * (1.0 + expWagePressure))
    else rawWage

    // Union downward wage rigidity (#44)
    val newWage = if Config.UnionEnabled && wageAfterExp < in.w.hh.marketWage.toDouble then
      val aggDensity = SectorDefs.zipWithIndex.map((s, i) => s.share.toDouble * Config.UnionDensity(i)).sum
      val decline = in.w.hh.marketWage.toDouble - wageAfterExp
      Math.max(in.s1.resWage, wageAfterExp + decline * Config.UnionRigidity * aggDensity)
    else wageAfterExp

    // Demographics caps employment at working-age population
    val employed =
      if Config.DemEnabled then Math.min(rawEmployed, in.w.demographics.workingAgePop)
      else rawEmployed

    // Immigration
    val unempRateForImmig = 1.0 - employed.toDouble / Config.TotalPopulation
    val newImmig = Immigration.step(
      in.w.immigration,
      in.households,
      newWage,
      unempRateForImmig,
      in.w.demographics.workingAgePop.max(Config.TotalPopulation),
      in.s1.m,
    )
    val netMigration = newImmig.monthlyInflow - newImmig.monthlyOutflow

    val newDemographics = SocialSecurity.demographicsStep(in.w.demographics, employed, netMigration)

    val newZus = SocialSecurity.zusStep(in.w.zus.fusBalance.toDouble, employed, newWage, newDemographics.retirees)
    val newPpk = SocialSecurity.ppkStep(in.w.ppk.bondHoldings.toDouble, employed, newWage)
    val rawPpkBondPurchase = SocialSecurity.ppkBondPurchase(newPpk)

    val wageGrowth = if in.w.hh.marketWage.toDouble > 0 then newWage / in.w.hh.marketWage.toDouble - 1.0 else 0.0

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
