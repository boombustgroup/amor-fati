package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.Sectors
import sfc.types.*
import sfc.util.KahanSum.*

object LaborDemographicsStep:

  case class Input(
    marketWage: PLN,
    firms: Array[Firm.State],
    demographics: SocialSecurity.DemographicsState,
    immigration: Immigration.State,
    zusBalance: Double,
    ppkBondHoldings: Double,
    households: Option[Vector[Household.State]],
    resWage: Double,
    expectedInflation: Double,
    m: Int,
    rc: RunConfig
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
    living: Array[Firm.State]
  )

  def run(in: Input): Output =
    val living = in.firms.filter(Firm.isAlive)
    val laborDemand = living.kahanSumBy(f => Firm.workers(f).toDouble).toInt
    val (rawWage, rawEmployed) = Sectors.updateLaborMarket(in.marketWage.toDouble, in.resWage, laborDemand)

    // Channel 1: Expectations-augmented wage Phillips curve
    val wageAfterExp = if Config.ExpEnabled then
      val target = if in.rc.isEurozone then Config.EcbTargetInfl else Config.NbpTargetInfl
      val expWagePressure = Config.ExpWagePassthrough *
        Math.max(0.0, in.expectedInflation - target) / 12.0
      Math.max(in.resWage, rawWage * (1.0 + expWagePressure))
    else rawWage

    // Union downward wage rigidity (#44)
    val newWage = if Config.UnionEnabled && wageAfterExp < in.marketWage.toDouble then
      val aggDensity = SECTORS.zipWithIndex.map((s, i) => s.share.toDouble * Config.UnionDensity(i)).sum
      val decline = in.marketWage.toDouble - wageAfterExp
      Math.max(in.resWage, wageAfterExp + decline * Config.UnionRigidity * aggDensity)
    else wageAfterExp

    // Demographics caps employment at working-age population
    val employed = if Config.DemEnabled then
      Math.min(rawEmployed, in.demographics.workingAgePop)
    else rawEmployed

    // Immigration
    val unempRateForImmig = 1.0 - employed.toDouble / Config.TotalPopulation
    val newImmig = Immigration.step(
      in.immigration, in.households, newWage, unempRateForImmig,
      in.demographics.workingAgePop.max(Config.TotalPopulation), in.m)
    val netMigration = newImmig.monthlyInflow - newImmig.monthlyOutflow

    val newDemographics = SocialSecurity.demographicsStep(in.demographics, employed, netMigration)

    val newZus = SocialSecurity.zusStep(in.zusBalance, employed, newWage, newDemographics.retirees)
    val newPpk = SocialSecurity.ppkStep(in.ppkBondHoldings, employed, newWage)
    val rawPpkBondPurchase = SocialSecurity.ppkBondPurchase(newPpk)

    val wageGrowth = if in.marketWage.toDouble > 0 then newWage / in.marketWage.toDouble - 1.0 else 0.0

    Output(newWage, employed, laborDemand, wageGrowth, newImmig, netMigration,
      newDemographics, newZus, newPpk, rawPpkBondPurchase, living)
