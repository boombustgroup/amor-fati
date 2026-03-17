package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.LaborMarket
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Labor market clearing and demographics: wage determination (Phillips curve
  * with expectations augmentation and union rigidity), employment capping at
  * working-age population, immigration flows, ZUS pension system, PPK capital
  * pillar contributions, and demographic transitions (aging, retirement).
  */
object LaborDemographicsStep:

  case class Input(
      w: World,                            // current world state
      firms: Vector[Firm.State],           // pre-step firm population
      households: Vector[Household.State], // pre-step household population
      s1: FiscalConstraintStep.Output,     // fiscal constraint (reservation wage, lending base rate)
  )

  case class Output(
      newWage: PLN,                                      // market-clearing wage after Phillips curve + expectations + union adjustments
      employed: Int,                                     // total employed workers across all living firms
      laborDemand: Int,                                  // aggregate labor demand (sum of firm worker counts)
      wageGrowth: Ratio,                                 // month-on-month nominal wage growth rate
      newImmig: Immigration.State,                       // updated immigration state (inflows, outflows)
      netMigration: Int,                                 // net migration this month (inflow minus outflow)
      newDemographics: SocialSecurity.DemographicsState, // updated demographics (working-age pop, retirees)
      newZus: SocialSecurity.ZusState,                   // updated ZUS pension system (contributions, payouts)
      newNfz: SocialSecurity.NfzState,                   // updated NFZ health insurance (contributions, spending)
      newPpk: SocialSecurity.PpkState,                   // updated PPK capital pillar (bond holdings)
      rawPpkBondPurchase: PLN,                           // PPK monthly gov bond purchase before supply cap
      living: Vector[Firm.State],                        // surviving firms (bankrupt firms filtered out)
  )

  def run(in: Input)(using p: SimParams): Output =
    val living                 = in.firms.filter(Firm.isAlive)
    val laborDemand            = living.kahanSumBy(f => Firm.workerCount(f).toDouble).toInt
    val wageResult             =
      LaborMarket.updateLaborMarket(in.w.hhAgg.marketWage, in.s1.resWage, laborDemand, in.w.totalPopulation)
    val (rawWage, rawEmployed) = (wageResult.wage.toDouble, wageResult.employed)

    // Channel 1: Expectations-augmented wage Phillips curve
    val wageAfterExp = if p.flags.expectations then
      val target          = p.monetary.targetInfl.toDouble
      val expWagePressure = p.labor.expWagePassthrough.toDouble *
        Math.max(0.0, in.w.mechanisms.expectations.expectedInflation.toDouble - target) / 12.0
      Math.max(in.s1.resWage.toDouble, rawWage * (1.0 + expWagePressure))
    else rawWage

    // Union downward wage rigidity (#44)
    val newWage = if p.flags.unions && wageAfterExp < in.w.hhAgg.marketWage.toDouble then
      val aggDensity =
        p.sectorDefs.zipWithIndex.map((s, i) => s.share.toDouble * p.labor.unionDensity.map(_.toDouble)(i)).sum
      val decline    = in.w.hhAgg.marketWage.toDouble - wageAfterExp
      Math.max(in.s1.resWage.toDouble, wageAfterExp + decline * p.labor.unionRigidity.toDouble * aggDensity)
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
    val newNfz             = SocialSecurity.nfzStep(in.w.social.nfz.balance, employed, PLN(newWage), newDemographics.workingAgePop, newDemographics.retirees)
    val newPpk             = SocialSecurity.ppkStep(in.w.social.ppk.bondHoldings, employed, PLN(newWage))
    val rawPpkBondPurchase = SocialSecurity.ppkBondPurchase(newPpk).toDouble

    val wageGrowth = if in.w.hhAgg.marketWage.toDouble > 0 then newWage / in.w.hhAgg.marketWage.toDouble - 1.0 else 0.0

    Output(
      PLN(newWage),
      employed,
      laborDemand,
      Ratio(wageGrowth),
      newImmig,
      netMigration,
      newDemographics,
      newZus,
      newNfz,
      newPpk,
      PLN(rawPpkBondPurchase),
      living,
    )
