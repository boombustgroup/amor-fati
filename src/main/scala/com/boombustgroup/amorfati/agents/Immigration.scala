package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

import scala.util.Random

/** Immigration: tracks immigrant stock, flows, remittances, spawning/removal.
  */
object Immigration:

  case class State(
      immigrantStock: Int,   // total immigrant workers currently in economy
      monthlyInflow: Int,    // new immigrants this month
      monthlyOutflow: Int,   // returning emigrants this month
      remittanceOutflow: PLN, // total remittance outflow leaving deposits this month
  )

  object State:
    val zero: State = State(0, 0, 0, PLN.Zero)

  /** Monthly immigration inflow. Exogenous: fixed rate × basePop. Endogenous:
    * responds to (domesticWage / foreignWage − 1) × elasticity.
    */
  @computationBoundary
  def computeInflow(wage: PLN, unempRate: Double)(using p: SimParams): Int =
    import ComputationBoundary.toDouble
    if !p.flags.immigration then 0
    else
      val basePop = p.pop.firmsCount * p.pop.workersPerFirm
      if p.flags.immigEndogenous then
        val wageGap = (wage / p.immigration.foreignWage - 1.0).max(0.0)
        val pull    = wageGap * toDouble(p.immigration.wageElasticity)
        val push    = (1.0 - unempRate).max(0.0)
        val rate    = toDouble(p.immigration.monthlyRate) * (0.5 + 0.5 * pull * push)
        (basePop * rate).toInt.max(0)
      else (basePop * toDouble(p.immigration.monthlyRate)).toInt.max(0)

  /** Monthly return migration (outflow). */
  def computeOutflow(immigrantStock: Int)(using p: SimParams): Int =
    if !p.flags.immigration then 0
    else (immigrantStock.toLong * p.immigration.returnRate.toLong / 10000L).toInt.max(0)

  /** Total remittance outflow from immigrant HH. Remittances = employed
    * immigrant wages × remittance rate.
    */
  def computeRemittances(immigrantHH: Iterable[Household.State])(using p: SimParams): PLN =
    if !p.flags.immigration then PLN.Zero
    else
      PLN.fromRaw(
        immigrantHH
          .filter(_.isImmigrant)
          .map { h =>
            h.status match
              case HhStatus.Employed(_, _, wage) => (wage * p.immigration.remitRate).toLong
              case _                             => 0L
          }
          .sum,
      )

  /** Choose sector for new immigrant (weighted by sectorShares). */
  def chooseSector(rng: Random)(using p: SimParams): SectorIdx =
    val shares = p.immigration.sectorShares.map(_.toLong)
    val total  = shares.sum
    val r      = (rng.nextDouble() * total).toLong
    val cumul  = shares.scanLeft(0L)(_ + _).tail
    val picked = cumul.indexWhere(_ > r)
    SectorIdx(if picked >= 0 then picked else p.sectorDefs.length - 1)

  /** Spawn new immigrant households. Start as Unemployed(0) — matched in next
    * jobSearch round.
    */
  @computationBoundary
  def spawnImmigrants(count: Int, startId: Int, rng: Random)(using p: SimParams): Vector[Household.State] =
    import ComputationBoundary.toDouble
    (0 until count).map { i =>
      val sector                     = chooseSector(rng)
      val edu                        = p.social.drawImmigrantEducation(rng)
      val skill                      = toDouble(p.immigration.skillMean) + (rng.nextGaussian() * 0.15)
      val (skillFloor, skillCeiling) = p.social.eduSkillRange(edu)
      val clampedSkill               = skill.max(skillFloor).min(skillCeiling)
      val savings                    = rng.nextDouble() * 5000.0
      val mpc                        = 0.85 + rng.nextGaussian() * 0.05
      val region                     = if p.flags.regionalLabor then Region.cdfSample(rng) else Region.Central
      val baseRent                   = toDouble(p.household.rentMean) + rng.nextGaussian() * toDouble(p.household.rentStd)
      val rent                       = if p.flags.regionalLabor then baseRent * toDouble(region.housingCostIndex) else baseRent
      val numChildren                =
        if p.flags.social800 && p.flags.social800ImmigEligible then Distributions.poissonSample(p.fiscal.social800ChildrenPerHh, rng)
        else 0
      Household.State(
        id = HhId(startId + i),
        savings = PLN(savings),
        debt = PLN.Zero,
        monthlyRent = PLN(rent.max(800.0)),
        skill = Share(clampedSkill),
        healthPenalty = Share.Zero,
        mpc = Share(mpc.max(0.7).min(0.98)),
        status = HhStatus.Unemployed(0),
        socialNeighbors = Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = sector,
        isImmigrant = true,
        numDependentChildren = numChildren,
        consumerDebt = PLN.Zero,
        education = edu,
        taskRoutineness = Household.Init.sampleTaskRoutineness(edu, sector, rng),
        wageScar = Share.Zero,
        region = region,
      )
    }.toVector

  /** Remove returning migrants from household vector. Removes oldest immigrants
    * (lowest ids among immigrants).
    */
  def removeReturnMigrants(households: Vector[Household.State], count: Int): Vector[Household.State] =
    if count <= 0 then households
    else
      val immigrantIds = households.filter(_.isImmigrant).map(_.id).sorted.take(count).toSet
      households.filterNot(h => immigrantIds.contains(h.id))

  /** Full monthly step: compute inflow, outflow, remittances, update state. */
  def step(
      prev: State,
      households: Vector[Household.State],
      wage: PLN,
      unempRate: Double,
  )(using SimParams): State =
    val inflow      = computeInflow(wage, unempRate)
    val outflow     = computeOutflow(prev.immigrantStock)
    val newStock    = (prev.immigrantStock + inflow - outflow).max(0)
    val remittances = computeRemittances(households)
    State(newStock, inflow, outflow, remittances)
