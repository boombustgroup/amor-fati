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

  /** Monthly immigration inflow. Exogenous: fixed rate × workingAgePop.
    * Endogenous: responds to (domesticWage / foreignWage − 1) × elasticity.
    */
  def computeInflow(workingAgePop: Int, wage: PLN, unempRate: Double, @scala.annotation.unused month: Int)(using p: SimParams): Int =
    if !p.flags.immigration then 0
    else if p.flags.immigEndogenous then
      val wageGap = (wage.toDouble / p.immigration.foreignWage.toDouble - 1.0).max(0.0)
      val pull    = wageGap * p.immigration.wageElasticity
      val push    = (1.0 - unempRate).max(0.0)
      val rate    = p.immigration.monthlyRate.toDouble * (0.5 + 0.5 * pull * push)
      (workingAgePop * rate).toInt.max(0)
    else (workingAgePop * p.immigration.monthlyRate.toDouble).toInt.max(0)

  /** Monthly return migration (outflow). */
  def computeOutflow(immigrantStock: Int)(using p: SimParams): Int =
    if !p.flags.immigration then 0
    else (immigrantStock * p.immigration.returnRate.toDouble).toInt.max(0)

  /** Total remittance outflow from immigrant HH. Remittances = employed
    * immigrant wages × remittance rate.
    */
  def computeRemittances(immigrantHH: Iterable[Household.State])(using p: SimParams): PLN =
    if !p.flags.immigration then PLN.Zero
    else
      PLN(
        immigrantHH
          .filter(_.isImmigrant)
          .map { h =>
            h.status match
              case HhStatus.Employed(_, _, wage) => wage.toDouble * p.immigration.remitRate.toDouble
              case _                             => 0.0
          }
          .sum,
      )

  /** Choose sector for new immigrant (weighted by sectorShares). */
  def chooseSector(rng: Random)(using p: SimParams): SectorIdx =
    val shares = p.immigration.sectorShares.map(_.toDouble)
    val r      = rng.nextDouble()
    val cumul  = shares.scanLeft(0.0)(_ + _).tail
    val picked = cumul.indexWhere(_ > r)
    SectorIdx(if picked >= 0 then picked else p.sectorDefs.length - 1)

  /** Spawn new immigrant households. Start as Unemployed(0) — matched in next
    * jobSearch round.
    */
  def spawnImmigrants(count: Int, startId: Int, rng: Random)(using p: SimParams): Vector[Household.State] =
    (0 until count).map { i =>
      val sector                     = chooseSector(rng)
      val edu                        = p.social.drawImmigrantEducation(rng)
      val skill                      = p.immigration.skillMean.toDouble + (rng.nextGaussian() * 0.15)
      val (skillFloor, skillCeiling) = p.social.eduSkillRange(edu)
      val clampedSkill               = skill.max(skillFloor).min(skillCeiling)
      val savings                    = rng.nextDouble() * 5000.0
      val mpc                        = 0.85 + rng.nextGaussian() * 0.05
      val rent                       = p.household.rentMean.toDouble + rng.nextGaussian() * p.household.rentStd.toDouble
      val numChildren                =
        if p.flags.social800 && p.flags.social800ImmigEligible then Distributions.poissonSample(p.fiscal.social800ChildrenPerHh, rng)
        else 0
      Household.State(
        id = HhId(startId + i),
        savings = PLN(savings),
        debt = PLN.Zero,
        monthlyRent = PLN(rent.max(800.0)),
        skill = Ratio(clampedSkill),
        healthPenalty = Ratio.Zero,
        mpc = Ratio(mpc.max(0.7).min(0.98)),
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
      workingAgePop: Int,
      month: Int,
  )(using SimParams): State =
    val inflow      = computeInflow(workingAgePop, wage, unempRate, month)
    val outflow     = computeOutflow(prev.immigrantStock)
    val newStock    = (prev.immigrantStock + inflow - outflow).max(0)
    val remittances = computeRemittances(households)
    State(newStock, inflow, outflow, remittances)
