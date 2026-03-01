package sfc.agents

import sfc.config.{Config, SECTORS}

import scala.util.Random

/** Immigration state: tracks immigrant stock, flows, and remittances per month. */
case class ImmigrationState(
  immigrantStock: Int,        // total immigrant workers currently in economy
  monthlyInflow: Int,         // new immigrants this month
  monthlyOutflow: Int,        // returning emigrants this month
  remittanceOutflow: Double   // total remittance PLN leaving deposits this month
)
object ImmigrationState:
  val zero: ImmigrationState = ImmigrationState(0, 0, 0, 0.0)

object ImmigrationLogic:

  /** Compute monthly immigration inflow.
    * Exogenous: fixed rate × workingAgePop.
    * Endogenous: responds to (domesticWage / foreignWage - 1) × elasticity. */
  def computeInflow(workingAgePop: Int, wage: Double, unempRate: Double,
                    month: Int): Int =
    if !Config.ImmigEnabled then 0
    else if Config.ImmigEndogenous then
      val wageGap = (wage / Config.ImmigForeignWage - 1.0).max(0.0)
      val pull = wageGap * Config.ImmigWageElasticity
      val push = (1.0 - unempRate).max(0.0)
      val rate = Config.ImmigMonthlyRate * (0.5 + 0.5 * pull * push)
      (workingAgePop * rate).toInt.max(0)
    else
      (workingAgePop * Config.ImmigMonthlyRate).toInt.max(0)

  /** Compute monthly return migration (outflow). */
  def computeOutflow(immigrantStock: Int): Int =
    if !Config.ImmigEnabled then 0
    else (immigrantStock * Config.ImmigReturnRate).toInt.max(0)

  /** Compute total remittance outflow from immigrant HH (individual mode).
    * Remittances = employed immigrant wages × remittance rate. */
  def computeRemittances(immigrantHH: Iterable[Household]): Double =
    if !Config.ImmigEnabled then 0.0
    else
      immigrantHH.filter(h => h.isImmigrant).map { h =>
        h.status match
          case HhStatus.Employed(_, _, wage) =>
            wage * Config.ImmigRemittanceRate
          case _ => 0.0
      }.sum

  /** Compute aggregate-mode remittances (no individual HH). */
  def computeRemittancesAggregate(immigrantStock: Int, wage: Double,
                                   unempRate: Double): Double =
    if !Config.ImmigEnabled then 0.0
    else
      val employedImmigrants = (immigrantStock * (1.0 - unempRate)).toInt
      employedImmigrants * wage * (1.0 - Config.ImmigWageDiscount) *
        Config.ImmigRemittanceRate

  /** Choose sector for new immigrant (weighted by Config.ImmigSectorShares). */
  def chooseSector(rng: Random): Int =
    val r = rng.nextDouble()
    var cum = 0.0
    var s = 0
    while s < SECTORS.length - 1 do
      cum += Config.ImmigSectorShares(s)
      if r < cum then return s
      s += 1
    SECTORS.length - 1 // fallback: last sector

  /** Spawn new immigrant households (individual mode).
    * Start as Unemployed(0) — will be matched in next jobSearch round. */
  def spawnImmigrants(count: Int, startId: Int, rng: Random): Vector[Household] =
    (0 until count).map { i =>
      val sector = chooseSector(rng)
      val skill = Config.ImmigSkillMean + (rng.nextGaussian() * 0.15)
      val clampedSkill = skill.max(0.15).min(0.95)
      val savings = rng.nextDouble() * 5000.0
      val mpc = 0.85 + rng.nextGaussian() * 0.05
      val rent = Config.HhRentMean + rng.nextGaussian() * Config.HhRentStd
      Household(
        id = startId + i,
        savings = savings,
        debt = 0.0,
        monthlyRent = rent.max(800.0),
        skill = clampedSkill,
        healthPenalty = 0.0,
        mpc = mpc.max(0.7).min(0.98),
        status = HhStatus.Unemployed(0),
        socialNeighbors = Array.empty,
        lastSectorIdx = sector,
        isImmigrant = true
      )
    }.toVector

  /** Remove returning migrants from household vector.
    * Removes oldest immigrants (lowest ids among immigrants). */
  def removeReturnMigrants(households: Vector[Household], count: Int): Vector[Household] =
    if count <= 0 then return households
    val immigrantIds = households.filter(_.isImmigrant).map(_.id).sorted.take(count).toSet
    households.filterNot(h => immigrantIds.contains(h.id))

  /** Full monthly step: compute inflow, outflow, remittances, update state. */
  def step(prev: ImmigrationState, households: Option[Vector[Household]],
           wage: Double, unempRate: Double, workingAgePop: Int,
           month: Int): ImmigrationState =
    val inflow = computeInflow(workingAgePop, wage, unempRate, month)
    val outflow = computeOutflow(prev.immigrantStock)
    val newStock = (prev.immigrantStock + inflow - outflow).max(0)
    val remittances = households match
      case Some(hhs) => computeRemittances(hhs)
      case None => computeRemittancesAggregate(newStock, wage, unempRate)
    ImmigrationState(newStock, inflow, outflow, remittances)
