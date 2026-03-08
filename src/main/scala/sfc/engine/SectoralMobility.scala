package sfc.engine

import sfc.agents.*
import sfc.config.Config
import sfc.types.*

import scala.util.Random

object SectoralMobility:
  case class State(
    crossSectorHires: Int = 0,
    voluntaryQuits: Int = 0,
    sectorMobilityRate: Double = 0.0,
  )

  def zero: State = State()

  /** Default 6x6 symmetric transition friction matrix. f(i,j) in [0,1]: 0 = frictionless, 1 = near-impossible.
    * BPO<->Retail low (digital/service overlap), Mfg<->Agr moderate (physical skills), Healthcare high from non-medical
    * (credential barrier).
    */
  val DefaultFrictionMatrix: Vector[Vector[Double]] = Vector(
    //       BPO   Mfg   Ret   Hlt   Pub   Agr
    Vector(0.0, 0.6, 0.3, 0.8, 0.5, 0.9), // BPO
    Vector(0.6, 0.0, 0.5, 0.7, 0.6, 0.4), // Mfg
    Vector(0.3, 0.5, 0.0, 0.5, 0.4, 0.6), // Ret
    Vector(0.8, 0.7, 0.5, 0.0, 0.4, 0.8), // Hlt
    Vector(0.5, 0.6, 0.4, 0.4, 0.0, 0.7), // Pub
    Vector(0.9, 0.4, 0.6, 0.8, 0.7, 0.0), // Agr
  )

  /** Compute number of vacancies per sector. */
  def sectorVacancies(households: Vector[Household.State], firms: Vector[Firm.State]): Array[Int] =
    val workerCounts = new Array[Int](6)
    for hh <- households do
      hh.status match
        case HhStatus.Employed(_, sectorIdx, _) => workerCounts(sectorIdx.toInt) += 1
        case _                                  =>

    val sectorDemand = new Array[Int](6)
    for f <- firms if Firm.isAlive(f) do sectorDemand(f.sector.toInt) += Firm.workers(f)

    val vac = new Array[Int](6)
    for s <- 0 until 6 do vac(s) = Math.max(0, sectorDemand(s) - workerCounts(s))
    vac

  /** Compute average wage per sector from employed households. */
  def sectorWages(households: Vector[Household.State]): Array[Double] =
    val sums = new Array[Double](6)
    val counts = new Array[Int](6)
    for hh <- households do
      hh.status match
        case HhStatus.Employed(_, sectorIdx, wage) =>
          sums(sectorIdx.toInt) += wage.toDouble
          counts(sectorIdx.toInt) += 1
        case _ =>
    val result = new Array[Double](6)
    for s <- 0 until 6 do result(s) = if counts(s) > 0 then sums(s) / counts(s) else 0.0
    result

  /** Probabilistic target sector selection weighted by wage, vacancy, and inverse friction. score(to) = avgWage(to) *
    * (vacancies(to) + 1)^vacancyWeight * (1 - friction(from, to)) Returns selected sector index.
    */
  def selectTargetSector(
    from: Int,
    wages: Array[Double],
    vacancies: Array[Int],
    matrix: Vector[Vector[Double]],
    vacancyWeight: Double,
    rng: Random,
  ): Int =
    val scores = new Array[Double](6)
    var total = 0.0
    for to <- 0 until 6 if to != from do
      val friction = matrix(from)(to)
      val s = Math.max(0.0, wages(to)) *
        Math.pow(vacancies(to).toDouble + 1.0, vacancyWeight) *
        (1.0 - friction)
      scores(to) = s
      total += s

    if total <= 0.0 then
      // Fallback: uniform random over other sectors
      val others = (0 until 6).filter(_ != from)
      others(rng.nextInt(others.length))
    else
      val r = rng.nextDouble() * total
      var cum = 0.0
      var selected = 0
      for to <- 0 until 6 do
        cum += scores(to)
        if cum >= r && scores(to) > 0 && selected == 0 && to != from then selected = to
      if selected == 0 then
        // Edge case: pick first non-from sector with score
        (0 until 6)
          .find(to => to != from && scores(to) > 0)
          .getOrElse(
            (0 until 6).find(_ != from).getOrElse(0),
          )
      else selected

  /** Adjust retraining parameters by friction level. */
  def frictionAdjustedParams(friction: Double, durationMult: Double, costMult: Double): (Int, Double) =
    val adjDuration = Math.round(Config.HhRetrainingDuration * (1.0 + friction * durationMult)).toInt
    val adjCost = Config.HhRetrainingCost * (1.0 + friction * costMult)
    (adjDuration, adjCost)

  /** Cross-sector wage penalty: proportional to friction, max 30%. */
  def crossSectorWagePenalty(friction: Double): Double =
    1.0 - friction * 0.3

  /** Friction-adjusted success probability for retraining. */
  def frictionAdjustedSuccess(baseProbSkillHealth: Double, friction: Double): Double =
    baseProbSkillHealth * (1.0 - friction * 0.5)
