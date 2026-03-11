package sfc.engine.mechanisms

import sfc.agents.*
import sfc.config.SimParams
import sfc.types.*

import scala.util.Random

/** Cross-sector labour mobility: friction-based transitions, voluntary quits,
  * wage penalties.
  *
  * Workers changing sectors face friction costs calibrated to a 6×6 symmetric
  * transition matrix (GUS BAEL 2023 inter-industry flows). Friction affects:
  * retraining duration/cost, success probability, and post-transition wage.
  *
  * Target sector selection is probabilistic, weighted by destination wage,
  * vacancy count, and inverse friction (gravity-like model).
  *
  * Sectors: BPO(0), Manufacturing(1), Retail(2), Healthcare(3), Public(4),
  * Agriculture(5).
  */
object SectoralMobility:

  // ---- Calibration constants ----
  private val MaxWagePenalty   = 0.3 // cross-sector wage penalty: up to 30% of friction
  private val FrictionSuccDisc = 0.5 // friction discount on retraining success probability
  private val NumSectors       = 6

  case class State(
      crossSectorHires: Int,     // workers hired into a different sector this step
      voluntaryQuits: Int,       // workers who quit to seek better sector
      sectorMobilityRate: Double, // cross-sector hires / total employed
  )

  object State:
    val zero: State = State(0, 0, 0.0)

  def zero: State = State.zero

  /** Default 6×6 symmetric transition friction matrix. f(i,j) ∈ [0,1]: 0 =
    * frictionless, 1 = near-impossible. GUS BAEL 2023 inter-industry flows.
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
  def sectorVacancies(households: Vector[Household.State], firms: Vector[Firm.State])(using SimParams): Vector[Int] =
    val workerCounts = households.foldLeft(Vector.fill(NumSectors)(0)) { (acc, hh) =>
      hh.status match
        case HhStatus.Employed(_, sectorIdx, _) => acc.updated(sectorIdx.toInt, acc(sectorIdx.toInt) + 1)
        case _                                  => acc
    }
    val sectorDemand = firms.filter(Firm.isAlive).foldLeft(Vector.fill(NumSectors)(0)) { (acc, f) =>
      acc.updated(f.sector.toInt, acc(f.sector.toInt) + Firm.workerCount(f))
    }
    (0 until NumSectors).map(s => Math.max(0, sectorDemand(s) - workerCounts(s))).toVector

  /** Compute average wage per sector from employed households. */
  def sectorWages(households: Vector[Household.State]): Vector[PLN] =
    val (sums, counts) = households.foldLeft((Vector.fill(NumSectors)(PLN.Zero), Vector.fill(NumSectors)(0))) { case ((sums, counts), hh) =>
      hh.status match
        case HhStatus.Employed(_, sectorIdx, wage) =>
          val s = sectorIdx.toInt
          (sums.updated(s, sums(s) + wage), counts.updated(s, counts(s) + 1))
        case _                                     => (sums, counts)
    }
    (0 until NumSectors).map(s => if counts(s) > 0 then sums(s) / counts(s).toDouble else PLN.Zero).toVector

  /** Probabilistic target sector selection (gravity model). score(to) =
    * wage(to) × (vacancies(to) + 1)^vacancyWeight × (1 − friction(from,to)).
    */
  def selectTargetSector(
      from: Int,
      wages: Vector[PLN],
      vacancies: Vector[Int],
      matrix: Vector[Vector[Double]],
      vacancyWeight: Double,
      rng: Random,
  )(using p: SimParams): Int =
    val scores = gravityScores(from, wages, vacancies, matrix, vacancyWeight)
    val total  = scores.sum
    if total <= 0.0 then uniformFallback(from, rng)
    else rouletteSelect(scores, from, rng.nextDouble() * total)

  /** Gravity-model attractiveness score per destination sector. */
  private def gravityScores(
      from: Int,
      wages: Vector[PLN],
      vacancies: Vector[Int],
      matrix: Vector[Vector[Double]],
      vacancyWeight: Double,
  ): Vector[Double] =
    (0 until NumSectors).map { to =>
      if to == from then 0.0
      else
        wages(to).max(PLN.Zero).toDouble *
          Math.pow(vacancies(to).toDouble + 1.0, vacancyWeight) *
          (1.0 - matrix(from)(to))
    }.toVector

  /** Roulette-wheel selection over non-zero scores (pure, no vars). */
  private def rouletteSelect(scores: Vector[Double], from: Int, threshold: Double): Int =
    val cumulative = scores.scanLeft(0.0)(_ + _).tail // cumulative sums, length = NumSectors
    cumulative.indexWhere(_ >= threshold) match
      case idx if idx >= 0 && idx != from => idx
      case _                              => firstNonZeroSector(scores, from)

  /** First sector with positive score, excluding `from`. Defensive fallback —
    * should only trigger on floating-point edge cases since rouletteSelect is
    * called only when total > 0.
    */
  private def firstNonZeroSector(scores: Vector[Double], from: Int): Int =
    scores.indices
      .find(i => i != from && scores(i) > 0)
      .getOrElse(scores.indices.find(_ != from).get)

  /** Uniform random fallback when all scores are zero. */
  private def uniformFallback(from: Int, rng: Random): Int =
    val others = (0 until NumSectors).filter(_ != from)
    others(rng.nextInt(others.length))

  /** Friction-adjusted retraining duration (months) and cost (PLN). */
  case class RetrainingParams(duration: Int, cost: PLN)

  /** Adjust retraining duration and cost by friction level. */
  def frictionAdjustedParams(friction: Double, durationMult: Double, costMult: Double)(using p: SimParams): RetrainingParams =
    val adjDuration = Math.round(p.household.retrainingDuration * (1.0 + friction * durationMult)).toInt
    val adjCost     = p.household.retrainingCost * (1.0 + friction * costMult)
    RetrainingParams(adjDuration, adjCost)

  /** Cross-sector wage penalty: proportional to friction, max 30%. */
  def crossSectorWagePenalty(friction: Double): Double =
    1.0 - friction * MaxWagePenalty

  /** Friction-adjusted success probability for retraining. */
  def frictionAdjustedSuccess(baseProbSkillHealth: Double, friction: Double): Double =
    baseProbSkillHealth * (1.0 - friction * FrictionSuccDisc)
