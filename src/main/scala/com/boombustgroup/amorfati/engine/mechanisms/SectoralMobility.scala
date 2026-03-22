package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.annotation.unused
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
  private val MaxWagePenalty         = Share(0.3) // cross-sector wage penalty: up to 30% of friction
  val FrictionSuccessDiscount: Share = Share(0.5) // friction discount on retraining success probability
  private val NumSectors             = 6

  case class State(
      crossSectorHires: Int,    // workers hired into a different sector this step
      voluntaryQuits: Int,      // workers who quit to seek better sector
      sectorMobilityRate: Share, // cross-sector hires / total employed
  )

  object State:
    val zero: State = State(0, 0, Share.Zero)

  def zero: State = State.zero

  /** Default 6×6 symmetric transition friction matrix. f(i,j) ∈ [0,1]: 0 =
    * frictionless, 1 = near-impossible. GUS BAEL 2023 inter-industry flows.
    */
  val DefaultFrictionMatrix: Vector[Vector[Share]] = Vector(
    //                     BPO         Mfg         Ret         Hlt         Pub         Agr
    Vector(Share(0.0), Share(0.6), Share(0.3), Share(0.8), Share(0.5), Share(0.9)), // BPO
    Vector(Share(0.6), Share(0.0), Share(0.5), Share(0.7), Share(0.6), Share(0.4)), // Mfg
    Vector(Share(0.3), Share(0.5), Share(0.0), Share(0.5), Share(0.4), Share(0.6)), // Ret
    Vector(Share(0.8), Share(0.7), Share(0.5), Share(0.0), Share(0.4), Share(0.8)), // Hlt
    Vector(Share(0.5), Share(0.6), Share(0.4), Share(0.4), Share(0.0), Share(0.7)), // Pub
    Vector(Share(0.9), Share(0.4), Share(0.6), Share(0.8), Share(0.7), Share(0.0)), // Agr
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
    (0 until NumSectors).map(s => if counts(s) > 0 then sums(s) / counts(s).toLong else PLN.Zero).toVector

  /** Probabilistic target sector selection (gravity model). score(to) =
    * wage(to) × (vacancies(to) + 1)^vacancyWeight × (1 − friction(from,to)).
    */
  @computationBoundary
  def selectTargetSector(
      from: Int,
      wages: Vector[PLN],
      vacancies: Vector[Int],
      matrix: Vector[Vector[Share]],
      vacancyWeight: Coefficient,
      rng: Random,
  )(using @unused p: SimParams): Int =
    import ComputationBoundary.toDouble
    val scores = gravityScores(from, wages, vacancies, matrix, vacancyWeight)
    val total  = scores.sum
    if total <= 0.0 then uniformFallback(from, rng)
    else rouletteSelect(scores, from, rng.nextDouble() * total)

  /** Gravity-model attractiveness score per destination sector. */
  @computationBoundary
  private def gravityScores(
      from: Int,
      wages: Vector[PLN],
      vacancies: Vector[Int],
      matrix: Vector[Vector[Share]],
      vacancyWeight: Coefficient,
  ): Vector[Double] =
    import ComputationBoundary.toDouble
    (0 until NumSectors).map { to =>
      if to == from then 0.0
      else
        toDouble(wages(to).max(PLN.Zero)) *
          Math.pow(vacancies(to).toDouble + 1.0, toDouble(vacancyWeight)) *
          (1.0 - toDouble(matrix(from)(to)))
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
  def frictionAdjustedParams(friction: Share, durationMult: Multiplier, costMult: Share)(using p: SimParams): RetrainingParams =
    val frictionDurAdj = friction * durationMult                             // Share × Multiplier → Multiplier
    val totalDurMult   = Multiplier.One + frictionDurAdj                     // Multiplier + Multiplier
    val adjDuration    = ((p.household.retrainingDuration.toLong * totalDurMult.toLong + 5000L) / 10000L).toInt
    val costAdj        = Multiplier.One + (friction * costMult).toMultiplier // Share × Share → Share → Multiplier
    val adjCost        = p.household.retrainingCost * costAdj
    RetrainingParams(adjDuration, adjCost)

  /** Cross-sector wage penalty: proportional to friction, max 30%. */
  def crossSectorWagePenalty(friction: Share): Multiplier =
    (Share.One - friction * MaxWagePenalty).toMultiplier

  /** Friction-adjusted success probability for retraining. */
  @computationBoundary
  def frictionAdjustedSuccess(baseProb: Double, friction: Share): Double =
    import ComputationBoundary.toDouble
    baseProb * (1.0 - toDouble(friction * FrictionSuccessDiscount))
