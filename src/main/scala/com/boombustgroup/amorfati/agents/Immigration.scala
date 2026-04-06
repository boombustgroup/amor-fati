package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

import scala.util.Random

/** Immigration: tracks immigrant stock, flows, remittances, spawning/removal.
  */
object Immigration:
  private val MaxInitSavings = PLN(5000.0)
  private val MinInitRent    = PLN(800.0)
  private val ImmigrantMpcMu = Share(0.85)
  private val ImmigrantMpcLo = Share(0.7)
  private val ImmigrantMpcHi = Share(0.98)
  private val SkillNoiseStd  = 0.15
  private val MpcNoiseStd    = 0.05

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
  private val HalfShare = Share(0.5)

  def computeInflow(wage: PLN, unempRate: Share)(using p: SimParams): Int =
    val basePop = p.pop.firmsCount * p.pop.workersPerFirm
    val wageGap = (wage.ratioTo(p.immigration.foreignWage) - Scalar.One).max(Scalar.Zero)
    val pull    = p.immigration.wageElasticity * wageGap.toMultiplier
    val push    = (Share.One - unempRate).max(Share.Zero)
    val rate    = p.immigration.monthlyRate * (HalfShare + (HalfShare * pull * push))
    rate.applyTo(basePop).max(0)

  /** Monthly return migration (outflow). */
  def computeOutflow(immigrantStock: Int)(using p: SimParams): Int =
    p.immigration.returnRate.applyTo(immigrantStock).max(0)

  /** Total remittance outflow from immigrant HH. Remittances = employed
    * immigrant wages × remittance rate.
    */
  def computeRemittances(immigrantHH: Iterable[Household.State])(using p: SimParams): PLN =
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
    val r      = rng.nextLong(total.max(1L))
    val cumul  = shares.scanLeft(0L)(_ + _).tail
    val picked = cumul.indexWhere(_ > r)
    SectorIdx(if picked >= 0 then picked else p.sectorDefs.length - 1)

  /** Spawn new immigrant households. Start as Unemployed(0) — matched in next
    * jobSearch round.
    */
  @boundaryEscape
  def spawnImmigrants(count: Int, startId: Int, rng: Random)(using p: SimParams): Vector[Household.State] =
    (0 until count).map { i =>
      inline def gaussianNoiseRaw(std: Double): Long =
        math.round(rng.nextGaussian() * std * com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD)

      val sector                       = chooseSector(rng)
      val edu                          = p.social.drawImmigrantEducation(rng)
      val (skillFloorS, skillCeilingS) = p.social.eduSkillRange(edu)
      val skill                        = Share.fromRaw((p.immigration.skillMean.toLong + gaussianNoiseRaw(SkillNoiseStd)).max(skillFloorS.toLong).min(skillCeilingS.toLong))
      val savings                      = PLN.fromRaw(rng.nextLong(MaxInitSavings.toLong.max(1L)))
      val mpc                          = Share.fromRaw((ImmigrantMpcMu.toLong + gaussianNoiseRaw(MpcNoiseStd)).max(ImmigrantMpcLo.toLong).min(ImmigrantMpcHi.toLong))
      val region                       = Region.cdfSample(rng)
      val baseRent                     = PLN.fromRaw((p.household.rentMean.toLong + math.round(p.household.rentStd.toLong.toDouble * rng.nextGaussian())).max(MinInitRent.toLong))
      val rent                         = (baseRent * region.housingCostIndex).max(MinInitRent)
      val numChildren                  =
        Distributions.poissonSample(p.fiscal.social800ChildrenPerHh, rng)
      Household.State(
        id = HhId(startId + i),
        savings = savings,
        debt = PLN.Zero,
        monthlyRent = rent,
        skill = skill,
        healthPenalty = Share.Zero,
        mpc = mpc,
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
      unempRate: Share,
  )(using SimParams): State =
    val inflow      = computeInflow(wage, unempRate)
    val outflow     = computeOutflow(prev.immigrantStock)
    val newStock    = (prev.immigrantStock + inflow - outflow).max(0)
    val remittances = computeRemittances(households)
    State(newStock, inflow, outflow, remittances)
