package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

import com.boombustgroup.amorfati.random.RandomStream

/** Immigration: tracks immigrant stock, flows, remittances, spawning/removal.
  */
object Immigration:
  private val MaxInitSavings = PLN(5000.0)
  private val MinInitRent    = PLN(800.0)
  private val ImmigrantMpcMu = Share(0.85)
  private val ImmigrantMpcLo = Share(0.7)
  private val ImmigrantMpcHi = Share(0.98)
  private val SkillNoiseStd  = Scalar(0.15)
  private val MpcNoiseStd    = Scalar(0.05)

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
    immigrantHH.foldLeft(PLN.Zero): (acc, h) =>
      if !h.isImmigrant then acc
      else
        h.status match
          case HhStatus.Employed(_, _, wage) => acc + (wage * p.immigration.remitRate)
          case _                             => acc

  /** Choose sector for new immigrant (weighted by sectorShares). */
  def chooseSector(rng: RandomStream)(using p: SimParams): SectorIdx =
    SectorIdx(Distributions.cdfSample(p.immigration.sectorShares, rng))

  /** Spawn new immigrant households with their opening ledger financial stock.
    * They start as Unemployed(0) and are matched in the next jobSearch round.
    */
  def spawnImmigrantPopulation(count: Int, startId: Int, rng: RandomStream)(using p: SimParams): Household.Population =
    import ComputationBoundary.toDouble
    val sampled = (0 until count).map { i =>
      val sector                       = chooseSector(rng)
      val edu                          = p.social.drawImmigrantEducation(rng)
      val (skillFloorS, skillCeilingS) = p.social.eduSkillRange(edu)
      val skill                        = Distributions.gaussianShare(p.immigration.skillMean, SkillNoiseStd, skillFloorS, skillCeilingS, rng)
      val savings                      = Distributions.randomPlnBelow(MaxInitSavings, rng)
      val mpc                          = Distributions.gaussianShare(ImmigrantMpcMu, MpcNoiseStd, ImmigrantMpcLo, ImmigrantMpcHi, rng)
      val region                       = Region.cdfSample(rng)
      val baseRent                     = Distributions.gaussianPlnAtLeast(p.household.rentMean, p.household.rentStd, MinInitRent, rng)
      val rent                         = (baseRent * region.housingCostIndex).max(MinInitRent)
      val numChildren                  = Distributions.poissonSample(toDouble(p.fiscal.social800ChildrenPerHh), rng)

      val household = Household.State(
        id = HhId(startId + i),
        monthlyRent = rent,
        skill = skill,
        healthPenalty = Share.Zero,
        mpc = mpc,
        status = HhStatus.Unemployed(0),
        socialNeighbors = Array.empty[HhId],
        bankId = BankId(0),
        lastSectorIdx = sector,
        isImmigrant = true,
        numDependentChildren = numChildren,
        education = edu,
        taskRoutineness = Household.Init.sampleTaskRoutineness(edu, sector, rng),
        wageScar = Share.Zero,
        contractType = ContractType.sampleForSector(sector, rng),
        region = region,
      )
      val stocks    = Household.FinancialStocks(
        demandDeposit = savings,
        mortgageLoan = PLN.Zero,
        consumerLoan = PLN.Zero,
        equity = PLN.Zero,
      )
      (household, stocks)
    }.toVector
    Household.Population(sampled.map(_._1), sampled.map(_._2))

  /** State-only convenience for behavioral tests and callers that do not need
    * opening financial balances.
    */
  def spawnImmigrants(count: Int, startId: Int, rng: RandomStream)(using p: SimParams): Vector[Household.State] =
    spawnImmigrantPopulation(count, startId, rng).households

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
