package sfc.agents

import sfc.config.Config
import sfc.types.*

import scala.util.Random

// ---- Aggregate household state (backward-compat, used in both modes) ----

case class HhState(
  employed: Int,
  marketWage: Double,
  reservationWage: Double,
  totalIncome: Double,
  consumption: Double,
  domesticConsumption: Double,
  importConsumption: Double,
  minWageLevel: Double = 4666.0,
  minWagePriceLevel: Double = 1.0
)

// ---- Individual household types (Paper-06) ----

enum HhStatus:
  case Employed(firmId: FirmId, sectorIdx: SectorIdx, wage: Double)
  case Unemployed(monthsUnemployed: Int)
  case Retraining(monthsLeft: Int, targetSector: SectorIdx, cost: Double)
  case Bankrupt

case class Household(
  id: Int,
  savings: Double,
  debt: Double,
  monthlyRent: Double,
  skill: Double,
  healthPenalty: Double,
  mpc: Double,
  status: HhStatus,
  socialNeighbors: Array[Int],
  bankId: BankId = BankId(0),   // Multi-bank: index into BankingSectorState.banks
  equityWealth: Double = 0.0,   // GPW: value of equity holdings
  lastSectorIdx: SectorIdx = SectorIdx(-1),  // Sectoral mobility: last sector employed in (-1 = never)
  isImmigrant: Boolean = false, // Immigration: tracks immigrant status for wage discount + remittances
  numDependentChildren: Int = 0, // 800+: children ≤ 18 for social transfers
  consumerDebt: Double = 0.0,   // Consumer credit: outstanding unsecured consumer loan
  education: Int = 2            // Education level: 0=Primary, 1=Vocational, 2=Secondary, 3=Tertiary
)

/** Aggregate statistics computed from individual households (Paper-06). */
case class HhAggregates(
  employed: Int,
  unemployed: Int,
  retraining: Int,
  bankrupt: Int,
  totalIncome: Double,
  consumption: Double,
  domesticConsumption: Double,
  importConsumption: Double,
  marketWage: Double,
  reservationWage: Double,
  giniIndividual: Double,
  giniWealth: Double,
  meanSavings: Double,
  medianSavings: Double,
  povertyRate50: Double,
  bankruptcyRate: Double,
  meanSkill: Double,
  meanHealthPenalty: Double,
  retrainingAttempts: Int,
  retrainingSuccesses: Int,
  consumptionP10: Double,
  consumptionP50: Double,
  consumptionP90: Double,
  meanMonthsToRuin: Double,
  povertyRate30: Double,
  totalRent: Double,
  totalDebtService: Double,
  totalUnempBenefits: Double,
  totalDepositInterest: Double = 0.0,
  crossSectorHires: Int = 0,
  voluntaryQuits: Int = 0,
  sectorMobilityRate: Double = 0.0,
  totalRemittances: Double = 0.0,
  totalPit: Double = 0.0,
  totalSocialTransfers: Double = 0.0,
  totalConsumerDebtService: Double = 0.0,
  totalConsumerOrigination: Double = 0.0,
  totalConsumerDefault: Double = 0.0,
  totalConsumerPrincipal: Double = 0.0
)

object HouseholdInit:
  /** Initialize households, all employed, assigned proportionally to firm sizes. */
  def initialize(nHouseholds: Int, nFirms: Int, firms: Array[Firm],
                 socialNetwork: Array[Array[Int]], rng: Random): Vector[Household] =
    var hhId = 0
    val builder = Vector.newBuilder[Household]

    for f <- firms if FirmOps.isAlive(f) do
      val nWorkers = FirmOps.workers(f)
      val sectorIdx = f.sector
      for _ <- 0 until nWorkers do
        if hhId < nHouseholds then
          // Savings: LogNormal(mu, sigma)
          val savings = Math.exp(Config.HhSavingsMu + Config.HhSavingsSigma * rng.nextGaussian())

          // Debt: 40% have debt
          val debt = if rng.nextDouble() < Config.HhDebtFraction then
            Math.exp(Config.HhDebtMu + Config.HhDebtSigma * rng.nextGaussian())
          else 0.0

          // Rent: Normal(mean, std), floored
          val rent = Math.max(Config.HhRentFloor,
            Config.HhRentMean + Config.HhRentStd * rng.nextGaussian())

          // MPC: Beta(alpha, beta) via gamma transformation
          val mpc = betaSample(Config.HhMpcAlpha, Config.HhMpcBeta, rng)

          // Education draw + skill range
          val edu = sfc.config.Config.drawEducation(sectorIdx.toInt, rng)
          val (skillFloor, skillCeiling) = sfc.config.Config.eduSkillRange(edu)
          val sectorSigma = sfc.config.SECTORS(sectorIdx.toInt).sigma
          val baseSkill = skillFloor + (skillCeiling - skillFloor) * rng.nextDouble()
          val sectorBonus = Math.min(0.1, 0.02 * Math.log(sectorSigma))
          val skill = Math.max(skillFloor, Math.min(skillCeiling, baseSkill + sectorBonus))

          val wage = Config.BaseWage * sfc.config.SECTORS(sectorIdx.toInt).wageMultiplier * skill

          // GPW equity wealth: GpwHhEquityFrac of HH participate, with wealth ∝ savings
          val eqWealth = if sfc.config.Config.GpwHhEquity && rng.nextDouble() < sfc.config.Config.GpwHhEquityFrac then
            savings * 0.05  // ~5% of savings in equity (NBP household wealth survey)
          else 0.0

          // 800+ children: Poisson(λ) per HH
          val numChildren = if sfc.config.Config.Social800Enabled then
            HouseholdInit.poissonSample(sfc.config.Config.Social800ChildrenPerHh, rng)
          else 0

          // Consumer credit: 40% of HH have small consumer loans (reuse HhDebtFraction)
          val consDebt = if rng.nextDouble() < Config.HhDebtFraction then
            Math.exp(Config.HhDebtMu + Config.HhDebtSigma * rng.nextGaussian()) * 0.3
          else 0.0

          builder += Household(
            id = hhId,
            savings = savings,
            debt = debt,
            monthlyRent = rent,
            skill = skill,
            healthPenalty = 0.0,
            mpc = Math.max(0.5, Math.min(0.98, mpc)),
            status = HhStatus.Employed(f.id, sectorIdx, wage),
            socialNeighbors = if hhId < socialNetwork.length then socialNetwork(hhId) else Array.empty,
            equityWealth = eqWealth,
            lastSectorIdx = sectorIdx,
            numDependentChildren = numChildren,
            consumerDebt = consDebt,
            education = edu
          )
          hhId += 1

    builder.result()

  /** Sample from Poisson(lambda) using Knuth algorithm (small λ). */
  private[agents] def poissonSample(lambda: Double, rng: Random): Int =
    if lambda <= 0 then 0
    else
      val L = Math.exp(-lambda)
      var k = 0
      var p = rng.nextDouble()
      while p > L do
        k += 1
        p *= rng.nextDouble()
      k

  /** Sample from Beta(alpha, beta) using two Gamma samples. */
  private def betaSample(alpha: Double, beta: Double, rng: Random): Double =
    val x = gammaSample(alpha, rng)
    val y = gammaSample(beta, rng)
    if x + y > 0 then x / (x + y) else 0.5

  /** Sample from Gamma(shape, 1) using Marsaglia-Tsang method. */
  private def gammaSample(shape: Double, rng: Random): Double =
    if shape < 1.0 then
      gammaSample(shape + 1.0, rng) * Math.pow(rng.nextDouble(), 1.0 / shape)
    else
      val d = shape - 1.0 / 3.0
      val c = 1.0 / Math.sqrt(9.0 * d)
      var result = 0.0
      var done = false
      while !done do
        var x = rng.nextGaussian()
        var v = 1.0 + c * x
        while v <= 0 do
          x = rng.nextGaussian()
          v = 1.0 + c * x
        v = v * v * v
        val u = rng.nextDouble()
        if u < 1.0 - 0.0331 * x * x * x * x then
          result = d * v
          done = true
        else if Math.log(u) < 0.5 * x * x + d * (1.0 - v + Math.log(v)) then
          result = d * v
          done = true
      result
