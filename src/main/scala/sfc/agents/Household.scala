package sfc.agents

import sfc.config.Config

import scala.util.Random

// ---- Aggregate household state (backward-compat, used in both modes) ----

case class HhState(
  employed: Int,
  marketWage: Double,
  reservationWage: Double,
  totalIncome: Double,
  consumption: Double,
  domesticConsumption: Double,
  importConsumption: Double
)

// ---- Individual household types (Paper-06) ----

enum HhStatus:
  case Employed(firmId: Int, sectorIdx: Int, wage: Double)
  case Unemployed(monthsUnemployed: Int)
  case Retraining(monthsLeft: Int, targetSector: Int, cost: Double)
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
  socialNeighbors: Array[Int]
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
  totalDebtService: Double
)

object HouseholdInit:
  /** Initialize N households, all employed, assigned to firms round-robin. */
  def initialize(nHouseholds: Int, nFirms: Int, firms: Array[Firm],
                 socialNetwork: Array[Array[Int]], rng: Random): Vector[Household] =
    val sectorShares = firms.groupBy(_.sector).view.mapValues(_.length).toMap

    (0 until nHouseholds).map { i =>
      // Round-robin firm assignment
      val firmId = i % nFirms
      val sectorIdx = firms(firmId).sector

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

      // Skill: Uniform(0.3, 1.0), with sector correlation
      val sectorSigma = sfc.config.SECTORS(sectorIdx).sigma
      val baseSkill = 0.3 + 0.7 * rng.nextDouble()
      val sectorBonus = Math.min(0.1, 0.02 * Math.log(sectorSigma))
      val skill = Math.max(0.3, Math.min(1.0, baseSkill + sectorBonus))

      val wage = Config.BaseWage * sfc.config.SECTORS(sectorIdx).wageMultiplier * skill

      Household(
        id = i,
        savings = savings,
        debt = debt,
        monthlyRent = rent,
        skill = skill,
        healthPenalty = 0.0,
        mpc = Math.max(0.5, Math.min(0.98, mpc)),
        status = HhStatus.Employed(firmId, sectorIdx, wage),
        socialNeighbors = if i < socialNetwork.length then socialNetwork(i) else Array.empty
      )
    }.toVector

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
