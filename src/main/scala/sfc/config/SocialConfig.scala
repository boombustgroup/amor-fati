package sfc.config

import sfc.types.*

/** Social security, pensions, demographics, and education.
  *
  * Covers ZUS (Social Insurance Institution) contributions and pension
  * payments, PPK (Employee Capital Plans) with three-asset allocation,
  * demographic transitions (retirement, working-age decline), and education
  * system with 4-tier attainment (primary, vocational, secondary, tertiary),
  * sector-specific composition, wage premia, retraining multipliers, and skill
  * ranges. Calibrated to ZUS 2024, Ustawa o PPK, GUS LFS 2024.
  *
  * @param zusContribRate
  *   total ZUS contribution rate as fraction of gross wage (Ustawa o systemie
  *   ubezpieczen spolecznych: 19.52%)
  * @param zusBasePension
  *   average monthly pension payment (PLN, ZUS 2024: ~3,500)
  * @param zusScale
  *   scaling factor for pension payments (for sensitivity analysis)
  * @param ppkEmployeeRate
  *   PPK employee contribution rate (Ustawa o PPK: 2%)
  * @param ppkEmployerRate
  *   PPK employer contribution rate (Ustawa o PPK: 1.5%)
  * @param ppkBondAlloc
  *   PPK bond allocation share (remainder split across corp bonds + equities)
  * @param demRetirementRate
  *   monthly retirement transition rate (fraction of working-age population)
  * @param demWorkingAgeDecline
  *   annual decline rate of working-age population (GUS 2024 projections)
  * @param demInitialRetirees
  *   initial retiree count (0 = built from flow during simulation)
  * @param eduShares
  *   population share by education tier (4 tiers: primary, vocational,
  *   secondary, tertiary; GUS LFS 2024)
  * @param eduSectorShares
  *   optional sector-specific education composition (6 sectors x 4 tiers,
  *   default from GUS)
  * @param eduWagePreemia
  *   wage multiplier by education tier (GUS 2024: primary 0.70, vocational
  *   0.85, secondary 1.00, tertiary 1.30)
  * @param eduRetrainMult
  *   retraining success multiplier by education tier
  * @param eduSkillFloors
  *   minimum initial skill by education tier
  * @param eduSkillCeilings
  *   maximum initial skill by education tier
  * @param eduImmigShares
  *   education tier distribution among immigrants (GUS/NBP 2024)
  */
case class SocialConfig(
    // ZUS (Ustawa o systemie ubezpieczen spolecznych)
    zusContribRate: Rate = Rate(0.1952),
    zusBasePension: PLN = PLN(3500.0),
    zusScale: Double = 1.0,
    // PPK (Ustawa o PPK)
    ppkEmployeeRate: Rate = Rate(0.02),
    ppkEmployerRate: Rate = Rate(0.015),
    ppkBondAlloc: Ratio = Ratio(0.60),
    // Demographics (GUS 2024)
    demRetirementRate: Rate = Rate(0.001),
    demWorkingAgeDecline: Rate = Rate(0.002),
    demInitialRetirees: Int = 0,
    // Education (GUS LFS 2024)
    eduShares: Vector[Ratio] = Vector(Ratio(0.08), Ratio(0.25), Ratio(0.30), Ratio(0.37)),
    eduSectorShares: Option[Vector[Vector[Double]]] = None,
    eduWagePreemia: Vector[Double] = Vector(0.70, 0.85, 1.00, 1.30),
    eduRetrainMult: Vector[Double] = Vector(0.67, 0.83, 1.00, 1.25),
    eduSkillFloors: Vector[Ratio] = Vector(Ratio(0.30), Ratio(0.35), Ratio(0.45), Ratio(0.55)),
    eduSkillCeilings: Vector[Ratio] = Vector(Ratio(0.75), Ratio(0.85), Ratio(0.95), Ratio(1.00)),
    eduImmigShares: Vector[Ratio] = Vector(Ratio(0.15), Ratio(0.40), Ratio(0.35), Ratio(0.10)),
):

  private val defaultEduSectorShares: Vector[Vector[Double]] = Vector(
    Vector(0.02, 0.10, 0.28, 0.60),
    Vector(0.08, 0.40, 0.32, 0.20),
    Vector(0.06, 0.22, 0.38, 0.34),
    Vector(0.02, 0.15, 0.23, 0.60),
    Vector(0.03, 0.08, 0.25, 0.64),
    Vector(0.15, 0.45, 0.30, 0.10),
  )

  /** Draw education tier for a worker in given sector using CDF sampling. */
  def drawEducation(sectorIdx: Int, rng: scala.util.Random): Int =
    val shares = eduSectorShares.getOrElse(defaultEduSectorShares)(sectorIdx.max(0).min(5))
    SocialConfig.cdfSample(shares, rng)

  /** Draw education tier for an immigrant worker. */
  def drawImmigrantEducation(rng: scala.util.Random): Int =
    SocialConfig.cdfSample(eduImmigShares.map(_.toDouble), rng)

  /** Wage premium multiplier for given education tier (0-3). */
  def eduWagePremium(education: Int): Double =
    eduWagePreemia(education.max(0).min(3))

  /** Retraining success multiplier for given education tier (0-3). */
  def eduRetrainMultiplier(education: Int): Double =
    eduRetrainMult(education.max(0).min(3))

  /** Skill floor and ceiling for given education tier (0-3). */
  def eduSkillRange(education: Int): (Double, Double) =
    val idx = education.max(0).min(3)
    (eduSkillFloors(idx).toDouble, eduSkillCeilings(idx).toDouble)

object SocialConfig:
  /** Sample a categorical index from a probability vector using the inverse CDF
    * method.
    *
    * Builds a cumulative distribution from `shares`, draws a uniform random
    * number, and returns the first index whose cumulative probability exceeds
    * the draw. The last category absorbs any floating-point residual.
    */
  private[config] def cdfSample(shares: Vector[Double], rng: scala.util.Random): Int =
    val r   = rng.nextDouble()
    var cum = 0.0
    var i   = 0
    while i < shares.length - 1 do
      cum += shares(i)
      if r < cum then return i
      i += 1
    shares.length - 1
