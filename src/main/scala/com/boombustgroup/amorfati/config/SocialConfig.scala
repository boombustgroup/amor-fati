package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

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
  * @param nfzContribRate
  *   NFZ health insurance contribution rate (9%, Ustawa o swiadczeniach opieki
  *   zdrowotnej Art. 79)
  * @param nfzPerCapitaCost
  *   monthly per-capita health spending (NFZ 2024: ~180 mld PLN/yr / 38M pop)
  * @param nfzAgingElasticity
  *   retiree health cost multiplier vs working-age (empirical: ~2.5×, OECD)
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
    zusContribRate: Rate = Rate.decimal(1952, 4),
    zusEmployeeRate: Rate = Rate.decimal(1371, 4), // employee portion deducted from PIT base (emerytura 9.76% + rentowe 1.5% + chorobowe 2.45%)
    zusBasePension: PLN = PLN(3500),
    zusScale: Multiplier = Multiplier(1),
    // NFZ (Ustawa o swiadczeniach opieki zdrowotnej, Art. 79)
    nfzContribRate: Rate = Rate.decimal(9, 2),
    nfzPerCapitaCost: PLN = PLN(1250),
    nfzAgingElasticity: Multiplier = Multiplier.decimal(25, 1),
    // PPK (Ustawa o PPK)
    ppkEmployeeRate: Rate = Rate.decimal(2, 2),
    ppkEmployerRate: Rate = Rate.decimal(15, 3),
    ppkBondAlloc: Share = Share.decimal(60, 2),
    // Demographics (GUS 2024)
    demRetirementRate: Rate = Rate.decimal(1, 3),
    demWorkingAgeDecline: Rate = Rate.decimal(2, 3),
    demInitialRetirees: Int = 0,
    // Education (GUS LFS 2024)
    eduShares: Vector[Share] = Vector(Share.decimal(8, 2), Share.decimal(25, 2), Share.decimal(30, 2), Share.decimal(37, 2)),
    eduSectorShares: Option[Vector[Vector[Share]]] = None,
    eduWagePreemia: Vector[Multiplier] = Vector(Multiplier.decimal(70, 2), Multiplier.decimal(85, 2), Multiplier(1), Multiplier.decimal(130, 2)),
    eduRetrainMult: Vector[Multiplier] = Vector(Multiplier.decimal(67, 2), Multiplier.decimal(83, 2), Multiplier(1), Multiplier.decimal(125, 2)),
    eduSkillFloors: Vector[Share] = Vector(Share.decimal(30, 2), Share.decimal(35, 2), Share.decimal(45, 2), Share.decimal(55, 2)),
    eduSkillCeilings: Vector[Share] = Vector(Share.decimal(75, 2), Share.decimal(85, 2), Share.decimal(95, 2), Share(1)),
    eduImmigShares: Vector[Share] = Vector(Share.decimal(15, 2), Share.decimal(40, 2), Share.decimal(35, 2), Share.decimal(10, 2)),
):

  private val defaultEduSectorShares: Vector[Vector[Share]] = Vector(
    Vector(Share.decimal(2, 2), Share.decimal(10, 2), Share.decimal(28, 2), Share.decimal(60, 2)),
    Vector(Share.decimal(8, 2), Share.decimal(40, 2), Share.decimal(32, 2), Share.decimal(20, 2)),
    Vector(Share.decimal(6, 2), Share.decimal(22, 2), Share.decimal(38, 2), Share.decimal(34, 2)),
    Vector(Share.decimal(2, 2), Share.decimal(15, 2), Share.decimal(23, 2), Share.decimal(60, 2)),
    Vector(Share.decimal(3, 2), Share.decimal(8, 2), Share.decimal(25, 2), Share.decimal(64, 2)),
    Vector(Share.decimal(15, 2), Share.decimal(45, 2), Share.decimal(30, 2), Share.decimal(10, 2)),
  )

  /** Draw education tier for a worker in given sector using CDF sampling. */
  def drawEducation(sectorIdx: Int, rng: RandomStream): Int =
    val shares = eduSectorShares.getOrElse(defaultEduSectorShares)(sectorIdx.max(0).min(5))
    Distributions.cdfSample(shares, rng)

  /** Draw education tier for an immigrant worker. */
  def drawImmigrantEducation(rng: RandomStream): Int =
    Distributions.cdfSample(eduImmigShares, rng)

  /** Wage premium multiplier for given education tier (0-3). */
  def eduWagePremium(education: Int): Multiplier =
    eduWagePreemia(education.max(0).min(3))

  /** Retraining success multiplier for given education tier (0-3). */
  def eduRetrainMultiplier(education: Int): Multiplier =
    eduRetrainMult(education.max(0).min(3))

  /** Skill floor and ceiling for given education tier (0-3). */
  def eduSkillRange(education: Int): (Share, Share) =
    val idx = education.max(0).min(3)
    (eduSkillFloors(idx), eduSkillCeilings(idx))

object SocialConfig
