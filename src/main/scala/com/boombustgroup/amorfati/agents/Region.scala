package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

/** NUTS-1 macroregions of Poland for regional labor market dynamics.
  *
  * Six macroregions with distinct labor market characteristics: unemployment
  * rates, wage levels, sector composition, and housing costs. Geographic
  * friction creates structural unemployment — workers cannot instantly relocate
  * from Warmia (10% unemployment) to Warsaw (3%).
  *
  * Migration between regions is costly: housing cost differential acts as
  * barrier, migration probability decreases with friction distance. This
  * generates persistent regional inequality that aggregate models miss.
  *
  * Calibration: GUS BAEL 2024 (regional labor force surveys), GUS average wages
  * by voivodeship, NBP residential price survey.
  */
enum Region(
    val label: String,
    val wageMultiplier: Multiplier,
    val baseUnemployment: Share,
    val housingCostIndex: Multiplier,
    val populationShare: Share,
):
  /** Centralny: Warszawa + mazowieckie. Highest wages, lowest unemployment,
    * most expensive housing. Services/BPO hub.
    */
  case Central extends Region("Centralny", Multiplier(1.35), Share(0.03), Multiplier(1.80), Share(0.21))

  /** Południowy: Śląsk + Małopolska. Industry + services mix. Kraków tech hub.
    * Mining restrukturyzacja in Śląsk.
    */
  case South extends Region("Poludniowy", Multiplier(1.10), Share(0.05), Multiplier(1.20), Share(0.21))

  /** Wschodni: Lubelskie, Podkarpackie, Podlaskie, Świętokrzyskie. Highest
    * unemployment, lowest wages. Agriculture-heavy. "Ściana wschodnia".
    */
  case East extends Region("Wschodni", Multiplier(0.80), Share(0.09), Multiplier(0.65), Share(0.14))

  /** Północno-zachodni: Wielkopolskie, Zachodniopomorskie, Lubuskie. Balanced
    * economy, Poznań as regional center, moderate wages.
    */
  case Northwest extends Region("Polnocno-zachodni", Multiplier(1.00), Share(0.05), Multiplier(0.90), Share(0.16))

  /** Południowo-zachodni: Dolnośląskie, Opolskie. Wrocław tech hub. Growing
    * services sector, moderate housing costs.
    */
  case Southwest extends Region("Poludniowo-zachodni", Multiplier(1.05), Share(0.05), Multiplier(1.00), Share(0.12))

  /** Północny: Kujawsko-pomorskie, Warmińsko-mazurskie, Pomorskie.
    * Gdańsk/Gdynia port economy. Warmia high unemployment.
    */
  case North extends Region("Polnocny", Multiplier(0.95), Share(0.07), Multiplier(0.85), Share(0.16))

object Region:

  val all: Vector[Region] = Vector(Central, South, East, Northwest, Southwest, North)

  val count: Int = all.length

  /** Population-weighted mean regional wage multiplier.
    *
    * Regional wage multipliers express relative wages around the national
    * average, so their population-weighted mean should be normalized to 1.0
    * whenever they are applied to aggregate wages.
    */
  private val weightedMeanWageMultiplier: Multiplier =
    all.foldLeft(Multiplier.Zero)((acc, r) => acc + (r.populationShare * r.wageMultiplier))

  /** Regional wage multiplier normalized to preserve the national average wage.
    */
  def normalizedWageMultiplier(region: Region): Multiplier =
    region.wageMultiplier.ratioTo(weightedMeanWageMultiplier).toMultiplier

  /** Sample a region from population share CDF (inverse transform). */
  def cdfSample(rng: scala.util.Random): Region =
    all(Distributions.cdfSample(all.map(_.populationShare), rng))

  /** Migration friction matrix: `friction(from)(to)` ∈ [0,1]. 0 = no friction
    * (same region), 1 = maximum friction. Asymmetric: easier to move TO Central
    * (jobs pull) than FROM Central. Based on GUS internal migration data 2024.
    */
  val frictionMatrix: Vector[Vector[Share]] = Vector(
    //          Central  South  East   NW     SW     North
    Vector(Share(0.00), Share(0.40), Share(0.60), Share(0.45), Share(0.45), Share(0.50)), // from Central
    Vector(Share(0.30), Share(0.00), Share(0.50), Share(0.40), Share(0.30), Share(0.45)), // from South
    Vector(Share(0.25), Share(0.45), Share(0.00), Share(0.55), Share(0.55), Share(0.50)), // from East (easier to Central)
    Vector(Share(0.35), Share(0.40), Share(0.55), Share(0.00), Share(0.35), Share(0.30)), // from Northwest
    Vector(Share(0.35), Share(0.30), Share(0.55), Share(0.35), Share(0.00), Share(0.40)), // from Southwest
    Vector(Share(0.30), Share(0.40), Share(0.50), Share(0.30), Share(0.40), Share(0.00)), // from North
  )

  /** Migration probability: inverse of friction × wage differential × housing
    * affordability.
    *
    * P(migrate from→to) = (1 − friction) × max(0, wageTo/wageFrom − 1) ×
    * affordability where affordability = max(0, 1 −
    * housingCostTo/housingCostFrom × threshold)
    *
    * Workers only migrate if destination wages justify the housing cost
    * increase.
    */
  def migrationProbability(
      from: Region,
      to: Region,
      wageDiffRatio: Multiplier,
      housingThreshold: Share,
  ): Share =
    if from == to then Share.Zero
    else
      val friction       = frictionMatrix(from.ordinal)(to.ordinal)
      val wagePull       = (wageDiffRatio.toScalar - Scalar.One).max(Scalar.Zero)
      val housingBarrier = (Share.One - to.housingCostIndex.ratioTo(from.housingCostIndex).toShare * housingThreshold).max(Share.Zero)
      ((Share.One - friction) * wagePull.toShare * housingBarrier).clamp(Share.Zero, Share.One)

  /** Sector composition by region (6 sectors × 6 regions). Rows = regions
    * (Central..North), columns = sectors (BPO..Agri). GUS employment by section
    * and voivodeship 2024.
    */
  val sectorComposition: Vector[Vector[Share]] = Vector(
    // BPO    Mfg    Retail  Health  Public  Agri
    Vector(Share(0.08), Share(0.12), Share(0.48), Share(0.06), Share(0.20), Share(0.06)), // Central
    Vector(Share(0.04), Share(0.25), Share(0.40), Share(0.06), Share(0.18), Share(0.07)), // South
    Vector(Share(0.01), Share(0.15), Share(0.30), Share(0.06), Share(0.23), Share(0.25)), // East
    Vector(Share(0.03), Share(0.22), Share(0.38), Share(0.06), Share(0.20), Share(0.11)), // Northwest
    Vector(Share(0.04), Share(0.22), Share(0.40), Share(0.06), Share(0.19), Share(0.09)), // Southwest
    Vector(Share(0.02), Share(0.18), Share(0.35), Share(0.06), Share(0.22), Share(0.17)), // North
  )
