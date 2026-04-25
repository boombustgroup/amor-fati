package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.random.RandomStream
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
  case Central extends Region("Centralny", Multiplier.decimal(135, 2), Share.decimal(3, 2), Multiplier.decimal(180, 2), Share.decimal(21, 2))

  /** Południowy: Śląsk + Małopolska. Industry + services mix. Kraków tech hub.
    * Mining restrukturyzacja in Śląsk.
    */
  case South extends Region("Poludniowy", Multiplier.decimal(110, 2), Share.decimal(5, 2), Multiplier.decimal(120, 2), Share.decimal(21, 2))

  /** Wschodni: Lubelskie, Podkarpackie, Podlaskie, Świętokrzyskie. Highest
    * unemployment, lowest wages. Agriculture-heavy. "Ściana wschodnia".
    */
  case East extends Region("Wschodni", Multiplier.decimal(80, 2), Share.decimal(9, 2), Multiplier.decimal(65, 2), Share.decimal(14, 2))

  /** Północno-zachodni: Wielkopolskie, Zachodniopomorskie, Lubuskie. Balanced
    * economy, Poznań as regional center, moderate wages.
    */
  case Northwest extends Region("Polnocno-zachodni", Multiplier(1), Share.decimal(5, 2), Multiplier.decimal(90, 2), Share.decimal(16, 2))

  /** Południowo-zachodni: Dolnośląskie, Opolskie. Wrocław tech hub. Growing
    * services sector, moderate housing costs.
    */
  case Southwest extends Region("Poludniowo-zachodni", Multiplier.decimal(105, 2), Share.decimal(5, 2), Multiplier(1), Share.decimal(12, 2))

  /** Północny: Kujawsko-pomorskie, Warmińsko-mazurskie, Pomorskie.
    * Gdańsk/Gdynia port economy. Warmia high unemployment.
    */
  case North extends Region("Polnocny", Multiplier.decimal(95, 2), Share.decimal(7, 2), Multiplier.decimal(85, 2), Share.decimal(16, 2))

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
  def cdfSample(rng: RandomStream): Region =
    all(Distributions.cdfSample(all.map(_.populationShare), rng))

  /** Migration friction matrix: `friction(from)(to)` ∈ [0,1]. 0 = no friction
    * (same region), 1 = maximum friction. Asymmetric: easier to move TO Central
    * (jobs pull) than FROM Central. Based on GUS internal migration data 2024.
    */
  val frictionMatrix: Vector[Vector[Share]] = Vector(
    //          Central  South  East   NW     SW     North
    Vector(Share(0), Share.decimal(40, 2), Share.decimal(60, 2), Share.decimal(45, 2), Share.decimal(45, 2), Share.decimal(50, 2)), // from Central
    Vector(Share.decimal(30, 2), Share(0), Share.decimal(50, 2), Share.decimal(40, 2), Share.decimal(30, 2), Share.decimal(45, 2)), // from South
    Vector(
      Share.decimal(25, 2),
      Share.decimal(45, 2),
      Share(0),
      Share.decimal(55, 2),
      Share.decimal(55, 2),
      Share.decimal(50, 2),
    ),                                                                                                                              // from East (easier to Central)
    Vector(Share.decimal(35, 2), Share.decimal(40, 2), Share.decimal(55, 2), Share(0), Share.decimal(35, 2), Share.decimal(30, 2)), // from Northwest
    Vector(Share.decimal(35, 2), Share.decimal(30, 2), Share.decimal(55, 2), Share.decimal(35, 2), Share(0), Share.decimal(40, 2)), // from Southwest
    Vector(Share.decimal(30, 2), Share.decimal(40, 2), Share.decimal(50, 2), Share.decimal(30, 2), Share.decimal(40, 2), Share(0)), // from North
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
    Vector(Share.decimal(8, 2), Share.decimal(12, 2), Share.decimal(48, 2), Share.decimal(6, 2), Share.decimal(20, 2), Share.decimal(6, 2)),  // Central
    Vector(Share.decimal(4, 2), Share.decimal(25, 2), Share.decimal(40, 2), Share.decimal(6, 2), Share.decimal(18, 2), Share.decimal(7, 2)),  // South
    Vector(Share.decimal(1, 2), Share.decimal(15, 2), Share.decimal(30, 2), Share.decimal(6, 2), Share.decimal(23, 2), Share.decimal(25, 2)), // East
    Vector(Share.decimal(3, 2), Share.decimal(22, 2), Share.decimal(38, 2), Share.decimal(6, 2), Share.decimal(20, 2), Share.decimal(11, 2)), // Northwest
    Vector(Share.decimal(4, 2), Share.decimal(22, 2), Share.decimal(40, 2), Share.decimal(6, 2), Share.decimal(19, 2), Share.decimal(9, 2)),  // Southwest
    Vector(Share.decimal(2, 2), Share.decimal(18, 2), Share.decimal(35, 2), Share.decimal(6, 2), Share.decimal(22, 2), Share.decimal(17, 2)), // North
  )
