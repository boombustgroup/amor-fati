package sfc.config

import sfc.types.*

/** Energy costs and climate policy: EU ETS carbon pricing, green capital
  * investment, and decarbonization.
  *
  * Models per-sector energy cost burdens (Eurostat/GUS 2023), EU ETS carbon
  * pricing with price drift (KOBiZE 2024, EC Fit for 55), and green capital
  * investment pathway: firms allocate a budget share to green capital with
  * separate K/L ratios, achieving energy cost discounts up to 30% as green
  * capital stock grows. Green capital has lower depreciation than conventional
  * capital.
  *
  * @param energyCostShares
  *   per-sector energy cost as fraction of revenue (6 sectors, Eurostat/GUS
  *   2023)
  * @param carbonIntensity
  *   per-sector carbon intensity in tCO2/PLN of revenue (6 sectors, KOBiZE
  *   2024)
  * @param etsBasePrice
  *   EU ETS carbon price at simulation start (EUR/tCO2, KOBiZE 2024: ~80 EUR)
  * @param etsPriceDrift
  *   annual drift rate of ETS carbon price (EC Fit for 55 trajectory: ~3%)
  * @param greenKLRatios
  *   per-sector green capital-labor ratio in PLN per worker (6 sectors)
  * @param greenDepRate
  *   annual depreciation rate of green capital (lower than conventional: ~4%)
  * @param greenAdjustSpeed
  *   monthly green capital adjustment speed
  * @param greenMaxDiscount
  *   maximum energy cost discount from full green capital stock (up to 30%)
  * @param greenImportShare
  *   share of green capital investment that is imported
  * @param greenInitRatio
  *   initial green capital as fraction of target (0-1)
  * @param greenBudgetShare
  *   fraction of firm investment budget allocated to green capital (default
  *   20%)
  * @param energyCostReplace
  *   replacement cost fraction for energy infrastructure
  * @param energyDomesticShare
  *   share of energy sourced domestically (vs. imported)
  */
case class ClimateConfig(
    energyCostShares: Vector[Ratio] = Vector(Ratio(0.02), Ratio(0.10), Ratio(0.04), Ratio(0.05), Ratio(0.03), Ratio(0.06)),
    carbonIntensity: Vector[Double] = Vector(0.01, 0.08, 0.02, 0.01, 0.02, 0.04),
    etsBasePrice: Double = 80.0,
    etsPriceDrift: Rate = Rate(0.03),
    greenKLRatios: Vector[PLN] = Vector(PLN(5000.0), PLN(30000.0), PLN(10000.0), PLN(15000.0), PLN(8000.0), PLN(20000.0)),
    greenDepRate: Rate = Rate(0.04),
    greenAdjustSpeed: Ratio = Ratio(0.08),
    greenMaxDiscount: Ratio = Ratio(0.30),
    greenImportShare: Ratio = Ratio(0.35),
    greenInitRatio: Ratio = Ratio(0.10),
    greenBudgetShare: Ratio = Ratio(0.20),
    energyCostReplace: Ratio = Ratio(0.30),
    energyDomesticShare: Ratio = Ratio(0.60),
):
  require(energyCostShares.length == 6, s"energyCostShares must have 6 sectors: ${energyCostShares.length}")
  require(carbonIntensity.length == 6, s"carbonIntensity must have 6 sectors: ${carbonIntensity.length}")
  require(greenKLRatios.length == 6, s"greenKLRatios must have 6 sectors: ${greenKLRatios.length}")
