package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Residential real estate market: prices, mortgages, wealth effects, and
  * regional disaggregation.
  *
  * Models the Polish housing market with income/rate-driven price dynamics,
  * mortgage origination subject to LTV limits (KNF Recommendation S), mortgage
  * default with unemployment sensitivity, housing wealth effects on
  * consumption, and 7-region disaggregation (Warsaw, Krakow, Wroclaw, Gdansk,
  * Lodz, Poznan, rest-of-Poland). Calibrated to NBP residential price survey
  * 2024, KNF Recommendation S, and GUS wage surveys 2024.
  *
  * Stock values (`initValue`, `initMortgage`) are in raw PLN — scaled by
  * `gdpRatio` in `SimParams.defaults`.
  *
  * @param initHpi
  *   initial house price index (base = 100)
  * @param initValue
  *   initial aggregate housing stock value in raw PLN (~3.0 bln PLN, scaled by
  *   gdpRatio)
  * @param initMortgage
  *   initial aggregate mortgage stock in raw PLN (NBP 2024: ~485 mld PLN,
  *   scaled by gdpRatio)
  * @param priceIncomeElast
  *   elasticity of house prices to income growth
  * @param priceRateElast
  *   elasticity of house prices to interest rate changes (negative = rate hikes
  *   depress prices)
  * @param priceReversion
  *   monthly mean-reversion speed of HPI toward fundamental value
  * @param mortgageSpread
  *   mortgage rate spread over policy rate (NBP 2024: ~2.5pp)
  * @param mortgageMaturity
  *   average mortgage maturity in months (25 years = 300, KNF Recommendation S)
  * @param ltvMax
  *   maximum loan-to-value ratio (KNF Recommendation S: 80%)
  * @param originationRate
  *   monthly mortgage origination as fraction of housing stock
  * @param defaultBase
  *   base monthly mortgage default rate
  * @param defaultUnempSens
  *   sensitivity of mortgage default to unemployment rate
  * @param mortgageRecovery
  *   recovery rate on defaulted mortgages (collateral value)
  * @param wealthMpc
  *   marginal propensity to consume from housing wealth (Case, Quigley &
  *   Shiller 2005)
  * @param rentalYield
  *   annual rental yield as fraction of property value (Otodom/NBP: ~4.5%)
  * @param regionalMarkets
  *   keyed regional configuration preserving market identity for Warsaw,
  *   Krakow, Wroclaw, Gdansk, Lodz, Poznan, and rest-of-Poland
  */
case class HousingConfig(
    initHpi: PriceIndex = PriceIndex(100.0),
    initValue: PLN = PLN(3.0e12),   // raw — scaled by gdpRatio
    initMortgage: PLN = PLN(485e9), // raw — scaled by gdpRatio
    priceIncomeElast: Coefficient = Coefficient(1.2),
    priceRateElast: Coefficient = Coefficient(-0.8),
    priceReversion: Coefficient = Coefficient(0.05),
    mortgageSpread: Rate = Rate(0.025),
    mortgageMaturity: Int = 300,
    ltvMax: Share = Share(0.80),
    originationRate: Share = Share(0.003),
    defaultBase: Share = Share(0.001),
    defaultUnempSens: Coefficient = Coefficient(0.05),
    mortgageRecovery: Share = Share(0.70),
    wealthMpc: Share = Share(0.05),
    rentalYield: Rate = Rate(0.045),
    regionalMarkets: Vector[HousingConfig.RegionalMarketConfig] = HousingConfig.DefaultRegionalMarkets,
):
  require(ltvMax > Share.Zero && ltvMax <= Share.One, s"ltvMax must be in (0,1]: $ltvMax")
  require(mortgageMaturity > 0, s"mortgageMaturity must be positive: $mortgageMaturity")
  require(initValue >= PLN.Zero, s"initValue must be non-negative: $initValue")

  require(
    regionalMarkets.length == HousingConfig.RegionalMarket.count,
    s"regionalMarkets must have ${HousingConfig.RegionalMarket.count} regions in order ${HousingConfig.RegionalMarket.labels.mkString(" -> ")}, got ${regionalMarkets.length}",
  )
  require(
    regionalMarkets.map(_.market) == HousingConfig.RegionalMarket.all,
    s"regionalMarkets must preserve region order ${HousingConfig.RegionalMarket.labels.mkString(" -> ")}, got ${regionalMarkets.map(_.market.label).mkString(" -> ")}",
  )
  regionalMarkets.foreach: marketConfig =>
    require(
      marketConfig.valueShare >= Share.Zero && marketConfig.valueShare <= Share.One,
      s"regionalMarkets valueShare for ${marketConfig.market.label} must be in [0,1], got ${marketConfig.valueShare}",
    )
    require(
      marketConfig.mortgageShare >= Share.Zero && marketConfig.mortgageShare <= Share.One,
      s"regionalMarkets mortgageShare for ${marketConfig.market.label} must be in [0,1], got ${marketConfig.mortgageShare}",
    )

  private val regionalValueShareSum    = regionalMarkets.foldLeft(Share.Zero)((acc, market) => acc + market.valueShare)
  private val regionalMortgageShareSum = regionalMarkets.foldLeft(Share.Zero)((acc, market) => acc + market.mortgageShare)

  require(regionalValueShareSum == Share.One, s"regionalValueShares must sum to 1.0, got $regionalValueShareSum")
  require(regionalMortgageShareSum == Share.One, s"regionalMortgageShares must sum to 1.0, got $regionalMortgageShareSum")

object HousingConfig:

  enum RegionalMarket(val columnPrefix: String, val label: String):
    case Warsaw       extends RegionalMarket("Waw", "Warsaw")
    case Krakow       extends RegionalMarket("Krk", "Krakow")
    case Wroclaw      extends RegionalMarket("Wro", "Wroclaw")
    case Gdansk       extends RegionalMarket("Gdn", "Gdansk")
    case Lodz         extends RegionalMarket("Ldz", "Lodz")
    case Poznan       extends RegionalMarket("Poz", "Poznan")
    case RestOfPoland extends RegionalMarket("Rest", "Rest of Poland")

    def hpiColName: String = s"${columnPrefix}Hpi"

  object RegionalMarket:
    val all: Vector[RegionalMarket] = values.toVector
    val count: Int                  = all.length
    val labels: Vector[String]      = all.map(_.label)

  final case class RegionalMarketConfig(
      market: RegionalMarket,
      initHpi: PriceIndex,
      valueShare: Share,
      mortgageShare: Share,
      gamma: Coefficient,
      incomeMultiplier: Multiplier,
  )

  private[amorfati] val DefaultRegionalMarkets: Vector[RegionalMarketConfig] =
    Vector(
      RegionalMarketConfig(RegionalMarket.Warsaw, PriceIndex(230.0), Share(0.25), Share(0.30), Coefficient(0.03), Multiplier(1.35)),
      RegionalMarketConfig(RegionalMarket.Krakow, PriceIndex(190.0), Share(0.08), Share(0.10), Coefficient(0.04), Multiplier(1.15)),
      RegionalMarketConfig(RegionalMarket.Wroclaw, PriceIndex(170.0), Share(0.07), Share(0.08), Coefficient(0.04), Multiplier(1.10)),
      RegionalMarketConfig(RegionalMarket.Gdansk, PriceIndex(175.0), Share(0.08), Share(0.09), Coefficient(0.04), Multiplier(1.12)),
      RegionalMarketConfig(RegionalMarket.Lodz, PriceIndex(110.0), Share(0.04), Share(0.04), Coefficient(0.06), Multiplier(0.95)),
      RegionalMarketConfig(RegionalMarket.Poznan, PriceIndex(140.0), Share(0.05), Share(0.06), Coefficient(0.05), Multiplier(1.05)),
      RegionalMarketConfig(RegionalMarket.RestOfPoland, PriceIndex(100.0), Share(0.43), Share(0.33), Coefficient(0.06), Multiplier(0.82)),
    )
