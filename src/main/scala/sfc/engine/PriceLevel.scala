package sfc.engine

import sfc.config.{Config, RunConfig}

object PriceLevel:

  def update(
    prevInflation: Double,
    prevPrice: Double,
    demandMult: Double,
    wageGrowth: Double,
    exRateDeviation: Double,
    autoRatio: Double,
    hybridRatio: Double,
    rc: RunConfig,
  ): (Double, Double) =
    val demandPull = (demandMult - 1.0) * 0.15
    val costPush = wageGrowth * 0.25
    val rawImportPush = Math.max(0.0, exRateDeviation) * Config.ImportPropensity * 0.25
    val importPush =
      if Config.OeEnabled then Math.min(rawImportPush, Config.OeImportPushCap)
      else rawImportPush
    val techDeflation = autoRatio * 0.060 + hybridRatio * 0.018
    // Soft floor: beyond -1.5%/month, deflation passes through at 30% rate
    // (models downward price stickiness -- Bewley 1999, Schmitt-Grohe & Uribe 2016)
    val rawMonthly = demandPull + costPush + importPush - techDeflation
    val monthly =
      if rawMonthly >= -0.015 then rawMonthly
      else -0.015 + (rawMonthly + 0.015) * 0.3
    val annualized = monthly * 12.0
    val smoothed = prevInflation * 0.7 + annualized * 0.3
    val newPrice = Math.max(0.30, prevPrice * (1.0 + monthly))
    (smoothed, newPrice)
