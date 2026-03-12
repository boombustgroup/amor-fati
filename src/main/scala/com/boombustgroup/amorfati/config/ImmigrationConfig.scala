package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Labor immigration: inflow dynamics, sectoral allocation, skill profile, and
  * return migration.
  *
  * Models endogenous labor immigration driven by wage differentials (NBP 2023
  * survey), with sector-specific allocation (GUS LFS 2024), skill distribution,
  * wage discount for immigrants, and return migration. Immigrant workers are
  * added to the household agent population and affect labor supply, ZUS
  * contributions, and (optionally) 800+ eligibility.
  *
  * @param monthlyRate
  *   base monthly immigration rate as fraction of labor force
  * @param wageElasticity
  *   elasticity of immigration to domestic/foreign wage ratio
  * @param foreignWage
  *   reference foreign wage for push-pull calculation (PLN equivalent)
  * @param remitRate
  *   fraction of immigrant income sent as remittances (outflow, NBP 2023)
  * @param returnRate
  *   monthly probability of return migration
  * @param sectorShares
  *   sectoral allocation of new immigrants (6 sectors, GUS LFS 2024)
  * @param skillMean
  *   mean skill level of immigrant workers (0-1 scale)
  * @param wageDiscount
  *   initial wage discount for immigrants vs. natives (NBP 2023 survey: ~20%)
  * @param initStock
  *   initial immigrant stock at simulation start (number of workers)
  */
case class ImmigrationConfig(
    monthlyRate: Ratio = Ratio(0.001),
    wageElasticity: Double = 2.0,
    foreignWage: PLN = PLN(4000.0),
    remitRate: Ratio = Ratio(0.15),
    returnRate: Ratio = Ratio(0.005),
    sectorShares: Vector[Ratio] = Vector(Ratio(0.05), Ratio(0.35), Ratio(0.25), Ratio(0.05), Ratio(0.05), Ratio(0.25)),
    skillMean: Ratio = Ratio(0.45),
    wageDiscount: Ratio = Ratio(0.20),
    initStock: Int = 0,
):
  require(sectorShares.length == 6, s"sectorShares must have 6 sectors: ${sectorShares.length}")
