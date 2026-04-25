package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*

/** Labor market: sectoral mobility, trade unions, and forward-looking
  * expectations.
  *
  * Three interacting mechanisms: (1) sectoral labor mobility with friction
  * matrix (GUS LFS 2024, Shimer 2005), (2) trade unions with per-sector
  * density, wage premia, and downward rigidity (GUS 2024), and (3)
  * forward-looking inflation/wage expectations with adaptive learning and
  * central bank credibility (Carroll 2003, Bewley 1999, NBP target band).
  *
  * @param frictionMatrix
  *   sector-to-sector transition friction matrix (6x6, higher = harder to move)
  * @param frictionDurationMult
  *   multiplier on transition duration (scales friction matrix)
  * @param frictionCostMult
  *   wage penalty during sectoral transition (fraction of wage lost)
  * @param voluntarySearchProb
  *   monthly probability of employed worker searching for better sector
  * @param voluntaryWageThreshold
  *   minimum wage gap (fraction) to trigger voluntary search
  * @param vacancyWeight
  *   weight of vacancy rate in sectoral attractiveness
  * @param adjacentFrictionMax
  *   maximum friction for adjacent sectors (caps transition difficulty)
  * @param unionDensity
  *   per-sector union membership density (6 sectors, GUS 2024)
  * @param unionWagePremium
  *   wage premium for unionized workers (empirical: ~8%)
  * @param unionRigidity
  *   downward nominal wage rigidity imposed by unions (0-1 scale)
  * @param expLambda
  *   adaptive learning rate for inflation expectations (Carroll 2003)
  * @param expCredibilityInit
  *   initial central bank credibility (0-1 scale)
  * @param expCredibilitySpeed
  *   monthly credibility adjustment speed
  * @param expCredibilityThreshold
  *   inflation deviation threshold for credibility loss (pp)
  * @param expWagePassthrough
  *   pass-through of inflation expectations to wage demands
  * @param expBondSensitivity
  *   sensitivity of bond yields to inflation expectations
  */
case class LaborConfig(
    // Sectoral mobility (GUS LFS 2024, Shimer 2005)
    frictionMatrix: Vector[Vector[Share]] = SectoralMobility.DefaultFrictionMatrix,
    frictionDurationMult: Multiplier = Multiplier("1.0"),
    frictionCostMult: Share = Share("0.5"),
    voluntarySearchProb: Share = Share("0.02"),
    voluntaryWageThreshold: Share = Share("0.20"),
    vacancyWeight: Coefficient = Coefficient("2.0"),
    adjacentFrictionMax: Share = Share("0.4"),
    // Unions (GUS 2024)
    unionDensity: Vector[Share] = Vector(Share("0.02"), Share("0.15"), Share("0.03"), Share("0.12"), Share("0.30"), Share("0.04")),
    unionWagePremium: Share = Share("0.08"),
    unionRigidity: Share = Share("0.50"),
    // Skills-biased technological change (Acemoglu & Restrepo 2020, Autor 2024)
    sbtcEduRoutineness: Vector[Share] = Vector(Share("0.80"), Share("0.65"), Share("0.45"), Share("0.25")), // Primary, Vocational, Secondary, Tertiary
    sbtcComplementPremium: Share = Share("0.15"),                                                           // max wage premium for AI-complemented cognitive workers
    // Expectations (Carroll 2003, Bewley 1999)
    expLambda: Coefficient = Coefficient("0.70"),
    expCredibilityInit: Share = Share("0.80"),
    expCredibilitySpeed: Coefficient = Coefficient("0.05"),
    expCredibilityThreshold: Rate = Rate("0.02"),
    expWagePassthrough: Coefficient = Coefficient("0.50"),
    expBondSensitivity: Coefficient = Coefficient("0.50"),
):
  require(unionDensity.length == 6, s"unionDensity must have 6 sectors: ${unionDensity.length}")
  require(sbtcEduRoutineness.length == 4, s"sbtcEduRoutineness must have 4 education levels: ${sbtcEduRoutineness.length}")
