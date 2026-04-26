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
    frictionDurationMult: Multiplier = Multiplier(1),
    frictionCostMult: Share = Share.decimal(5, 1),
    voluntarySearchProb: Share = Share.decimal(2, 2),
    voluntaryWageThreshold: Share = Share.decimal(20, 2),
    vacancyWeight: Coefficient = Coefficient(2),
    adjacentFrictionMax: Share = Share.decimal(4, 1),
    // Unions (GUS 2024)
    unionDensity: Vector[Share] =
      Vector(Share.decimal(2, 2), Share.decimal(15, 2), Share.decimal(3, 2), Share.decimal(12, 2), Share.decimal(30, 2), Share.decimal(4, 2)),
    unionWagePremium: Share = Share.decimal(8, 2),
    unionRigidity: Share = Share.decimal(50, 2),
    // Skills-biased technological change (Acemoglu & Restrepo 2020, Autor 2024)
    sbtcEduRoutineness: Vector[Share] =
      Vector(Share.decimal(80, 2), Share.decimal(65, 2), Share.decimal(45, 2), Share.decimal(25, 2)), // Primary, Vocational, Secondary, Tertiary
    sbtcComplementPremium: Share = Share.decimal(15, 2),                                              // max wage premium for AI-complemented cognitive workers
    // Expectations (Carroll 2003, Bewley 1999)
    expLambda: Coefficient = Coefficient.decimal(70, 2),
    expCredibilityInit: Share = Share.decimal(80, 2),
    expCredibilitySpeed: Coefficient = Coefficient.decimal(5, 2),
    expCredibilityThreshold: Rate = Rate.decimal(2, 2),
    expWagePassthrough: Coefficient = Coefficient.decimal(50, 2),
    expBondSensitivity: Coefficient = Coefficient.decimal(50, 2),
):
  require(unionDensity.length == 6, s"unionDensity must have 6 sectors: ${unionDensity.length}")
  require(sbtcEduRoutineness.length == 4, s"sbtcEduRoutineness must have 4 education levels: ${sbtcEduRoutineness.length}")
