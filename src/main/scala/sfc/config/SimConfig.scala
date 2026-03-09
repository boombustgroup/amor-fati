package sfc.config

import sfc.types.*

/** Network topology selection for comparative experiments. */
enum Topology:
  case Ws, Er, Ba, Lattice

/** Runtime configuration: values that depend on CLI arguments. Passed through
  * runSingle and Simulation.step.
  */
case class RunConfig(
    nSeeds: Int,
    outputPrefix: String,
)

/** 4-to-6 sector definition with heterogeneous sigma (CES elasticity of
  * substitution). sigma affects: decision threshold, automation efficiency,
  * CAPEX costs.
  */
case class SectorDef(
    name: String,
    share: Ratio,                // Share of firm population (GUS BAEL 2024)
    sigma: Double,               // CES elasticity of substitution
    wageMultiplier: Double,      // Sector wage multiplier vs national average
    revenueMultiplier: Double,
    aiCapexMultiplier: Double,
    hybridCapexMultiplier: Double,
    baseDigitalReadiness: Ratio, // Central tendency of digitalReadiness
    hybridRetainFrac: Ratio,     // Fraction of workers RETAINED in hybrid mode (0.5 = halve)
)

/** SectorDefs — default 6-sector structure from SimParams. */
val SectorDefs: Vector[SectorDef] = SimParams.DefaultSectorDefs

/** Network topology — reads TOPOLOGY env var for backward compatibility. */
val TOPOLOGY: Topology = sys.env.getOrElse("TOPOLOGY", "ws").toLowerCase match
  case "er"      => Topology.Er
  case "ba"      => Topology.Ba
  case "lattice" => Topology.Lattice
  case _         => Topology.Ws

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise
  * data.
  */
object FirmSizeDistribution:
  import scala.util.Random

  def draw(rng: Random)(using p: SimParams): Int = p.pop.firmSizeDist match
    case FirmSizeDist.Gus     =>
      val r = rng.nextDouble()
      if r < p.pop.firmSizeMicroShare.toDouble then rng.between(1, 10)
      else if r < p.pop.firmSizeMicroShare.toDouble + p.pop.firmSizeSmallShare.toDouble then rng.between(10, 50)
      else if r < 1.0 - p.pop.firmSizeLargeShare.toDouble then rng.between(50, 250)
      else rng.between(250, p.pop.firmSizeLargeMax + 1)
    case FirmSizeDist.Uniform => p.pop.workersPerFirm
