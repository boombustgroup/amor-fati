package sfc.dynamics

import sfc.agents.{Firm, FirmOps, TechState}
import sfc.config.{Config, SECTORS, FirmSizeDistribution}

import scala.util.Random

object DynamicNetwork:
  /** Replace bankrupt firms with new Traditional entrants, wired via preferential attachment.
    *
    * Each bankrupt firm has probability rho of being replaced each step.
    * New entrants inherit the same sector, start with fresh state, and connect
    * to k alive firms weighted by degree (preferential attachment).
    *
    * When rho=0.0, returns firms unchanged (static mode = no-op).
    *
    * @param firms current firm array (some may be Bankrupt)
    * @param rho   replacement probability per bankrupt firm per step (0.0 = static)
    * @return updated firm array with same length */
  def rewire(firms: Array[Firm], rho: Double): Array[Firm] =
    if rho == 0.0 then return firms

    val n = firms.length
    val k = Config.NetworkK

    // Identify bankrupt firms to replace (each with probability rho)
    val toReplace = (0 until n).filter(i =>
      !FirmOps.isAlive(firms(i)) && Random.nextDouble() < rho
    ).toSet
    if toReplace.isEmpty then return firms

    // Build mutable adjacency from current neighbor arrays
    val adj = Array.tabulate(n)(i =>
      scala.collection.mutable.Set.from(firms(i).neighbors))

    // Alive firm indices (for preferential attachment targets)
    val alive = (0 until n).filter(i => FirmOps.isAlive(firms(i))).toArray

    for idx <- toReplace do
      // Remove old edges
      for nb <- adj(idx) do adj(nb) -= idx
      adj(idx).clear()

      if alive.nonEmpty then
        val numTargets = Math.min(k, alive.length)
        val targets = scala.collection.mutable.Set.empty[Int]
        val degrees = alive.map(i => Math.max(1, adj(i).size))
        val totalDeg = degrees.sum

        var attempts = 0
        while targets.size < numTargets && attempts < numTargets * 20 do
          var r = Random.nextInt(if totalDeg > 0 then totalDeg else 1)
          var j = 0
          while j < alive.length - 1 && r >= degrees(j) do
            r -= degrees(j)
            j += 1
          targets += alive(j)
          attempts += 1

        for t <- targets do
          adj(idx) += t
          adj(t) += idx

    // Rebuild firms with updated state
    (0 until n).map { i =>
      if toReplace.contains(i) then
        val sec = firms(i).sector
        val newSize = FirmSizeDistribution.draw(Random)
        val sizeMult = newSize.toDouble / Config.WorkersPerFirm
        Firm(
          id = i,
          cash = (Random.between(10000.0, 80000.0)) * sizeMult,
          debt = 0.0,
          tech = TechState.Traditional(newSize),
          riskProfile = Random.between(0.1, 0.9),
          innovationCostFactor = Random.between(0.8, 1.5),
          digitalReadiness = Math.max(0.02, Math.min(0.98,
            SECTORS(sec).baseDigitalReadiness + (Random.nextGaussian() * 0.20))),
          sector = sec,
          neighbors = adj(i).toArray,
          initialSize = newSize
        )
      else
        val newNb = adj(i).toArray
        if newNb.length != firms(i).neighbors.length then
          firms(i).copy(neighbors = newNb)
        else
          firms(i)
    }.toArray
