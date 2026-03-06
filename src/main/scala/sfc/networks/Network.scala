package sfc.networks

import sfc.agents.{Firm, TechState}

import scala.util.Random

object Network:
  /** Generate Watts-Strogatz small-world adjacency for N nodes */
  def wattsStrogatz(n: Int, k: Int, p: Double): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])

    // Ring lattice: connect each node to k/2 nearest neighbors on each side
    val halfK = k / 2
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val right = (i + j) % n
      val left  = (i - j + n) % n
      adj(i) += right
      adj(right) += i
      adj(i) += left
      adj(left) += i

    // Rewire with probability p
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val target = (i + j) % n
      if Random.nextDouble() < p && adj(i).size < n - 1 then
        // Find a random node not already connected
        var newTarget = Random.nextInt(n)
        var attempts = 0
        while (newTarget == i || adj(i).contains(newTarget)) && attempts < 20 do
          newTarget = Random.nextInt(n)
          attempts += 1
        if newTarget != i && !adj(i).contains(newTarget) then
          adj(i) -= target
          adj(target) -= i
          adj(i) += newTarget
          adj(newTarget) += i

    adj.map(_.toArray)

  /** Generate Erdos-Renyi random graph with target average degree */
  def erdosRenyi(n: Int, avgDegree: Int, rng: Random): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])
    val p = avgDegree.toDouble / (n - 1)
    for
      i <- 0 until n
      j <- (i + 1) until n
    do
      if rng.nextDouble() < p then
        adj(i) += j
        adj(j) += i
    adj.map(_.toArray)

  /** Generate Barabasi-Albert preferential attachment graph.
    * Starts with K_{m+1} complete seed, attaches each new node to m existing nodes. */
  def barabasiAlbert(n: Int, m: Int, rng: Random): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])
    val seedSize = m + 1
    // Seed: complete graph on first m+1 nodes
    for
      i <- 0 until seedSize
      j <- (i + 1) until seedSize
    do
      adj(i) += j
      adj(j) += i
    // Degree array for preferential attachment (sum = 2 * edges)
    val degree = new Array[Int](n)
    for i <- 0 until seedSize do degree(i) = seedSize - 1
    var totalDegree = seedSize * (seedSize - 1) // 2 * edges in seed
    // Attach remaining nodes
    for i <- seedSize until n do
      val targets = scala.collection.mutable.Set.empty[Int]
      while targets.size < m do
        // Weighted random selection by degree
        var r = rng.nextInt(totalDegree)
        var j = 0
        while r >= degree(j) do
          r -= degree(j)
          j += 1
        if j < i && !targets.contains(j) then
          targets += j
      for t <- targets do
        adj(i) += t
        adj(t) += i
        degree(i) += 1
        degree(t) += 1
        totalDegree += 2
    adj.map(_.toArray)

  /** Generate ring lattice: k/2 neighbors on each side, no rewiring (WS step 1 only) */
  def lattice(n: Int, k: Int): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])
    val halfK = k / 2
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val right = (i + j) % n
      val left  = (i - j + n) % n
      adj(i) += right
      adj(right) += i
      adj(i) += left
      adj(left) += i
    adj.map(_.toArray)

  /** Compute local automation ratio among neighbors */
  def localAutoRatio(firm: Firm.State, firms: Array[Firm.State]): Double =
    val neighbors = firm.neighbors
    if neighbors.isEmpty then return 0.0
    val autoCount = neighbors.count { nid =>
      val nf = firms(nid)
      nf.tech.isInstanceOf[TechState.Automated] || nf.tech.isInstanceOf[TechState.Hybrid]
    }
    autoCount.toDouble / neighbors.length
