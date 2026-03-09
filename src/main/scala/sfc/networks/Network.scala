package sfc.networks

import scala.util.Random

/** Graph generation algorithms for agent interaction networks.
  *
  * Used in two contexts:
  *   - '''Firm supply-chain network''' (FirmInit) — topology governs technology
  *     diffusion and input-output linkages. Topology selected by env var
  *     `NETWORK_TOPOLOGY` (ws/er/ba/lattice), parameterized by `NETWORK_K`
  *     (degree) and `NETWORK_REWIRE_P` (WS only).
  *   - '''Household social network''' (Household.initVector) — Watts-Strogatz
  *     small-world graph governing consumption peer effects and social
  *     learning. Parameterized by `HH_SOCIAL_K` and `HH_SOCIAL_P`.
  *
  * All generators return undirected graphs as adjacency lists:
  * `Array[Array[Int]]` where `result(i)` contains the neighbor indices of node
  * `i`. Adjacency is always symmetric (i∈adj(j) ⟺ j∈adj(i)) and self-loop free.
  *
  * References:
  *   - Watts & Strogatz (1998), "Collective dynamics of 'small-world'
  *     networks", Nature 393
  *   - Erdős & Rényi (1959), "On random graphs", Publicationes Mathematicae 6
  *   - Barabási & Albert (1999), "Emergence of scaling in random networks",
  *     Science 286
  */
object Network:

  /** Generate a Watts-Strogatz small-world graph.
    *
    * Starts from a ring lattice (each node connected to k/2 nearest neighbors
    * on each side), then rewires each edge with probability p. At p=0 the graph
    * is a regular lattice with high clustering; at p=1 it approaches a random
    * graph with short path lengths. The "small-world" regime (high clustering +
    * short paths) emerges for intermediate p ≈ 0.01–0.1.
    *
    * @param n
    *   number of nodes (must be > k)
    * @param k
    *   target degree (even integer; each node gets k/2 neighbors per side)
    * @param p
    *   rewiring probability ∈ [0, 1]
    * @return
    *   adjacency lists (symmetric, no self-loops)
    */
  def wattsStrogatz(n: Int, k: Int, p: Double): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])

    // Ring lattice: connect each node to k/2 nearest neighbors on each side
    val halfK = k / 2
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val right = (i + j)     % n
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
        var attempts  = 0
        while (newTarget == i || adj(i).contains(newTarget)) && attempts < 20 do
          newTarget = Random.nextInt(n)
          attempts += 1
        if newTarget != i && !adj(i).contains(newTarget) then
          adj(i) -= target
          adj(target) -= i
          adj(i) += newTarget
          adj(newTarget) += i

    adj.map(_.toArray)

  /** Generate an Erdős–Rényi G(n,p) random graph with target average degree.
    *
    * Each possible edge (i,j) is included independently with probability p =
    * avgDegree/(n−1). The resulting degree distribution is approximately
    * Poisson. Clustering coefficient is low (≈ p) and average path length is
    * O(log n / log avgDegree).
    *
    * @param n
    *   number of nodes
    * @param avgDegree
    *   target average degree (actual degree is stochastic)
    * @param rng
    *   random number generator (deterministic with fixed seed)
    * @return
    *   adjacency lists (symmetric, no self-loops)
    */
  def erdosRenyi(n: Int, avgDegree: Int, rng: Random): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])
    val p   = avgDegree.toDouble / (n - 1)
    for
      i <- 0 until n
      j <- (i + 1) until n
    do
      if rng.nextDouble() < p then
        adj(i) += j
        adj(j) += i
    adj.map(_.toArray)

  /** Generate a Barabási–Albert preferential attachment (scale-free) graph.
    *
    * Starts with a complete seed graph on m+1 nodes, then adds nodes one at a
    * time, each connecting to m existing nodes with probability proportional to
    * their current degree. Produces a power-law degree distribution P(k) ∝
    * k^{−3} — a few hub nodes accumulate many connections while most nodes have
    * low degree. Models "rich get richer" dynamics in real-world networks.
    *
    * @param n
    *   total number of nodes (including seed)
    * @param m
    *   number of edges added per new node (average degree ≈ 2m for large n)
    * @param rng
    *   random number generator (deterministic with fixed seed)
    * @return
    *   adjacency lists (symmetric, no self-loops)
    */
  def barabasiAlbert(n: Int, m: Int, rng: Random): Array[Array[Int]] =
    val adj         = Array.fill(n)(scala.collection.mutable.Set.empty[Int])
    val seedSize    = m + 1
    // Seed: complete graph on first m+1 nodes
    for
      i <- 0 until seedSize
      j <- (i + 1) until seedSize
    do
      adj(i) += j
      adj(j) += i
    // Degree array for preferential attachment (sum = 2 * edges)
    val degree      = new Array[Int](n)
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
        if j < i && !targets.contains(j) then targets += j
      for t <- targets do
        adj(i) += t
        adj(t) += i
        degree(i) += 1
        degree(t) += 1
        totalDegree += 2
    adj.map(_.toArray)

  /** Generate a regular ring lattice (deterministic, no rewiring).
    *
    * Each node is connected to its k/2 nearest neighbors on each side along a
    * circular ring. This is the starting point of the Watts-Strogatz algorithm
    * (p=0). High clustering coefficient (≈ 3(k−2) / 4(k−1)) but long average
    * path length (≈ n / 2k). Useful as a baseline topology or for models
    * requiring strict spatial regularity.
    *
    * @param n
    *   number of nodes
    * @param k
    *   degree (even integer; each node gets exactly k neighbors)
    * @return
    *   adjacency lists (symmetric, no self-loops, every node has exactly degree
    *   k)
    */
  def lattice(n: Int, k: Int): Array[Array[Int]] =
    val adj   = Array.fill(n)(scala.collection.mutable.Set.empty[Int])
    val halfK = k / 2
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val right = (i + j)     % n
      val left  = (i - j + n) % n
      adj(i) += right
      adj(right) += i
      adj(i) += left
      adj(left) += i
    adj.map(_.toArray)
