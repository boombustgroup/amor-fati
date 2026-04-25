package com.boombustgroup.amorfati.networks

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*

class NetworkSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  // --- Watts-Strogatz ---

  "wattsStrogatz" should "produce average degree ≈ k" in {
    val adj    = Network.wattsStrogatz(1000, 6, Share.decimal(10, 2), RandomStream.seeded(42))
    val avgDeg = decimal(adj.map(_.length).sum) / decimal(adj.length)
    avgDeg shouldBe BigDecimal("6.0") +- BigDecimal("0.3") // ±5%
  }

  it should "be symmetric" in {
    val adj = Network.wattsStrogatz(1000, 6, Share.decimal(10, 2), RandomStream.seeded(42))
    for i <- adj.indices; j <- adj(i) do adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val adj = Network.wattsStrogatz(1000, 6, Share.decimal(10, 2), RandomStream.seeded(42))
    for i <- adj.indices do adj(i) should not contain i
  }

  it should "be a single connected component" in {
    val adj     = Network.wattsStrogatz(1000, 6, Share.decimal(10, 2), RandomStream.seeded(42))
    val visited = Array.fill(adj.length)(false)
    val queue   = scala.collection.mutable.Queue(0)
    visited(0) = true
    while queue.nonEmpty do
      val node = queue.dequeue()
      for nb <- adj(node) if !visited(nb) do
        visited(nb) = true
        queue.enqueue(nb)
    visited.count(identity) shouldBe adj.length
  }

  it should "produce exact degree k when p=0.0" in {
    val adj = Network.wattsStrogatz(1000, 6, Share.Zero, RandomStream.seeded(42))
    for i <- adj.indices do adj(i).length shouldBe 6
  }

  // --- Erdos-Renyi ---

  "erdosRenyi" should "produce average degree ≈ target" in {
    val rng    = RandomStream.seeded(42)
    val adj    = Network.erdosRenyi(1000, 6, rng)
    val avgDeg = decimal(adj.map(_.length).sum) / decimal(adj.length)
    avgDeg shouldBe BigDecimal("6.0") +- BigDecimal("0.9") // ±15%
  }

  it should "be symmetric" in {
    val rng = RandomStream.seeded(42)
    val adj = Network.erdosRenyi(1000, 6, rng)
    for i <- adj.indices; j <- adj(i) do adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val rng = RandomStream.seeded(42)
    val adj = Network.erdosRenyi(1000, 6, rng)
    for i <- adj.indices do adj(i) should not contain i
  }

  // --- Barabasi-Albert ---

  "barabasiAlbert" should "produce average degree ≈ 2m" in {
    val rng    = RandomStream.seeded(42)
    val adj    = Network.barabasiAlbert(1000, 3, rng)
    val avgDeg = decimal(adj.map(_.length).sum) / decimal(adj.length)
    avgDeg shouldBe BigDecimal("6.0") +- BigDecimal("0.6") // ±10%
  }

  it should "be symmetric" in {
    val rng = RandomStream.seeded(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    for i <- adj.indices; j <- adj(i) do adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val rng = RandomStream.seeded(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    for i <- adj.indices do adj(i) should not contain i
  }

  it should "have no isolated nodes" in {
    val rng = RandomStream.seeded(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    for i <- adj.indices do adj(i).length should be > 0
  }

  // --- Ring Lattice ---

  "lattice" should "produce exact degree k for all nodes" in {
    val adj = Network.lattice(1000, 6)
    for i <- adj.indices do adj(i).length shouldBe 6
  }

  it should "be symmetric" in {
    val adj = Network.lattice(1000, 6)
    for i <- adj.indices; j <- adj(i) do adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val adj = Network.lattice(1000, 6)
    for i <- adj.indices do adj(i) should not contain i
  }
