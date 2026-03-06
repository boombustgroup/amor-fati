package sfc.networks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.{Firm, TechState}
import sfc.types.*

import scala.util.Random

class NetworkSpec extends AnyFlatSpec with Matchers:

  // --- Watts-Strogatz ---

  "wattsStrogatz" should "produce average degree ≈ k" in {
    Random.setSeed(42)
    val adj = Network.wattsStrogatz(1000, 6, 0.10)
    val avgDeg = adj.map(_.length).sum.toDouble / adj.length
    avgDeg shouldBe 6.0 +- 0.3 // ±5%
  }

  it should "be symmetric" in {
    Random.setSeed(42)
    val adj = Network.wattsStrogatz(1000, 6, 0.10)
    for i <- adj.indices; j <- adj(i) do
      adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    Random.setSeed(42)
    val adj = Network.wattsStrogatz(1000, 6, 0.10)
    for i <- adj.indices do
      adj(i) should not contain i
  }

  it should "be a single connected component" in {
    Random.setSeed(42)
    val adj = Network.wattsStrogatz(1000, 6, 0.10)
    val visited = Array.fill(adj.length)(false)
    val queue = scala.collection.mutable.Queue(0)
    visited(0) = true
    while queue.nonEmpty do
      val node = queue.dequeue()
      for nb <- adj(node) if !visited(nb) do
        visited(nb) = true
        queue.enqueue(nb)
    visited.count(identity) shouldBe adj.length
  }

  it should "produce exact degree k when p=0.0" in {
    Random.setSeed(42)
    val adj = Network.wattsStrogatz(1000, 6, 0.0)
    for i <- adj.indices do
      adj(i).length shouldBe 6
  }

  // --- Erdos-Renyi ---

  "erdosRenyi" should "produce average degree ≈ target" in {
    val rng = new Random(42)
    val adj = Network.erdosRenyi(1000, 6, rng)
    val avgDeg = adj.map(_.length).sum.toDouble / adj.length
    avgDeg shouldBe 6.0 +- 0.9 // ±15%
  }

  it should "be symmetric" in {
    val rng = new Random(42)
    val adj = Network.erdosRenyi(1000, 6, rng)
    for i <- adj.indices; j <- adj(i) do
      adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val rng = new Random(42)
    val adj = Network.erdosRenyi(1000, 6, rng)
    for i <- adj.indices do
      adj(i) should not contain i
  }

  // --- Barabasi-Albert ---

  "barabasiAlbert" should "produce average degree ≈ 2m" in {
    val rng = new Random(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    val avgDeg = adj.map(_.length).sum.toDouble / adj.length
    avgDeg shouldBe 6.0 +- 0.6 // ±10%
  }

  it should "be symmetric" in {
    val rng = new Random(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    for i <- adj.indices; j <- adj(i) do
      adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val rng = new Random(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    for i <- adj.indices do
      adj(i) should not contain i
  }

  it should "have no isolated nodes" in {
    val rng = new Random(42)
    val adj = Network.barabasiAlbert(1000, 3, rng)
    for i <- adj.indices do
      adj(i).length should be > 0
  }

  // --- Ring Lattice ---

  "lattice" should "produce exact degree k for all nodes" in {
    val adj = Network.lattice(1000, 6)
    for i <- adj.indices do
      adj(i).length shouldBe 6
  }

  it should "be symmetric" in {
    val adj = Network.lattice(1000, 6)
    for i <- adj.indices; j <- adj(i) do
      adj(j) should contain(i)
  }

  it should "have no self-loops" in {
    val adj = Network.lattice(1000, 6)
    for i <- adj.indices do
      adj(i) should not contain i
  }

  // --- localAutoRatio ---

  "localAutoRatio" should "return 0.0 when no automated neighbors" in {
    val firms = Array(
      mkFirm(0, TechState.Traditional(10), Array(1, 2)),
      mkFirm(1, TechState.Traditional(10), Array(0)),
      mkFirm(2, TechState.Traditional(10), Array(0))
    )
    Network.localAutoRatio(firms(0), firms) shouldBe 0.0
  }

  it should "return 1.0 when all neighbors are Automated" in {
    val firms = Array(
      mkFirm(0, TechState.Traditional(10), Array(1, 2)),
      mkFirm(1, TechState.Automated(1.2), Array(0)),
      mkFirm(2, TechState.Automated(1.1), Array(0))
    )
    Network.localAutoRatio(firms(0), firms) shouldBe 1.0
  }

  it should "count Hybrid as automated in ratio" in {
    val firms = Array(
      mkFirm(0, TechState.Traditional(10), Array(1, 2, 3)),
      mkFirm(1, TechState.Automated(1.2), Array(0)),
      mkFirm(2, TechState.Hybrid(5, 1.0), Array(0)),
      mkFirm(3, TechState.Traditional(10), Array(0))
    )
    Network.localAutoRatio(firms(0), firms) shouldBe (2.0 / 3.0 +- 0.001)
  }

  it should "return 0.0 for firm with no neighbors" in {
    val firms = Array(mkFirm(0, TechState.Traditional(10), Array.empty[Int]))
    Network.localAutoRatio(firms(0), firms) shouldBe 0.0
  }

  private def mkFirm(id: Int, tech: TechState, neighbors: Array[Int]): Firm.State =
    Firm.State(FirmId(id), PLN(50000.0), PLN.Zero, tech, Ratio(0.5), 1.0, Ratio(0.5), SectorIdx(0), neighbors)
