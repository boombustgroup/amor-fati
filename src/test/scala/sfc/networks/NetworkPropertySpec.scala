package sfc.networks

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.agents.{Firm, TechState}
import sfc.types.*

import scala.util.Random

class NetworkPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  private val genN = Gen.choose(20, 200)
  private val genK = Gen.oneOf(4, 6, 8)

  // --- Watts-Strogatz properties ---

  "wattsStrogatz" should "produce symmetric adjacency" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      val adj = Network.wattsStrogatz(n, k, 0.10)
      for
        i <- 0 until n
        j <- adj(i)
      do adj(j) should contain(i)
    }
  }

  it should "have no self-loops" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      val adj = Network.wattsStrogatz(n, k, 0.10)
      for i <- 0 until n do adj(i) should not contain i
    }
  }

  it should "have exact degree k when p=0" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      whenever(n > k) {
        val adj = Network.wattsStrogatz(n, k, 0.0)
        for i <- 0 until n do adj(i).toSet.size shouldBe k
      }
    }
  }

  it should "have average degree approximately k" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      whenever(n > k) {
        val adj = Network.wattsStrogatz(n, k, 0.10)
        val avgDeg = adj.map(_.length).sum.toDouble / n
        avgDeg shouldBe (k.toDouble +- (k * 0.3))
      }
    }
  }

  it should "be connected (single component)" in {
    forAll(genN) { (n: Int) =>
      whenever(n >= 20) {
        val adj = Network.wattsStrogatz(n, 6, 0.10)
        val visited = new Array[Boolean](n)
        val queue = scala.collection.mutable.Queue(0)
        visited(0) = true
        while queue.nonEmpty do
          val node = queue.dequeue()
          for nb <- adj(node) if !visited(nb) do
            visited(nb) = true
            queue.enqueue(nb)
        visited.count(identity) shouldBe n
      }
    }
  }

  // --- Erdos-Renyi properties ---

  "erdosRenyi" should "produce symmetric adjacency" in {
    forAll(genN) { (n: Int) =>
      val adj = Network.erdosRenyi(n, 6, new Random(42))
      for
        i <- 0 until n
        j <- adj(i)
      do adj(j) should contain(i)
    }
  }

  it should "have no self-loops" in {
    forAll(genN) { (n: Int) =>
      val adj = Network.erdosRenyi(n, 6, new Random(42))
      for i <- 0 until n do adj(i) should not contain i
    }
  }

  it should "have average degree approximately equal to target" in {
    forAll(genN) { (n: Int) =>
      whenever(n >= 30) {
        val target = 6
        val adj = Network.erdosRenyi(n, target, new Random(42))
        val avgDeg = adj.map(_.length).sum.toDouble / n
        avgDeg shouldBe (target.toDouble +- (target * 0.5))
      }
    }
  }

  // --- Barabasi-Albert properties ---

  "barabasiAlbert" should "produce symmetric adjacency" in {
    forAll(genN) { (n: Int) =>
      whenever(n >= 10) {
        val adj = Network.barabasiAlbert(n, 3, new Random(42))
        for
          i <- 0 until n
          j <- adj(i)
        do adj(j) should contain(i)
      }
    }
  }

  it should "have no self-loops" in {
    forAll(genN) { (n: Int) =>
      whenever(n >= 10) {
        val adj = Network.barabasiAlbert(n, 3, new Random(42))
        for i <- 0 until n do adj(i) should not contain i
      }
    }
  }

  it should "have no isolated nodes" in {
    forAll(genN) { (n: Int) =>
      whenever(n >= 10) {
        val adj = Network.barabasiAlbert(n, 3, new Random(42))
        for i <- 0 until n do adj(i).length should be > 0
      }
    }
  }

  it should "have average degree approximately 2m" in {
    forAll(genN) { (n: Int) =>
      whenever(n >= 20) {
        val m = 3
        val adj = Network.barabasiAlbert(n, m, new Random(42))
        val avgDeg = adj.map(_.length).sum.toDouble / n
        avgDeg shouldBe (2.0 * m +- (m * 1.5))
      }
    }
  }

  // --- Lattice properties ---

  "lattice" should "have exact degree k" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      whenever(n > k) {
        val adj = Network.lattice(n, k)
        for i <- 0 until n do adj(i).toSet.size shouldBe k
      }
    }
  }

  it should "produce symmetric adjacency" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      whenever(n > k) {
        val adj = Network.lattice(n, k)
        for
          i <- 0 until n
          j <- adj(i)
        do adj(j) should contain(i)
      }
    }
  }

  it should "have no self-loops" in {
    forAll(genN, genK) { (n: Int, k: Int) =>
      whenever(n > k) {
        val adj = Network.lattice(n, k)
        for i <- 0 until n do adj(i) should not contain i
      }
    }
  }

  // --- localAutoRatio properties ---

  "localAutoRatio" should "be in [0, 1]" in {
    val firms = (0 until 10).map { i =>
      val tech = if i < 3 then TechState.Automated(1.0) else TechState.Traditional(10)
      Firm.State(
        FirmId(i),
        PLN(100000),
        PLN.Zero,
        tech,
        Ratio(0.5),
        1.0,
        Ratio(0.4),
        SectorIdx(0),
        (0 until 10).filter(_ != i).toArray,
      )
    }.toArray
    for f <- firms do
      val r = Network.localAutoRatio(f, firms)
      r should be >= 0.0
      r should be <= 1.0
  }

  it should "be 0 when no neighbors" in {
    val firm = Firm.State(
      FirmId(0),
      PLN(100000),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.4),
      SectorIdx(0),
      Array.empty[Int],
    )
    val firms = Array(firm)
    Network.localAutoRatio(firm, firms) shouldBe 0.0
  }
