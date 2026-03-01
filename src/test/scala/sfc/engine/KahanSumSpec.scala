package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import KahanSum.*

class KahanSumSpec extends AnyFlatSpec with Matchers:

  "KahanSum.sum" should "return 0.0 for empty collection" in {
    KahanSum.sum(Seq.empty[Double]) shouldBe 0.0
  }

  it should "return exact value for single element" in {
    KahanSum.sum(Seq(42.0)) shouldBe 42.0
  }

  it should "return exact sum for small array" in {
    KahanSum.sum(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) shouldBe 15.0
  }

  it should "preserve precision with 1.0 + many small values" in {
    // Classic Kahan test: 1.0 + 10000 × 1e-16
    // Naive sum loses the small values; Kahan preserves them
    val n = 10000
    val values = 1.0 +: Array.fill(n)(1e-16)
    val kahanResult = KahanSum.sum(values)
    val naiveResult = values.sum
    val exact = 1.0 + n * 1e-16

    // Kahan should be much closer to exact than naive
    val kahanError = Math.abs(kahanResult - exact)
    val naiveError = Math.abs(naiveResult - exact)
    kahanError should be <= naiveError
    kahanResult shouldBe exact +- 1e-15
  }

  it should "produce result closer to BigDecimal truth than naive sum on large array" in {
    val rng = new scala.util.Random(42)
    val n = 10000
    val values = Array.fill(n)(rng.nextDouble() * 1e8 - 5e7)

    val exactBD = values.map(BigDecimal(_)).sum.toDouble
    val kahanResult = KahanSum.sum(values)
    val naiveResult = values.sum

    val kahanError = Math.abs(kahanResult - exactBD)
    val naiveError = Math.abs(naiveResult - exactBD)
    kahanError should be <= naiveError
  }

  "KahanSum.sumBy" should "be equivalent to kahanSum with identity" in {
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    KahanSum.sumBy(values)(identity) shouldBe KahanSum.sum(values)
  }

  it should "apply mapping function correctly" in {
    val values = Array(1, 2, 3, 4, 5)
    KahanSum.sumBy(values)(_.toDouble * 2) shouldBe 30.0
  }

  "Iterable[Double].kahanSum extension" should "work on List" in {
    List(1.0, 2.0, 3.0).kahanSum shouldBe 6.0
  }

  it should "work on Vector" in {
    Vector(10.0, 20.0, 30.0).kahanSum shouldBe 60.0
  }

  "Iterable[A].kahanSumBy extension" should "work on Vector of case classes" in {
    case class Item(value: Double)
    Vector(Item(1.0), Item(2.0), Item(3.0)).kahanSumBy(_.value) shouldBe 6.0
  }

  "Array[Double].kahanSum extension" should "work on Array" in {
    Array(1.0, 2.0, 3.0).kahanSum shouldBe 6.0
  }

  "Array[A].kahanSumBy extension" should "work on Array of tuples" in {
    Array((1, 10.0), (2, 20.0), (3, 30.0)).kahanSumBy(_._2) shouldBe 60.0
  }

  it should "handle negative values" in {
    Array(-1.0, -2.0, -3.0).kahanSum shouldBe -6.0
  }

  it should "handle mixed positive and negative values" in {
    Array(1e15, 1.0, -1e15).kahanSum shouldBe 1.0
  }
