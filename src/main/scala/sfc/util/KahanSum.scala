package sfc.util

object KahanSum:
  /** Kahan compensated summation — O(n) time, O(1) space, error bound O(ε) instead of O(nε). */
  def sum(values: IterableOnce[Double]): Double = sumBy(values)(identity)

  /** Kahan sum with mapping function — avoids intermediate allocation. */
  def sumBy[A](values: IterableOnce[A])(f: A => Double): Double =
    var s = 0.0
    var c = 0.0
    val it = values.iterator
    while it.hasNext do
      val y = f(it.next()) - c
      val t = s + y
      c = (t - s) - y
      s = t
    s

  extension (coll: Iterable[Double]) def kahanSum: Double = sum(coll)

  extension [A](coll: Iterable[A]) def kahanSumBy(f: A => Double): Double = sumBy(coll)(f)

  extension (arr: Array[Double]) def kahanSum: Double = sum(arr)

  extension [A](arr: Array[A]) def kahanSumBy(f: A => Double): Double = sumBy(arr)(f)
