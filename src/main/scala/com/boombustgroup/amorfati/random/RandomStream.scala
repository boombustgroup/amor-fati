package com.boombustgroup.amorfati.random

/** Project-wide RNG facade.
  *
  * Domain code depends on this opaque type instead of `scala.util.Random`. The
  * underlying engine stays an implementation detail of the `random` package.
  */
opaque type RandomStream = scala.util.Random

object RandomStream:

  def seeded(seed: Long): RandomStream =
    new scala.util.Random(seed)

  def fresh(): RandomStream =
    new scala.util.Random()

  /** Migration/testing adapter for the rare cases that need a custom
    * `scala.util.Random` implementation.
    */
  def wrap(random: scala.util.Random): RandomStream =
    random

  extension (stream: RandomStream)
    def nextInt(boundExclusive: Int): Int =
      stream.nextInt(boundExclusive)

    def nextLong(): Long =
      stream.nextLong()

    def nextLong(boundExclusive: Long): Long =
      stream.nextLong(boundExclusive)

    def nextDouble(): Double =
      stream.nextDouble()

    def nextGaussian(): Double =
      stream.nextGaussian()

    def between(minInclusive: Int, maxExclusive: Int): Int =
      stream.between(minInclusive, maxExclusive)

    def between(minInclusive: Long, maxExclusive: Long): Long =
      stream.between(minInclusive, maxExclusive)

    def between(minInclusive: Double, maxExclusive: Double): Double =
      stream.between(minInclusive, maxExclusive)

    def shuffle[A](values: Seq[A]): Vector[A] =
      stream.shuffle(values).toVector
