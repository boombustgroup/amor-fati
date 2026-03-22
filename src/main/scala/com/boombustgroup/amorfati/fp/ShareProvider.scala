package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Bounded [0,1] fractions, probabilities, proportions. */
object ShareProvider:
  opaque type Share = Long

  object Share:
    val Zero: Share                         = 0L
    val One: Share                          = Scale
    def apply(d: Double): Share             = Math.round(d * Scale)
    def fraction(num: Int, den: Int): Share = Math.round(num.toDouble / den.toDouble * Scale)

  extension (s: Share)
    inline def toLong: Long                    = s
    def unary_- : Share                        = -s
    def abs: Share                             = math.abs(s)
    def +(other: Share): Share                  = s + other
    def -(other: Share): Share                  = s - other
    def *(other: Share): Share                  = bankerRound(BigInt(s) * BigInt(other))
    def /(n: Int): Share                       = Share(asDouble(s) / n)
    def /(other: Share): Double                = if other != 0L then s.toDouble / other.toDouble else 0.0
    def max(other: Share): Share                = math.max(s, other)
    def min(other: Share): Share                = math.min(s, other)
    def clamp(lo: Share, hi: Share): Share      = math.max(lo, math.min(hi, s))
    def monthly: Share                         = Share(asDouble(s) / 12.0)
    def sqrt: Share                            = Share(math.sqrt(asDouble(s)))
    def >(other: Share): Boolean                = s > other
    def <(other: Share): Boolean                = s < other
    def >=(other: Share): Boolean               = s >= other
    def <=(other: Share): Boolean               = s <= other
    /** True if rng.nextDouble() < this share. For probability sampling. */
    def sampleBelow(rng: scala.util.Random): Boolean = rng.nextDouble() < asDouble(s)

  extension (n: Int)
    def *(s: Share): Double = n.toDouble * asDouble(s)

  given Ordering[Share] = Ordering.Long
  given Numeric[Share] with
    def plus(x: Share, y: Share): Share         = x + y
    def minus(x: Share, y: Share): Share        = x - y
    def times(x: Share, y: Share): Share        = x * y
    def negate(x: Share): Share                 = Share(-asDouble(x))
    def fromInt(x: Int): Share                  = Share(x.toDouble)
    def parseString(str: String): Option[Share] = str.toDoubleOption.map(Share(_))
    def toInt(x: Share): Int                    = (x / Scale).toInt
    def toLong(x: Share): Long                  = x / Scale
    def toFloat(x: Share): Float                = asDouble(x).toFloat
    def toDouble(x: Share): Double              = asDouble(x)
    def compare(x: Share, y: Share): Int        = java.lang.Long.compare(x, y)
