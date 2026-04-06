package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Bounded [0,1] fractions, probabilities, proportions. */
object ShareProvider:
  opaque type Share = Long

  object Share:
    val Zero: Share                           = 0L
    val One: Share                            = Scale
    def apply(d: Double): Share               = Math.round(d * Scale)
    def fraction(num: Int, den: Int): Share   =
      if den == 0 then Zero
      else
        val scaled         = BigInt(num.toLong) * BigInt(Scale)
        val denominator    = BigInt(den.toLong)
        val quotient       = scaled / denominator
        val remainder      = (scaled % denominator).abs
        val denominatorAbs = denominator.abs
        val twiceRemainder = remainder * 2
        if twiceRemainder < denominatorAbs then quotient.toLong
        else
          val resultSign = scaled.signum * denominator.signum
          if twiceRemainder > denominatorAbs then (quotient + resultSign).toLong
          else if quotient % 2 == 0 then quotient.toLong
          else (quotient + resultSign).toLong
    def fromRaw(raw: Long): Share             = raw
    def random(rng: scala.util.Random): Share = Share.fromRaw(rng.nextInt(Scale.toInt).toLong)

  extension (s: Share)
    inline def toLong: Long                = s
    def unary_- : Share                    = -s
    def abs: Share                         = math.abs(s)
    def +(other: Share): Share             = s + other
    def -(other: Share): Share             = s - other
    def *(other: Share): Share             = bankerRound(BigInt(s) * BigInt(other))
    def /(n: Int): Share                   = Share.fromRaw(s / n.toLong)
    def /(other: Share): Double            = if other != 0L then s.toDouble / other.toDouble else 0.0
    def max(other: Share): Share           = math.max(s, other)
    def min(other: Share): Share           = math.min(s, other)
    def clamp(lo: Share, hi: Share): Share = math.max(lo, math.min(hi, s))
    def monthly: Share                     = Share.fromRaw(s / 12L)
    def sqrt: Share                        = Share(math.sqrt(s.toDouble / ScaleD))
    def >(other: Share): Boolean           = s > other
    def <(other: Share): Boolean           = s < other
    def >=(other: Share): Boolean          = s >= other
    def <=(other: Share): Boolean          = s <= other

    /** True if rng.nextDouble() < this share. For probability sampling. */
    def sampleBelow(rng: scala.util.Random): Boolean = rng.nextInt(Scale.toInt) < s

  extension (n: Int) def *(s: Share): Double = n.toDouble * (s.toDouble / ScaleD)

  given Ordering[Share] = Ordering.Long
  given Numeric[Share] with
    def plus(x: Share, y: Share): Share         = x + y
    def minus(x: Share, y: Share): Share        = x - y
    def times(x: Share, y: Share): Share        = x * y
    def negate(x: Share): Share                 = Share.fromRaw(-x)
    def fromInt(x: Int): Share                  = Share(x.toDouble)
    def parseString(str: String): Option[Share] = str.toDoubleOption.map(Share(_))
    def toInt(x: Share): Int                    = (x / Scale).toInt
    def toLong(x: Share): Long                  = x / Scale
    def toFloat(x: Share): Float                = (x.toDouble / ScaleD).toFloat
    def toDouble(x: Share): Double              = x.toDouble / ScaleD
    def compare(x: Share, y: Share): Int        = java.lang.Long.compare(x, y)
