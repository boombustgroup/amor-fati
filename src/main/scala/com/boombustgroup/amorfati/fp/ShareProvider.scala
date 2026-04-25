package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import com.boombustgroup.amorfati.random.RandomStream
import ScalarProvider.*
import scala.annotation.targetName

/** Bounded [0,1] fractions, probabilities, proportions. */
object ShareProvider:
  opaque type Share = Long

  object Share:
    val Zero: Share                         = 0L
    val One: Share                          = Scale
    def apply(value: BigDecimal): Share     = fromDecimal(value)
    def apply(value: String): Share         = parseDecimal(value)
    def apply(value: Int): Share            = fromRaw(value.toLong * Scale)
    def apply(value: Long): Share           = fromRaw(value * Scale)
    @targetName("fromScalar")
    def apply(value: Scalar): Share         = fromRaw(value.toLong)
    def fraction(num: Int, den: Int): Share =
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
    def fromRaw(raw: Long): Share           = raw
    def random(rng: RandomStream): Share    = Share.fromRaw(rng.nextInt(Scale.toInt).toLong)

  extension (s: Share)
    inline def toLong: Long                    = s
    def unary_- : Share                        = -s
    def abs: Share                             = math.abs(s)
    def +(other: Share): Share                 = s + other
    def -(other: Share): Share                 = s - other
    def *(other: Share): Share                 = bankerRound(BigInt(s) * BigInt(other))
    def /(n: Int): Share                       = Share.fromRaw(divideRaw(s, n.toLong))
    def /(other: Share): ScalarProvider.Scalar = ScalarProvider.Scalar.fromRaw(ratioRaw(s, other))
    def max(other: Share): Share               = math.max(s, other)
    def min(other: Share): Share               = math.min(s, other)
    def clamp(lo: Share, hi: Share): Share     = math.max(lo, math.min(hi, s))
    def monthly: Share                         = Share.fromRaw(divideRaw(s, 12L))
    def sqrt: Share                            = Share.fromRaw(sqrtRaw(s))
    def format(fractionalDigits: Int): String  = FixedPointBase.format(s, fractionalDigits)
    def compact: String                        = FixedPointBase.formatCompact(s)
    def >(other: Share): Boolean               = s > other
    def <(other: Share): Boolean               = s < other
    def >=(other: Share): Boolean              = s >= other
    def <=(other: Share): Boolean              = s <= other

    /** True if a fixed-point unit draw is below this share. */
    def sampleBelow(rng: RandomStream): Boolean = rng.nextInt(Scale.toInt) < s

  given Ordering[Share] = Ordering.Long
