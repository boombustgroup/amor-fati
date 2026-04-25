package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import com.boombustgroup.amorfati.random.RandomStream

/** Dimensionless scalar used for algebraic composition and ratios. */
object ScalarProvider:
  opaque type Scalar = Long

  object Scalar:
    val Zero: Scalar                                                     = 0L
    val One: Scalar                                                      = Scale
    def apply(value: BigDecimal): Scalar                                 = fromDecimal(value)
    def apply(value: String): Scalar                                     = parseDecimal(value)
    def apply(value: Int): Scalar                                        = fromRaw(value.toLong * Scale)
    def apply(value: Long): Scalar                                       = fromRaw(value * Scale)
    def fromRaw(raw: Long): Scalar                                       = raw
    def fraction(num: Int, den: Int): Scalar                             =
      if den == 0 then Zero else fromRaw(ratioRaw(num.toLong, den.toLong))
    def randomBetween(lo: Scalar, hi: Scalar, rng: RandomStream): Scalar =
      if hi <= lo then lo
      else Scalar.fromRaw(rng.between(lo.toLong, hi.toLong))

  extension (s: Scalar)
    inline def toLong: Long                   = s
    def unary_- : Scalar                      = -s
    def abs: Scalar                           = math.abs(s)
    def +(other: Scalar): Scalar              = s + other
    def -(other: Scalar): Scalar              = s - other
    def *(other: Scalar): Scalar              = multiplyRaw(s, other)
    def /(n: Int): Scalar                     = Scalar.fromRaw(divideRaw(s, n.toLong))
    def ratioTo(other: Scalar): Scalar        =
      if other == 0L then Scalar.Zero else Scalar.fromRaw(ratioRaw(s, other))
    def reciprocal: Scalar                    =
      if s == 0L then Scalar.Zero else Scalar.fromRaw(ratioRaw(Scale, s))
    def pow(exponent: Int): Scalar            = Scalar.fromRaw(powIntRaw(s, exponent))
    def pow(exponent: Scalar): Scalar         = Scalar.fromRaw(FixedPointMath.powRaw(s, exponent.toLong))
    def log10: Scalar                         =
      if s <= 0L then Scalar.Zero
      else Scalar.fromRaw(ratioRaw(FixedPointMath.lnRaw(s), FixedPointMath.lnRaw(10L * Scale)))
    def max(other: Scalar): Scalar            = math.max(s, other)
    def min(other: Scalar): Scalar            = math.min(s, other)
    def clamp(lo: Scalar, hi: Scalar): Scalar = math.max(lo, math.min(hi, s))
    def format(fractionalDigits: Int): String = FixedPointBase.format(s, fractionalDigits)
    def compact: String                       = FixedPointBase.formatCompact(s)
    def >(other: Scalar): Boolean             = s > other
    def <(other: Scalar): Boolean             = s < other
    def >=(other: Scalar): Boolean            = s >= other
    def <=(other: Scalar): Boolean            = s <= other

  given Ordering[Scalar] = Ordering.Long
