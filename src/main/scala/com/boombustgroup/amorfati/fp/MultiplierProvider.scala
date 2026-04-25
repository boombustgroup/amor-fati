package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import ScalarProvider.*
import scala.annotation.targetName

/** [0,∞) multiplicative factors (markup, wage mult, labor eff). */
object MultiplierProvider:
  opaque type Multiplier = Long

  object Multiplier:
    val Zero: Multiplier                     = 0L
    val One: Multiplier                      = Scale
    def apply(value: BigDecimal): Multiplier = fromDecimal(value)
    def apply(value: String): Multiplier     = parseDecimal(value)
    def apply(value: Int): Multiplier        = fromRaw(value.toLong * Scale)
    def apply(value: Long): Multiplier       = fromRaw(value * Scale)
    @targetName("fromScalar")
    def apply(value: Scalar): Multiplier     = fromRaw(value.toLong)
    def fromRaw(raw: Long): Multiplier       = raw

  extension (m: Multiplier)
    inline def toLong: Long                               = m
    def unary_- : Multiplier                              = -m
    def abs: Multiplier                                   = math.abs(m)
    def +(other: Multiplier): Multiplier                  = m + other
    def -(other: Multiplier): Multiplier                  = m - other
    def *(other: Multiplier): Multiplier                  = bankerRound(BigInt(m) * BigInt(other))
    def /(other: Multiplier): ScalarProvider.Scalar       = ScalarProvider.Scalar.fromRaw(ratioRaw(m, other))
    def pow(exponent: ScalarProvider.Scalar): Multiplier  = Multiplier.fromRaw(FixedPointMath.powRaw(m, exponent.toLong))
    def max(other: Multiplier): Multiplier                = math.max(m, other)
    def min(other: Multiplier): Multiplier                = math.min(m, other)
    def clamp(lo: Multiplier, hi: Multiplier): Multiplier = math.max(lo, math.min(hi, m))
    def format(fractionalDigits: Int): String             = FixedPointBase.format(m, fractionalDigits)
    def compact: String                                   = FixedPointBase.formatCompact(m)
    def >(other: Multiplier): Boolean                     = m > other
    def <(other: Multiplier): Boolean                     = m < other
    def >=(other: Multiplier): Boolean                    = m >= other
    def <=(other: Multiplier): Boolean                    = m <= other

  given Ordering[Multiplier] = Ordering.Long
