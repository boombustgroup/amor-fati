package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import ScalarProvider.*
import scala.annotation.targetName

/** Interest rates (annual, e.g., 0.0575 = 5.75%). */
object RateProvider:
  opaque type Rate = Long

  object Rate:
    val Zero: Rate                     = 0L
    def apply(value: BigDecimal): Rate = fromDecimal(value)
    def apply(value: String): Rate     = parseDecimal(value)
    def apply(value: Int): Rate        = fromRaw(value.toLong * Scale)
    def apply(value: Long): Rate       = fromRaw(value * Scale)
    @targetName("fromScalar")
    def apply(value: Scalar): Rate     = fromRaw(value.toLong)
    def fromRaw(raw: Long): Rate       = raw

  extension (r: Rate)
    inline def toLong: Long                   = r
    def +(other: Rate): Rate                  = r + other
    def -(other: Rate): Rate                  = r - other
    def unary_- : Rate                        = -r
    def abs: Rate                             = math.abs(r)
    def max(other: Rate): Rate                = math.max(r, other)
    def min(other: Rate): Rate                = math.min(r, other)
    def clamp(lo: Rate, hi: Rate): Rate       = math.max(lo, math.min(hi, r))
    def monthly: Rate                         = Rate.fromRaw(r / 12L)
    def annualize: Rate                       = Rate.fromRaw(r * 12L)
    def /(other: Rate): ScalarProvider.Scalar = ScalarProvider.Scalar.fromRaw(ratioRaw(r, other))
    def format(fractionalDigits: Int): String = FixedPointBase.format(r, fractionalDigits)
    def compact: String                       = FixedPointBase.formatCompact(r)
    def >(other: Rate): Boolean               = r > other
    def <(other: Rate): Boolean               = r < other
    def >=(other: Rate): Boolean              = r >= other
    def <=(other: Rate): Boolean              = r <= other

  given Ordering[Rate] = Ordering.Long
