package com.boombustgroup.amorfati.fp

import scala.annotation.targetName
import FixedPointBase.*

/** Monetary amounts — stocks and flows. SFC identity: diff == 0L. */
object PLNProvider:
  opaque type PLN = Long

  object PLN:
    val Zero: PLN                                           = 0L
    def apply(value: Int): PLN                              = fromLong(value.toLong)
    def apply(value: Long): PLN                             = fromLong(value)
    def decimal(unscaled: Long, fractionalDigits: Int): PLN =
      fromRaw(decimalRaw(unscaled, fractionalDigits))
    def fromLong(l: Long): PLN                              = l * Scale
    def fromRaw(raw: Long): PLN                             = raw

  extension (p: PLN)
    inline def toLong: Long                   = p
    def +(other: PLN): PLN                    = p + other
    def -(other: PLN): PLN                    = p - other
    def unary_- : PLN                         = -p
    def abs: PLN                              = math.abs(p)
    def max(other: PLN): PLN                  = math.max(p, other)
    def min(other: PLN): PLN                  = math.min(p, other)
    def clamp(lo: PLN, hi: PLN): PLN          = math.max(lo, math.min(hi, p))
    @targetName("plnDivPln")
    def /(other: PLN): ScalarProvider.Scalar  = ScalarProvider.Scalar.fromRaw(ratioRaw(p, other))
    @targetName("plnDivLong")
    def /(divisor: Long): PLN                 = p / divisor
    def format(fractionalDigits: Int): String = FixedPointBase.format(p, fractionalDigits)
    def compact: String                       = FixedPointBase.formatCompact(p)
    def >(other: PLN): Boolean                = p > other
    def <(other: PLN): Boolean                = p < other
    def >=(other: PLN): Boolean               = p >= other
    def <=(other: PLN): Boolean               = p <= other

  extension (n: Int) def *(p: PLN): PLN = p * n.toLong

  given Ordering[PLN] = Ordering.Long
