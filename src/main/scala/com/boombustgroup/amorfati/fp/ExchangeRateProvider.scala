package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Exchange-rate level, e.g. PLN per EUR. */
object ExchangeRateProvider:
  opaque type ExchangeRate = Long

  object ExchangeRate:
    def apply(value: BigDecimal): ExchangeRate             =
      require(value > 0, s"ExchangeRate must be > 0, got: $value")
      fromDecimal(value)
    def apply(value: String): ExchangeRate                 =
      apply(BigDecimal(value.replace("_", "")))
    def apply(value: Int): ExchangeRate                    = fromRaw(value.toLong * Scale)
    def apply(value: Long): ExchangeRate                   = fromRaw(value * Scale)
    private[amorfati] def fromRaw(raw: Long): ExchangeRate =
      require(raw > 0L, s"ExchangeRate raw value must be > 0, got: $raw")
      raw

  extension (er: ExchangeRate)
    inline def toLong: Long                                     = er
    def max(other: ExchangeRate): ExchangeRate                  = math.max(er, other)
    def min(other: ExchangeRate): ExchangeRate                  = math.min(er, other)
    def clamp(lo: ExchangeRate, hi: ExchangeRate): ExchangeRate = math.max(lo, math.min(hi, er))
    def format(fractionalDigits: Int): String                   = FixedPointBase.format(er, fractionalDigits)
    def compact: String                                         = FixedPointBase.formatCompact(er)
    def >(other: ExchangeRate): Boolean                         = er > other
    def <(other: ExchangeRate): Boolean                         = er < other
    def >=(other: ExchangeRate): Boolean                        = er >= other
    def <=(other: ExchangeRate): Boolean                        = er <= other

  given Ordering[ExchangeRate] = Ordering.Long
