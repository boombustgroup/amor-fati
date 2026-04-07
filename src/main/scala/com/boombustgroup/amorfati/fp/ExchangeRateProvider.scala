package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Exchange-rate level, e.g. PLN per EUR. */
object ExchangeRateProvider:
  opaque type ExchangeRate = Long

  object ExchangeRate:
    def apply(d: Double): ExchangeRate                     =
      require(d.isFinite && d > 0.0, s"ExchangeRate must be finite and > 0, got: $d")
      Math.round(d * Scale)
    private[amorfati] def fromRaw(raw: Long): ExchangeRate =
      require(raw > 0L, s"ExchangeRate raw value must be > 0, got: $raw")
      raw

  extension (er: ExchangeRate)
    inline def toLong: Long                                     = er
    def max(other: ExchangeRate): ExchangeRate                  = math.max(er, other)
    def min(other: ExchangeRate): ExchangeRate                  = math.min(er, other)
    def clamp(lo: ExchangeRate, hi: ExchangeRate): ExchangeRate = math.max(lo, math.min(hi, er))
    def >(other: ExchangeRate): Boolean                         = er > other
    def <(other: ExchangeRate): Boolean                         = er < other
    def >=(other: ExchangeRate): Boolean                        = er >= other
    def <=(other: ExchangeRate): Boolean                        = er <= other

  given Ordering[ExchangeRate] = Ordering.Long
