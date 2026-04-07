package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Exchange-rate level, e.g. PLN per EUR. */
object ExchangeRateProvider:
  opaque type ExchangeRate = Long

  object ExchangeRate:
    val Zero: ExchangeRate               = 0L
    def apply(d: Double): ExchangeRate   = Math.round(d * Scale)
    def fromRaw(raw: Long): ExchangeRate = raw

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
