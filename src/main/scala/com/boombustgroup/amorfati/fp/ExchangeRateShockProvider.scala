package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Signed relative exchange-rate change / market shock. */
object ExchangeRateShockProvider:
  opaque type ExchangeRateShock = Long

  object ExchangeRateShock:
    val Zero: ExchangeRateShock               = 0L
    def apply(d: Double): ExchangeRateShock   = Math.round(d * Scale)
    def fromRaw(raw: Long): ExchangeRateShock = raw

  extension (shock: ExchangeRateShock)
    inline def toLong: Long                                                    = shock
    def unary_- : ExchangeRateShock                                            = -shock
    def abs: ExchangeRateShock                                                 = math.abs(shock)
    def max(other: ExchangeRateShock): ExchangeRateShock                       = math.max(shock, other)
    def min(other: ExchangeRateShock): ExchangeRateShock                       = math.min(shock, other)
    def clamp(lo: ExchangeRateShock, hi: ExchangeRateShock): ExchangeRateShock = math.max(lo, math.min(hi, shock))
    def sign: Int                                                              = java.lang.Long.signum(shock)
    def >(other: ExchangeRateShock): Boolean                                   = shock > other
    def <(other: ExchangeRateShock): Boolean                                   = shock < other
    def >=(other: ExchangeRateShock): Boolean                                  = shock >= other
    def <=(other: ExchangeRateShock): Boolean                                  = shock <= other

  given Ordering[ExchangeRateShock] = Ordering.Long
