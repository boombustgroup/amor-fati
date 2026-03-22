package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** [0,∞) multiplicative factors (markup, wage mult, labor eff). */
object MultiplierProvider:
  opaque type Multiplier = Long

  object Multiplier:
    val Zero: Multiplier               = 0L
    val One: Multiplier                = Scale
    def apply(d: Double): Multiplier   = Math.round(d * Scale)
    def fromRaw(raw: Long): Multiplier = raw

  extension (m: Multiplier)
    inline def toLong: Long                               = m
    def unary_- : Multiplier                              = -m
    def abs: Multiplier                                   = math.abs(m)
    def +(other: Multiplier): Multiplier                  = m + other
    def -(other: Multiplier): Multiplier                  = m - other
    def *(other: Multiplier): Multiplier                  = bankerRound(BigInt(m) * BigInt(other))
    def /(other: Multiplier): Double                      = if other != 0L then m.toDouble / other.toDouble else 0.0
    def max(other: Multiplier): Multiplier                = math.max(m, other)
    def min(other: Multiplier): Multiplier                = math.min(m, other)
    def clamp(lo: Multiplier, hi: Multiplier): Multiplier = math.max(lo, math.min(hi, m))
    def >(other: Multiplier): Boolean                     = m > other
    def <(other: Multiplier): Boolean                     = m < other
    def >=(other: Multiplier): Boolean                    = m >= other
    def <=(other: Multiplier): Boolean                    = m <= other

  given Ordering[Multiplier] = Ordering.Long
