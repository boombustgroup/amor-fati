package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Price indices (CPI, HPI, ETS), base = 1.0. */
object PriceIndexProvider:
  opaque type PriceIndex = Long

  object PriceIndex:
    val Zero: PriceIndex               = 0L
    val Base: PriceIndex               = Scale
    def apply(d: Double): PriceIndex   = Math.round(d * Scale)
    def fromRaw(raw: Long): PriceIndex = raw

  extension (p: PriceIndex)
    inline def toLong: Long                = p
    def +(other: PriceIndex): PriceIndex   = p + other
    def -(other: PriceIndex): PriceIndex   = p - other
    def *(other: PriceIndex): PriceIndex   = bankerRound(BigInt(p) * BigInt(other))
    def /(other: PriceIndex): Double       = if other != 0L then p.toDouble / other.toDouble else 0.0
    def max(other: PriceIndex): PriceIndex = math.max(p, other)
    def min(other: PriceIndex): PriceIndex = math.min(p, other)
    def >(other: PriceIndex): Boolean      = p > other
    def <(other: PriceIndex): Boolean      = p < other
    def >=(other: PriceIndex): Boolean     = p >= other
    def <=(other: PriceIndex): Boolean     = p <= other

  given Ordering[PriceIndex] = Ordering.Long
