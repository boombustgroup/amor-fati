package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Price indices (CPI, HPI, ETS), base = 1.0. */
object PriceIndexProvider:
  opaque type PriceIndex = Long

  object PriceIndex:
    val Base: PriceIndex             = Scale
    def apply(d: Double): PriceIndex = Math.round(d * Scale)

  extension (p: PriceIndex)
    inline def toLong: Long                  = p
    def *(other: PriceIndex): PriceIndex     = bankerRound(BigInt(p) * BigInt(other))
    def /(other: PriceIndex): Double         = if other != 0L then p.toDouble / other.toDouble else 0.0
    def >(other: PriceIndex): Boolean        = p > other
    def <(other: PriceIndex): Boolean        = p < other
    def >=(other: PriceIndex): Boolean       = p >= other
