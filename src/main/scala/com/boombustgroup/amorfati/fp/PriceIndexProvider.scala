package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import com.boombustgroup.amorfati.types.Scalar

/** Price indices (CPI, HPI, ETS), base = 1.0. */
object PriceIndexProvider:
  opaque type PriceIndex = Long

  object PriceIndex:
    val Zero: PriceIndex                     = 0L
    val Base: PriceIndex                     = Scale
    def apply(value: BigDecimal): PriceIndex = fromDecimal(value)
    def apply(value: String): PriceIndex     = parseDecimal(value)
    def apply(value: Int): PriceIndex        = fromRaw(value.toLong * Scale)
    def apply(value: Long): PriceIndex       = fromRaw(value * Scale)
    def fromRaw(raw: Long): PriceIndex       = raw

  extension (p: PriceIndex)
    inline def toLong: Long                   = p
    def +(other: PriceIndex): PriceIndex      = p + other
    def -(other: PriceIndex): PriceIndex      = p - other
    def *(other: PriceIndex): PriceIndex      = multiplyRaw(p, other)
    def /(other: PriceIndex): Scalar          = Scalar.fromRaw(ratioRaw(p, other))
    def max(other: PriceIndex): PriceIndex    = math.max(p, other)
    def min(other: PriceIndex): PriceIndex    = math.min(p, other)
    def format(fractionalDigits: Int): String = FixedPointBase.format(p, fractionalDigits)
    def compact: String                       = FixedPointBase.formatCompact(p)
    def >(other: PriceIndex): Boolean         = p > other
    def <(other: PriceIndex): Boolean         = p < other
    def >=(other: PriceIndex): Boolean        = p >= other
    def <=(other: PriceIndex): Boolean        = p <= other

  given Ordering[PriceIndex] = Ordering.Long
