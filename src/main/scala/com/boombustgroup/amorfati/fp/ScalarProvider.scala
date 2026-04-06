package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Dimensionless scalar used for algebraic composition and ratios. */
object ScalarProvider:
  opaque type Scalar = Long

  object Scalar:
    val Zero: Scalar               = 0L
    val One: Scalar                = Scale
    def apply(d: Double): Scalar   = Math.round(d * Scale)
    def fromRaw(raw: Long): Scalar = raw

  extension (s: Scalar)
    inline def toLong: Long                   = s
    def unary_- : Scalar                      = -s
    def abs: Scalar                           = math.abs(s)
    def +(other: Scalar): Scalar              = s + other
    def -(other: Scalar): Scalar              = s - other
    def *(other: Scalar): Scalar              = bankerRound(BigInt(s) * BigInt(other))
    def /(n: Int): Scalar                     = Scalar.fromRaw(s / n.toLong)
    def max(other: Scalar): Scalar            = math.max(s, other)
    def min(other: Scalar): Scalar            = math.min(s, other)
    def clamp(lo: Scalar, hi: Scalar): Scalar = math.max(lo, math.min(hi, s))
    def >(other: Scalar): Boolean             = s > other
    def <(other: Scalar): Boolean             = s < other
    def >=(other: Scalar): Boolean            = s >= other
    def <=(other: Scalar): Boolean            = s <= other

  given Ordering[Scalar] = Ordering.Long
