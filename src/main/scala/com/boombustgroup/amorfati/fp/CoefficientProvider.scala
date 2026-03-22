package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** (-∞,∞) behavioral parameters (sensitivity, elasticity, gap). */
object CoefficientProvider:
  opaque type Coefficient = Long

  object Coefficient:
    val Zero: Coefficient             = 0L
    val One: Coefficient              = Scale
    def apply(d: Double): Coefficient = Math.round(d * Scale)

  extension (c: Coefficient)
    inline def toLong: Long                  = c
    def unary_- : Coefficient                = -c
    def abs: Coefficient                     = math.abs(c)
    def +(other: Coefficient): Coefficient   = c + other
    def -(other: Coefficient): Coefficient   = c - other
    def *(other: Coefficient): Coefficient   = bankerRound(BigInt(c) * BigInt(other))
    def max(other: Coefficient): Coefficient = math.max(c, other)
    def min(other: Coefficient): Coefficient = math.min(c, other)
    def >(other: Coefficient): Boolean       = c > other
    def <(other: Coefficient): Boolean       = c < other
    def >=(other: Coefficient): Boolean      = c >= other
    def <=(other: Coefficient): Boolean      = c <= other

  given Ordering[Coefficient] = Ordering.Long
