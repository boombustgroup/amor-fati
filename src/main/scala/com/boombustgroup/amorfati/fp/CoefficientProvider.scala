package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import MultiplierProvider.Multiplier

/** (-∞,∞) behavioral parameters (sensitivity, elasticity, gap). */
object CoefficientProvider:
  opaque type Coefficient = Long

  object Coefficient:
    val Zero: Coefficient               = 0L
    val One: Coefficient                = Scale
    def apply(d: Double): Coefficient   = Math.round(d * Scale)
    def fromRaw(raw: Long): Coefficient = raw

  extension (c: Coefficient)
    inline def toLong: Long                  = c
    def unary_- : Coefficient                = -c
    def abs: Coefficient                     = math.abs(c)
    def +(other: Coefficient): Coefficient   = c + other
    def -(other: Coefficient): Coefficient   = c - other
    def *(other: Coefficient): Coefficient   = bankerRound(BigInt(c) * BigInt(other))
    def max(other: Coefficient): Coefficient = math.max(c, other)
    def min(other: Coefficient): Coefficient = math.min(c, other)
    def exp: Multiplier                      = Multiplier(math.exp(c.toDouble / ScaleD))
    def >(other: Coefficient): Boolean       = c > other
    def <(other: Coefficient): Boolean       = c < other
    def >=(other: Coefficient): Boolean      = c >= other
    def <=(other: Coefficient): Boolean      = c <= other

  given Ordering[Coefficient] = Ordering.Long
