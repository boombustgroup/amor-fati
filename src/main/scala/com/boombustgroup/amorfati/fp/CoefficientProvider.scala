package com.boombustgroup.amorfati.fp

import FixedPointBase.*
import MultiplierProvider.Multiplier
import ScalarProvider.*
import scala.annotation.targetName

/** (-∞,∞) behavioral parameters (sensitivity, elasticity, gap). */
object CoefficientProvider:
  opaque type Coefficient = Long

  object Coefficient:
    val Zero: Coefficient                     = 0L
    val One: Coefficient                      = Scale
    def apply(value: BigDecimal): Coefficient = fromDecimal(value)
    def apply(value: String): Coefficient     = parseDecimal(value)
    def apply(value: Int): Coefficient        = fromRaw(value.toLong * Scale)
    def apply(value: Long): Coefficient       = fromRaw(value * Scale)
    @targetName("fromScalar")
    def apply(value: Scalar): Coefficient     = fromRaw(value.toLong)
    def fromRaw(raw: Long): Coefficient       = raw

  extension (c: Coefficient)
    inline def toLong: Long                   = c
    def unary_- : Coefficient                 = -c
    def abs: Coefficient                      = math.abs(c)
    def +(other: Coefficient): Coefficient    = c + other
    def -(other: Coefficient): Coefficient    = c - other
    def *(other: Coefficient): Coefficient    = multiplyRaw(c, other)
    def max(other: Coefficient): Coefficient  = math.max(c, other)
    def min(other: Coefficient): Coefficient  = math.min(c, other)
    def exp: Multiplier                       = Multiplier.fromRaw(FixedPointMath.expRaw(c))
    def format(fractionalDigits: Int): String = FixedPointBase.format(c, fractionalDigits)
    def compact: String                       = FixedPointBase.formatCompact(c)
    def >(other: Coefficient): Boolean        = c > other
    def <(other: Coefficient): Boolean        = c < other
    def >=(other: Coefficient): Boolean       = c >= other
    def <=(other: Coefficient): Boolean       = c <= other

  given Ordering[Coefficient] = Ordering.Long
