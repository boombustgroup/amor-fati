package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Interest rates (annual, e.g., 0.0575 = 5.75%). */
object RateProvider:
  opaque type Rate = Long

  object Rate:
    val Zero: Rate             = 0L
    def apply(d: Double): Rate = Math.round(d * Scale)

  extension (r: Rate)
    inline def toLong: Long             = r
    def +(other: Rate): Rate            = r + other
    def -(other: Rate): Rate            = r - other
    def unary_- : Rate                  = -r
    def abs: Rate                       = math.abs(r)
    def max(other: Rate): Rate          = math.max(r, other)
    def min(other: Rate): Rate          = math.min(r, other)
    def clamp(lo: Rate, hi: Rate): Rate = math.max(lo, math.min(hi, r))
    def monthly: Rate                   = Rate(asDouble(r) / 12.0)
    def annualize: Rate                 = Rate(asDouble(r) * 12.0)
    def /(other: Rate): Double          = if other != 0L then r.toDouble / other.toDouble else 0.0
    def >(other: Rate): Boolean         = r > other
    def <(other: Rate): Boolean         = r < other
    def >=(other: Rate): Boolean        = r >= other
    def <=(other: Rate): Boolean        = r <= other

  given Ordering[Rate] = Ordering.Long
  given Numeric[Rate] with
    def plus(x: Rate, y: Rate): Rate           = x + y
    def minus(x: Rate, y: Rate): Rate          = x - y
    def times(x: Rate, y: Rate): Rate          = Rate(asDouble(x) * asDouble(y))
    def negate(x: Rate): Rate                  = -x
    def fromInt(x: Int): Rate                  = Rate(x.toDouble)
    def parseString(str: String): Option[Rate] = str.toDoubleOption.map(Rate(_))
    def toInt(x: Rate): Int                    = (x / Scale).toInt
    def toLong(x: Rate): Long                  = x / Scale
    def toFloat(x: Rate): Float                = asDouble(x).toFloat
    def toDouble(x: Rate): Double              = asDouble(x)
    def compare(x: Rate, y: Rate): Int         = java.lang.Long.compare(x, y)
