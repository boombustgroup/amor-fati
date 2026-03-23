package com.boombustgroup.amorfati.fp

import scala.annotation.targetName
import FixedPointBase.*

/** Monetary amounts — stocks and flows. SFC identity: diff == 0L. */
object PLNProvider:
  opaque type PLN = Long

  object PLN:
    val Zero: PLN               = 0L
    def apply(d: Double): PLN   = Math.round(d * Scale)
    def fromLong(l: Long): PLN  = l * Scale
    def fromRaw(raw: Long): PLN = raw

  extension (p: PLN)
    inline def toLong: Long          = p
    def +(other: PLN): PLN           = p + other
    def -(other: PLN): PLN           = p - other
    def unary_- : PLN                = -p
    def abs: PLN                     = math.abs(p)
    def max(other: PLN): PLN         = math.max(p, other)
    def min(other: PLN): PLN         = math.min(p, other)
    def clamp(lo: PLN, hi: PLN): PLN = math.max(lo, math.min(hi, p))
    @targetName("plnDivPln")
    def /(other: PLN): Double        = if other != 0L then p.toDouble / other.toDouble else 0.0
    @targetName("plnDivLong")
    def /(divisor: Long): PLN        = p / divisor
    def >(other: PLN): Boolean       = p > other
    def <(other: PLN): Boolean       = p < other
    def >=(other: PLN): Boolean      = p >= other
    def <=(other: PLN): Boolean      = p <= other

  extension (n: Int) def *(p: PLN): PLN = p * n.toLong

  given Ordering[PLN] = Ordering.Long
  given Numeric[PLN] with
    def plus(x: PLN, y: PLN): PLN             = x + y
    def minus(x: PLN, y: PLN): PLN            = x - y
    def times(x: PLN, y: PLN): PLN            = PLN(asDouble(x) * asDouble(y))
    def negate(x: PLN): PLN                   = -x
    def fromInt(x: Int): PLN                  = PLN(x.toDouble)
    def parseString(str: String): Option[PLN] = str.toDoubleOption.map(PLN(_))
    def toInt(x: PLN): Int                    = (x / Scale).toInt
    def toLong(x: PLN): Long                  = x / Scale
    def toFloat(x: PLN): Float                = asDouble(x).toFloat
    def toDouble(x: PLN): Double              = asDouble(x)
    def compare(x: PLN, y: PLN): Int          = java.lang.Long.compare(x, y)
