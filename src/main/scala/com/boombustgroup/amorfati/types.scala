package com.boombustgroup.amorfati

import scala.annotation.targetName

object types:
  // === Entity IDs ===
  opaque type BankId = Int
  object BankId:
    inline def apply(i: Int): BankId            = i
    val NoBank: BankId                          = -1
    extension (b: BankId) inline def toInt: Int = b

  opaque type FirmId = Int
  object FirmId:
    inline def apply(i: Int): FirmId            = i
    extension (f: FirmId) inline def toInt: Int = f

  opaque type HhId = Int
  object HhId:
    inline def apply(i: Int): HhId            = i
    extension (h: HhId) inline def toInt: Int = h
    given Ordering[HhId]                      = Ordering.Int

  opaque type SectorIdx = Int
  object SectorIdx:
    inline def apply(i: Int): SectorIdx            = i
    extension (s: SectorIdx) inline def toInt: Int = s

  // === Monetary amounts ===
  opaque type PLN = Double
  object PLN:
    inline def apply(d: Double): PLN = d
    val Zero: PLN                    = 0.0
    extension (p: PLN)
      inline def +(other: PLN): PLN           = p + other
      inline def -(other: PLN): PLN           = p - other
      inline def *(scalar: Double): PLN       = p * scalar
      @targetName("plnTimesRate")
      inline def *(r: Rate): PLN              = p * r
      @targetName("plnTimesRatio")
      inline def *(r: Ratio): PLN             = p * r
      @targetName("plnDivPln")
      inline def /(other: PLN): Double        = p / other
      @targetName("plnDivScalar")
      inline def /(scalar: Double): PLN       = p / scalar
      @targetName("plnDivRatio")
      inline def /(r: Ratio): PLN             = p / r
      inline def unary_- : PLN                = -p
      inline def abs: PLN                     = math.abs(p)
      inline def max(other: PLN): PLN         = math.max(p, other)
      inline def min(other: PLN): PLN         = math.min(p, other)
      inline def clamp(lo: PLN, hi: PLN): PLN = math.max(lo, math.min(hi, p))
      inline def toDouble: Double             = p
      inline def >(other: PLN): Boolean       = p > other
      inline def <(other: PLN): Boolean       = p < other
      inline def >=(other: PLN): Boolean      = p >= other
      inline def <=(other: PLN): Boolean      = p <= other
    given Ordering[PLN]              = Ordering.Double.TotalOrdering
    given Numeric[PLN] with
      def plus(x: PLN, y: PLN): PLN             = x + y
      def minus(x: PLN, y: PLN): PLN            = x - y
      def times(x: PLN, y: PLN): PLN            = PLN(x.toDouble * y.toDouble)
      def negate(x: PLN): PLN                   = -x
      def fromInt(x: Int): PLN                  = PLN(x.toDouble)
      def parseString(str: String): Option[PLN] = str.toDoubleOption.map(PLN(_))
      def toInt(x: PLN): Int                    = x.toDouble.toInt
      def toLong(x: PLN): Long                  = x.toDouble.toLong
      def toFloat(x: PLN): Float                = x.toDouble.toFloat
      def toDouble(x: PLN): Double              = x.toDouble
      def compare(x: PLN, y: PLN): Int          = java.lang.Double.compare(x, y)

  // === Interest rates (annual, e.g., 0.0575 = 5.75%) ===
  opaque type Rate = Double
  object Rate:
    inline def apply(d: Double): Rate = d
    val Zero: Rate                    = 0.0
    extension (r: Rate)
      inline def +(other: Rate): Rate            = r + other
      inline def -(other: Rate): Rate            = r - other
      inline def *(scalar: Double): Rate         = r * scalar
      @targetName("rateTimesRatio")
      inline def *(ratio: Ratio): Rate           = r * ratio
      inline def /(scalar: Double): Rate         = r / scalar
      @targetName("rateDivRate")
      inline def /(other: Rate): Double          = r / other
      inline def unary_- : Rate                  = -r
      inline def abs: Rate                       = math.abs(r)
      inline def max(other: Rate): Rate          = math.max(r, other)
      inline def min(other: Rate): Rate          = math.min(r, other)
      inline def clamp(lo: Rate, hi: Rate): Rate = math.max(lo, math.min(hi, r))
      inline def monthly: Rate                   = r / 12.0
      inline def toDouble: Double                = r
      inline def >(other: Rate): Boolean         = r > other
      inline def <(other: Rate): Boolean         = r < other
      inline def >=(other: Rate): Boolean        = r >= other
      inline def <=(other: Rate): Boolean        = r <= other
    given Ordering[Rate]              = Ordering.Double.TotalOrdering
    given Numeric[Rate] with
      def plus(x: Rate, y: Rate): Rate           = x + y
      def minus(x: Rate, y: Rate): Rate          = x - y
      def times(x: Rate, y: Rate): Rate          = Rate(x.toDouble * y.toDouble)
      def negate(x: Rate): Rate                  = -x
      def fromInt(x: Int): Rate                  = Rate(x.toDouble)
      def parseString(str: String): Option[Rate] = str.toDoubleOption.map(Rate(_))
      def toInt(x: Rate): Int                    = x.toDouble.toInt
      def toLong(x: Rate): Long                  = x.toDouble.toLong
      def toFloat(x: Rate): Float                = x.toDouble.toFloat
      def toDouble(x: Rate): Double              = x.toDouble
      def compare(x: Rate, y: Rate): Int         = java.lang.Double.compare(x, y)

  // === Ratios (0-1 range: shares, probabilities, adoption rates) ===
  opaque type Ratio = Double
  object Ratio:
    inline def apply(d: Double): Ratio             = d
    inline def fraction(num: Int, den: Int): Ratio = num.toDouble / den.toDouble
    val Zero: Ratio                                = 0.0
    val One: Ratio                                 = 1.0
    extension (r: Ratio)
      inline def +(other: Ratio): Ratio             = r + other
      inline def -(other: Ratio): Ratio             = r - other
      inline def *(scalar: Double): Ratio           = r * scalar
      @targetName("ratioTimesRatio")
      inline def *(other: Ratio): Ratio             = r * other
      @targetName("ratioTimesPln")
      inline def *(p: PLN): PLN                     = p * r
      @targetName("ratioDivScalar")
      inline def /(scalar: Double): Ratio           = r / scalar
      @targetName("ratioDivRatio")
      inline def /(other: Ratio): Double            = r / other
      inline def max(other: Ratio): Ratio           = math.max(r, other)
      inline def min(other: Ratio): Ratio           = math.min(r, other)
      inline def clamp(lo: Ratio, hi: Ratio): Ratio = math.max(lo, math.min(hi, r))
      inline def monthly: Ratio                     = r / 12.0
      inline def toRate: Rate                       = Rate(r) // explicit Ratio → Rate at semantic boundary
      inline def toDouble: Double                   = r
      inline def >(other: Ratio): Boolean           = r > other
      inline def <(other: Ratio): Boolean           = r < other
      inline def >=(other: Ratio): Boolean          = r >= other
      inline def <=(other: Ratio): Boolean          = r <= other
    given Ordering[Ratio]                          = Ordering.Double.TotalOrdering
    given Numeric[Ratio] with
      def plus(x: Ratio, y: Ratio): Ratio         = x + y
      def minus(x: Ratio, y: Ratio): Ratio        = x - y
      def times(x: Ratio, y: Ratio): Ratio        = x * y
      def negate(x: Ratio): Ratio                 = Ratio(-x.toDouble)
      def fromInt(x: Int): Ratio                  = Ratio(x.toDouble)
      def parseString(str: String): Option[Ratio] = str.toDoubleOption.map(Ratio(_))
      def toInt(x: Ratio): Int                    = x.toDouble.toInt
      def toLong(x: Ratio): Long                  = x.toDouble.toLong
      def toFloat(x: Ratio): Float                = x.toDouble.toFloat
      def toDouble(x: Ratio): Double              = x.toDouble
      def compare(x: Ratio, y: Ratio): Int        = java.lang.Double.compare(x, y)
