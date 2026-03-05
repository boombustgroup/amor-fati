package sfc

import scala.annotation.targetName

object types:
  // === Entity IDs ===
  opaque type BankId = Int
  object BankId:
    inline def apply(i: Int): BankId = i
    val NoBank: BankId = -1
    extension (b: BankId)
      inline def toInt: Int = b
      inline def ==(other: Int): Boolean = b == other

  opaque type FirmId = Int
  object FirmId:
    inline def apply(i: Int): FirmId = i
    extension (f: FirmId) inline def toInt: Int = f

  opaque type SectorIdx = Int
  object SectorIdx:
    inline def apply(i: Int): SectorIdx = i
    extension (s: SectorIdx)
      inline def toInt: Int = s

  // === Monetary amounts ===
  opaque type PLN = Double
  object PLN:
    inline def apply(d: Double): PLN = d
    val Zero: PLN = 0.0
    extension (p: PLN)
      inline def +(other: PLN): PLN = p + other
      inline def -(other: PLN): PLN = p - other
      inline def *(scalar: Double): PLN = p * scalar
      @targetName("plnTimesRate")
      inline def *(r: Rate): PLN = p * r
      @targetName("plnDivPln")
      inline def /(other: PLN): Double = p / other
      @targetName("plnDivScalar")
      inline def /(scalar: Double): PLN = p / scalar
      inline def unary_- : PLN = -p
      inline def abs: PLN = math.abs(p)
      inline def max(other: PLN): PLN = math.max(p, other)
      inline def min(other: PLN): PLN = math.min(p, other)
      inline def toDouble: Double = p
      inline def >(other: PLN): Boolean = p > other
      inline def <(other: PLN): Boolean = p < other
      inline def >=(other: PLN): Boolean = p >= other
      inline def <=(other: PLN): Boolean = p <= other

  // === Interest rates (annual, e.g., 0.0575 = 5.75%) ===
  opaque type Rate = Double
  object Rate:
    inline def apply(d: Double): Rate = d
    val Zero: Rate = 0.0
    extension (r: Rate)
      inline def +(other: Rate): Rate = r + other
      inline def -(other: Rate): Rate = r - other
      inline def *(scalar: Double): Rate = r * scalar
      inline def /(scalar: Double): Rate = r / scalar
      inline def unary_- : Rate = -r
      inline def abs: Rate = math.abs(r)
      inline def max(other: Rate): Rate = math.max(r, other)
      inline def min(other: Rate): Rate = math.min(r, other)
      inline def toDouble: Double = r
      inline def >(other: Rate): Boolean = r > other
      inline def <(other: Rate): Boolean = r < other

  // === Ratios (0-1 range: shares, probabilities, adoption rates) ===
  opaque type Ratio = Double
  object Ratio:
    inline def apply(d: Double): Ratio = d
    val Zero: Ratio = 0.0
    val One: Ratio = 1.0
    extension (r: Ratio)
      inline def +(other: Ratio): Ratio = r + other
      inline def -(other: Ratio): Ratio = r - other
      inline def *(scalar: Double): Ratio = r * scalar
      @targetName("ratioTimesRatio")
      inline def *(other: Ratio): Ratio = r * other
      inline def toDouble: Double = r
      inline def >(other: Ratio): Boolean = r > other
      inline def <(other: Ratio): Boolean = r < other
