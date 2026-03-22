package com.boombustgroup.amorfati

import scala.annotation.targetName

/** Marker annotation: this function deliberately converts fixed-point to Double
  * at a computation boundary (CES, Math.pow, CSV output). Grep for
  * `@computationBoundary` to audit all escape points in the codebase.
  */
class computationBoundary extends scala.annotation.StaticAnnotation

/** Explicit Double conversion — requires `import ComputationBoundary.toDouble`
  * and `@computationBoundary` annotation on the enclosing function. No
  * extension methods — cannot be called as `value.toDouble`, only as
  * `toDouble(value)`. This makes every Double escape grep-able and
  * code-review visible.
  */
object ComputationBoundary:
  private val Scale: Double = 10_000.0
  def toDouble(p: types.PLN): Double         = (p: Long).toDouble / Scale
  def toDouble(r: types.Rate): Double        = (r: Long).toDouble / Scale
  def toDouble(s: types.Share): Double       = (s: Long).toDouble / Scale
  def toDouble(m: types.Multiplier): Double  = (m: Long).toDouble / Scale
  def toDouble(c: types.Coefficient): Double = (c: Long).toDouble / Scale
  def toDouble(p: types.PriceIndex): Double  = (p: Long).toDouble / Scale
  def toDouble(s: types.Sigma): Double       = (s: Long).toDouble / Scale

/** Fixed-point type system for SFC-ABM engine.
  *
  * All monetary and behavioral values are Long-based (scale 10^4). Addition is
  * exact. Multiplication uses BigInt intermediate with banker's rounding
  * (half-even).
  *
  * NO `.toDouble` in public API. Double conversion requires explicit
  * `import ComputationBoundary.toDouble` + `@computationBoundary` annotation.
  *
  * SFC identity check: `flows.map(_.toLong).sum == 0L` — exact zero, no
  * tolerance.
  */
object types:

  // === Fixed-point scale ===
  private val Scale: Long     = 10_000L
  private val HalfScale: Long  = 5_000L
  private val ScaleD: Double   = Scale.toDouble

  /** Internal: convert raw Long to real Double value. NOT public API. */
  private inline def asDouble(raw: Long): Double = raw.toDouble / ScaleD

  /** Banker's rounding (half-even) for BigInt intermediate results. Eliminates
    * systematic drift across thousands of agents.
    */
  private def bankerRound(product: BigInt): Long =
    val quotient  = product / Scale
    val remainder = (product % Scale).abs.toLong
    if remainder < HalfScale then quotient.toLong
    else if remainder > HalfScale then (quotient + product.signum).toLong
    else if quotient % 2 == 0 then quotient.toLong
    else (quotient + product.signum).toLong

  // === Entity IDs (Int, not fixed-point) ===

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

  // === PLN — monetary amounts (stocks, flows) ===

  opaque type PLN = Long
  object PLN:
    val Zero: PLN               = 0L
    def apply(d: Double): PLN   = Math.round(d * Scale)
    def fromLong(l: Long): PLN  = l * Scale
    def fromRaw(raw: Long): PLN = raw

    extension (p: PLN)
      def toLong: Long                 = p
      def +(other: PLN): PLN           = p + other
      def -(other: PLN): PLN           = p - other
      def unary_- : PLN                = -p
      def abs: PLN                     = math.abs(p)
      def max(other: PLN): PLN         = math.max(p, other)
      def min(other: PLN): PLN         = math.min(p, other)
      def clamp(lo: PLN, hi: PLN): PLN = math.max(lo, math.min(hi, p))
      def >(other: PLN): Boolean       = p > other
      def <(other: PLN): Boolean       = p < other
      def >=(other: PLN): Boolean      = p >= other
      def <=(other: PLN): Boolean      = p <= other

      // Cross-type multiplication (BigInt intermediate, banker's rounding)
      @targetName("plnTimesRate")
      def *(r: Rate): PLN        = bankerRound(BigInt(p) * BigInt(r))
      @targetName("plnTimesShare")
      def *(s: Share): PLN       = bankerRound(BigInt(p) * BigInt(s))
      @targetName("plnTimesMultiplier")
      def *(m: Multiplier): PLN  = bankerRound(BigInt(p) * BigInt(m))
      @targetName("plnTimesCoefficient")
      def *(c: Coefficient): PLN = bankerRound(BigInt(p) * BigInt(c))

      // Scalar operations removed — use typed Multiplier/Share/Rate instead
      @targetName("plnDivPln")
      def /(other: PLN): Double  = if other != 0L then p.toDouble / other.toDouble else 0.0
      def /(divisor: Long): PLN  = p / divisor
      @targetName("plnDivShare")
      def /(s: Share): PLN       = if s != 0L then PLN(p.toDouble / Scale / ((s: Long).toDouble / Scale)) else PLN.Zero
      @targetName("plnDivMultiplier")
      def /(m: Multiplier): PLN  = if m != 0L then PLN(p.toDouble / Scale / ((m: Long).toDouble / Scale)) else PLN.Zero

    extension (n: Int)
      @targetName("intTimesPln")
      def *(p: PLN): PLN = p * n.toLong

    given Ordering[PLN] = Ordering.Long
    given Numeric[PLN] with
      def plus(x: PLN, y: PLN): PLN             = x + y
      def minus(x: PLN, y: PLN): PLN            = x - y
      def times(x: PLN, y: PLN): PLN            = PLN(x.toDouble * y.toDouble) // rare, use typed ops
      def negate(x: PLN): PLN                   = -x
      def fromInt(x: Int): PLN                  = PLN(x.toDouble)
      def parseString(str: String): Option[PLN] = str.toDoubleOption.map(PLN(_))
      def toInt(x: PLN): Int                    = (x / Scale).toInt
      def toLong(x: PLN): Long                  = x / Scale
      def toFloat(x: PLN): Float                = x.toDouble.toFloat
      def toDouble(x: PLN): Double              = x.toDouble
      def compare(x: PLN, y: PLN): Int          = java.lang.Long.compare(x, y)

  // === Rate — interest rates (annual, e.g., 0.0575 = 5.75%) ===

  opaque type Rate = Long
  object Rate:
    val Zero: Rate             = 0L
    def apply(d: Double): Rate = Math.round(d * Scale)

    extension (r: Rate)
      def toLong: Long                    = r
      def +(other: Rate): Rate            = r + other
      def -(other: Rate): Rate            = r - other
      def unary_- : Rate                  = -r
      def abs: Rate                       = math.abs(r)
      def max(other: Rate): Rate          = math.max(r, other)
      def min(other: Rate): Rate          = math.min(r, other)
      def clamp(lo: Rate, hi: Rate): Rate = math.max(lo, math.min(hi, r))
      def monthly: Rate                   = Rate(r.toDouble / Scale / 12.0)
      def annualize: Rate                 = Rate(r.toDouble / Scale * 12.0)
      def *(m: Multiplier): Rate           = Rate(r.toDouble / Scale * (m: Long).toDouble / Scale)
      @targetName("rateTimesShare")
      def *(s: Share): Rate               = Rate(r.toDouble / Scale * (s: Long).toDouble / Scale)
      @targetName("rateTimesCoefficient")
      def *(c: Coefficient): Rate         = Rate(r.toDouble / Scale * (c: Long).toDouble / Scale)
      @targetName("rateDivRate")
      def /(other: Rate): Double          = if other != 0L then r.toDouble / other.toDouble else 0.0
      def >(other: Rate): Boolean         = r > other
      def <(other: Rate): Boolean         = r < other
      def >=(other: Rate): Boolean        = r >= other
      def <=(other: Rate): Boolean        = r <= other

    given Ordering[Rate] = Ordering.Long
    given Numeric[Rate] with
      def plus(x: Rate, y: Rate): Rate           = x + y
      def minus(x: Rate, y: Rate): Rate          = x - y
      def times(x: Rate, y: Rate): Rate          = Rate(x.toDouble * y.toDouble)
      def negate(x: Rate): Rate                  = -x
      def fromInt(x: Int): Rate                  = Rate(x.toDouble)
      def parseString(str: String): Option[Rate] = str.toDoubleOption.map(Rate(_))
      def toInt(x: Rate): Int                    = (x / Scale).toInt
      def toLong(x: Rate): Long                  = x / Scale
      def toFloat(x: Rate): Float                = x.toDouble.toFloat
      def toDouble(x: Rate): Double              = x.toDouble
      def compare(x: Rate, y: Rate): Int         = java.lang.Long.compare(x, y)

  // === Share — bounded [0,1] fractions, probabilities, proportions ===

  opaque type Share = Long
  object Share:
    val Zero: Share                         = 0L
    val One: Share                          = Scale
    def apply(d: Double): Share             = Math.round(d * Scale)
    def fraction(num: Int, den: Int): Share = Math.round(num.toDouble / den.toDouble * Scale)

    extension (s: Share)
      def toLong: Long                       = s
      def unary_- : Share                    = -s
      def abs: Share                         = math.abs(s)
      def +(other: Share): Share             = s + other
      def -(other: Share): Share             = s - other
      def *(other: Share): Share             = bankerRound(BigInt(s) * BigInt(other))
      @targetName("shareTimesMultiplier")
      def *(m: Multiplier): Multiplier       = bankerRound(BigInt(s) * BigInt(m))
      @targetName("shareTimesCoefficient")
      def *(c: Coefficient): Coefficient     = bankerRound(BigInt(s) * BigInt(c))
      @targetName("shareTimesPln")
      def *(p: PLN): PLN                     = bankerRound(BigInt(p) * BigInt(s))
      @targetName("shareDivInt")
      def /(n: Int): Share                   = Share(s.toDouble / Scale / n)
      @targetName("shareDivShare")
      def /(other: Share): Double            = if other != 0L then s.toDouble / other.toDouble else 0.0
      def max(other: Share): Share           = math.max(s, other)
      def min(other: Share): Share           = math.min(s, other)
      def clamp(lo: Share, hi: Share): Share = math.max(lo, math.min(hi, s))
      def monthly: Share                     = Share(s.toDouble / Scale / 12.0)
      def sqrt: Share                        = Share(math.sqrt(s.toDouble / Scale))
      def toRate: Rate                       = Rate.apply(s.toDouble / Scale)
      def toMultiplier: Multiplier           = Multiplier.fromRaw(s)
      def >(other: Share): Boolean           = s > other
      def <(other: Share): Boolean           = s < other
      def >=(other: Share): Boolean          = s >= other
      def <=(other: Share): Boolean          = s <= other

    given Ordering[Share]     = Ordering.Long
    extension (n: Int)
      @targetName("intTimesShare")
      def *(s: Share): Double = n.toDouble * s.toDouble / Scale
    given Numeric[Share] with
      def plus(x: Share, y: Share): Share         = x + y
      def minus(x: Share, y: Share): Share        = x - y
      def times(x: Share, y: Share): Share        = x * y
      def negate(x: Share): Share                 = Share(-x.toDouble)
      def fromInt(x: Int): Share                  = Share(x.toDouble)
      def parseString(str: String): Option[Share] = str.toDoubleOption.map(Share(_))
      def toInt(x: Share): Int                    = (x / Scale).toInt
      def toLong(x: Share): Long                  = x / Scale
      def toFloat(x: Share): Float                = x.toDouble.toFloat
      def toDouble(x: Share): Double              = x.toDouble
      def compare(x: Share, y: Share): Int        = java.lang.Long.compare(x, y)

  // === Multiplier — [0,∞) multiplicative factors (markup, wage mult, labor eff) ===

  opaque type Multiplier = Long
  object Multiplier:
    val Zero: Multiplier               = 0L
    val One: Multiplier                = Scale
    def apply(d: Double): Multiplier   = Math.round(d * Scale)
    def fromRaw(raw: Long): Multiplier = raw

    extension (m: Multiplier)
      def toLong: Long                                      = m
      def unary_- : Multiplier                              = -m
      def abs: Multiplier                                   = math.abs(m)
      @targetName("multPlusMult")
      def +(other: Multiplier): Multiplier                  = m + other
      @targetName("multMinusMult")
      def -(other: Multiplier): Multiplier                  = m - other
      @targetName("multTimesMult")
      def *(other: Multiplier): Multiplier                  = bankerRound(BigInt(m) * BigInt(other))
      @targetName("multTimesShare")
      def *(s: Share): Multiplier                           = bankerRound(BigInt(m) * BigInt(s))
      @targetName("multTimesPln")
      def *(p: PLN): PLN                                    = bankerRound(BigInt(p) * BigInt(m))
      @targetName("multDivScalar")
      @targetName("multDivMult")
      def /(other: Multiplier): Double                      = if other != 0L then m.toDouble / other.toDouble else 0.0
      def max(other: Multiplier): Multiplier                = math.max(m, other)
      def min(other: Multiplier): Multiplier                = math.min(m, other)
      def clamp(lo: Multiplier, hi: Multiplier): Multiplier = math.max(lo, math.min(hi, m))
      def >(other: Multiplier): Boolean                     = m > other
      def <(other: Multiplier): Boolean                     = m < other
      def >=(other: Multiplier): Boolean                    = m >= other
      def <=(other: Multiplier): Boolean                    = m <= other

    given Ordering[Multiplier] = Ordering.Long

  // === Coefficient — (-∞,∞) behavioral parameters (sensitivity, elasticity, gap) ===

  opaque type Coefficient = Long
  object Coefficient:
    val Zero: Coefficient             = 0L
    val One: Coefficient              = Scale
    def apply(d: Double): Coefficient = Math.round(d * Scale)

    extension (c: Coefficient)
      def toLong: Long                         = c
      def unary_- : Coefficient                = -c
      @targetName("coefPlusCoef")
      def +(other: Coefficient): Coefficient   = c + other
      @targetName("coefMinusCoef")
      def -(other: Coefficient): Coefficient   = c - other
      @targetName("coefTimesCoef")
      def *(other: Coefficient): Coefficient   = bankerRound(BigInt(c) * BigInt(other))
      @targetName("coefTimesShare")
      def *(s: Share): Coefficient             = bankerRound(BigInt(c) * BigInt(s))
      @targetName("coefTimesPln")
      def *(p: PLN): PLN                       = bankerRound(BigInt(p) * BigInt(c))
      def abs: Coefficient                     = math.abs(c)
      def max(other: Coefficient): Coefficient = math.max(c, other)
      def min(other: Coefficient): Coefficient = math.min(c, other)
      def >(other: Coefficient): Boolean       = c > other
      def <(other: Coefficient): Boolean       = c < other
      def >=(other: Coefficient): Boolean      = c >= other
      def <=(other: Coefficient): Boolean      = c <= other

    given Ordering[Coefficient] = Ordering.Long

  // === PriceIndex — price indices (CPI, HPI, ETS), base = 1.0 ===

  opaque type PriceIndex = Long
  object PriceIndex:
    val Base: PriceIndex             = Scale
    def apply(d: Double): PriceIndex = Math.round(d * Scale)

    extension (p: PriceIndex)
      def toLong: Long                     = p
      @targetName("priceIdxTimesIdx")
      def *(other: PriceIndex): PriceIndex = bankerRound(BigInt(p) * BigInt(other))
      @targetName("priceIdxTimesRate")
      def *(r: Rate): PriceIndex           = PriceIndex(p.toDouble / Scale * (r: Long).toDouble / Scale)
      @targetName("priceIdxTimesMultiplier")
      def *(m: Multiplier): PriceIndex     = PriceIndex(p.toDouble / Scale * (m: Long).toDouble / Scale)
      @targetName("priceIdxTimesScalar")
      @targetName("priceIdxTimesPln")
      def *(pln: PLN): PLN                 = bankerRound(BigInt(pln) * BigInt(p))
      @targetName("priceIdxDivIdx")
      def /(other: PriceIndex): Double     = if other != 0L then p.toDouble / other.toDouble else 0.0
      def >(other: PriceIndex): Boolean    = p > other
      def <(other: PriceIndex): Boolean    = p < other
      def >=(other: PriceIndex): Boolean   = p >= other

  // === Sigma — CES elasticity of substitution (evolves via logistic dynamics) ===

  opaque type Sigma = Long
  object Sigma:
    val Zero: Sigma             = 0L
    def apply(d: Double): Sigma = Math.round(d * Scale)

    extension (s: Sigma)
      def toLong: Long              = s
      def +(other: Sigma): Sigma    = s + other
      def >(other: Sigma): Boolean  = s > other
      def <(other: Sigma): Boolean  = s < other
      def >=(other: Sigma): Boolean = s >= other

    given Ordering[Sigma] = Ordering.Long
