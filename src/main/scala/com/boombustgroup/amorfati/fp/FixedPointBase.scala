package com.boombustgroup.amorfati.fp

/** Shared constants and helpers for all fixed-point types. */
object FixedPointBase:
  val Scale: Long              = 10_000L
  val HalfScale: Long          = 5_000L
  val ScaleBig: BigInt         = BigInt(Scale)
  val ScaleDecimal: BigDecimal = BigDecimal(Scale)

  /** Banker's rounding (half-even) for BigInt intermediate results. */
  def bankerRound(product: BigInt): Long =
    val quotient  = product / Scale
    val remainder = (product % Scale).abs.toLong
    if remainder < HalfScale then quotient.toLong
    else if remainder > HalfScale then (quotient + product.signum).toLong
    else if quotient % 2 == 0 then quotient.toLong
    else (quotient + product.signum).toLong

  /** Parse a decimal literal into the shared fixed-point raw scale. */
  def parseDecimal(value: String): Long =
    (BigDecimal(value.replace("_", "")) * ScaleDecimal).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLongExact

  /** Convert a decimal value into the shared fixed-point raw scale. */
  def fromDecimal(value: BigDecimal): Long =
    (value * ScaleDecimal).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLongExact

  /** Render a fixed-point raw value using a fixed number of fractional digits.
    */
  def format(raw: Long, fractionalDigits: Int): String =
    require(fractionalDigits >= 0, s"fractionalDigits must be non-negative, got $fractionalDigits")
    val sign         = if raw < 0L then "-" else ""
    val absRaw       = BigInt(raw).abs
    val integral     = absRaw / ScaleBig
    val fractional   = absRaw % ScaleBig
    val fullFrac     = fractional.toString.reverse.padTo(4, '0').reverse
    val renderedFrac =
      if fractionalDigits <= 4 then fullFrac.take(fractionalDigits)
      else fullFrac + ("0" * (fractionalDigits - 4))
    if fractionalDigits == 0 then s"$sign$integral"
    else s"$sign$integral.$renderedFrac"

  /** Render a fixed-point raw value without insignificant trailing zeros. */
  def formatCompact(raw: Long): String =
    val fixed = format(raw, 4)
    if fixed.contains('.') then fixed.reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
    else fixed

  /** Banker's rounding (half-even) for integer division on raw fixed-point
    * values.
    */
  def divideRaw(dividend: Long, divisor: Long): Long =
    if divisor == 0L then 0L
    else
      val dividendBig    = BigInt(dividend)
      val divisorBig     = BigInt(divisor)
      val quotient       = dividendBig / divisorBig
      val remainder      = (dividendBig % divisorBig).abs
      val divisorAbs     = divisorBig.abs
      val twiceRemainder = remainder * 2
      if twiceRemainder < divisorAbs then quotient.toLong
      else
        val resultSign = dividendBig.signum * divisorBig.signum
        if twiceRemainder > divisorAbs then (quotient + resultSign).toLong
        else if quotient % 2 == 0 then quotient.toLong
        else (quotient + resultSign).toLong

  /** Fixed-point division: `(numerator / denominator) * Scale`. */
  def ratioRaw(numerator: Long, denominator: Long): Long =
    if denominator == 0L then 0L
    else
      val scaled         = BigInt(numerator) * ScaleBig
      val den            = BigInt(denominator)
      val quotient       = scaled / den
      val remainder      = (scaled % den).abs
      val denominatorAbs = den.abs
      val twiceRemainder = remainder * 2
      if twiceRemainder < denominatorAbs then quotient.toLong
      else
        val resultSign = scaled.signum * den.signum
        if twiceRemainder > denominatorAbs then (quotient + resultSign).toLong
        else if quotient % 2 == 0 then quotient.toLong
        else (quotient + resultSign).toLong

  def multiplyRaw(left: Long, right: Long): Long =
    bankerRound(BigInt(left) * BigInt(right))

  def reciprocalRaw(raw: Long): Long =
    if raw == 0L then 0L else ratioRaw(Scale, raw)

  def powIntRaw(base: Long, exponent: Int): Long =
    if exponent == 0 then Scale
    else if exponent < 0 then reciprocalRaw(powIntRaw(base, -exponent))
    else
      var result = Scale
      var factor = base
      var power  = exponent
      while power > 0 do
        if (power & 1) == 1 then result = multiplyRaw(result, factor)
        factor = multiplyRaw(factor, factor)
        power = power >> 1
      result

  def sqrtRaw(raw: Long): Long =
    if raw <= 0L then 0L
    else integerSqrt(BigInt(raw) * ScaleBig).toLong

  private def integerSqrt(n: BigInt): BigInt =
    if n <= 0 then BigInt(0)
    else
      var x = BigInt(1) << ((n.bitLength + 1) / 2)
      var y = (x + n / x) >> 1
      while y < x do
        x = y
        y = (x + n / x) >> 1
      x
