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

  /** Banker's rounding (half-even) for Long intermediate results. */
  def bankerRound(product: Long): Long =
    val quotient  = product / Scale
    val remainder = product % Scale
    val absRem    = if remainder < 0L then -remainder else remainder
    if absRem < HalfScale then quotient
    else
      val sign = if product < 0L then -1L else if product > 0L then 1L else 0L
      if absRem > HalfScale then quotient + sign
      else if quotient % 2L == 0L then quotient
      else quotient + sign

  /** Parse a decimal literal into the shared fixed-point raw scale. */
  def parseDecimal(value: String): Long =
    (BigDecimal(value.replace("_", "")) * ScaleDecimal).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLongExact

  /** Build an exact fixed-point raw value from decimal digits, e.g. (30, 2) =
    * 0.30.
    */
  def decimalRaw(unscaled: Long, fractionalDigits: Int): Long =
    require(fractionalDigits >= 0, s"fractionalDigits must be non-negative: $fractionalDigits")
    if fractionalDigits <= 4 then java.lang.Math.multiplyExact(unscaled, pow10(4 - fractionalDigits))
    else divideRaw(unscaled, pow10(fractionalDigits - 4))

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
    roundedDivRaw(dividend, divisor)

  /** Fixed-point division: `(numerator / denominator) * Scale`. */
  def ratioRaw(numerator: Long, denominator: Long): Long =
    if denominator == 0L then 0L
    else
      try roundedDivRaw(java.lang.Math.multiplyExact(numerator, Scale), denominator)
      catch case _: ArithmeticException => roundedDivBig(BigInt(numerator) * ScaleBig, BigInt(denominator))

  def multiplyRaw(left: Long, right: Long): Long =
    try bankerRound(java.lang.Math.multiplyExact(left, right))
    catch case _: ArithmeticException => bankerRound(BigInt(left) * BigInt(right))

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
    else
      try integerSqrtLong(java.lang.Math.multiplyExact(raw, Scale))
      catch case _: ArithmeticException => integerSqrt(BigInt(raw) * ScaleBig).toLong

  private def integerSqrtLong(n: Long): Long =
    var lo     = 1L
    var hi     = math.min(n, 3037000499L)
    var result = 0L
    while lo <= hi do
      val mid = (lo + hi) >>> 1
      if mid <= n / mid then
        result = mid
        lo = mid + 1L
      else hi = mid - 1L
    result

  private def integerSqrt(n: BigInt): BigInt =
    if n <= 0 then BigInt(0)
    else
      var x = BigInt(1) << ((n.bitLength + 1) / 2)
      var y = (x + n / x) >> 1
      while y < x do
        x = y
        y = (x + n / x) >> 1
      x

  private def pow10(exponent: Int): Long =
    require(exponent >= 0 && exponent <= 18, s"unsupported decimal exponent: $exponent")
    var result = 1L
    var i      = 0
    while i < exponent do
      result *= 10L
      i += 1
    result

  private def roundedDivRaw(dividend: Long, divisor: Long): Long =
    if divisor == 0L then 0L
    else if divisor == Long.MinValue || (dividend == Long.MinValue && divisor == -1L) then roundedDivBig(BigInt(dividend), BigInt(divisor))
    else
      val quotient     = dividend / divisor
      val remainder    = dividend % divisor
      val absRemainder = if remainder < 0L then -remainder else remainder
      val absDivisor   = if divisor < 0L then -divisor else divisor
      if absRemainder > Long.MaxValue / 2L then roundedDivBig(BigInt(dividend), BigInt(divisor))
      else
        val twiceRemainder = absRemainder * 2L
        if twiceRemainder < absDivisor then quotient
        else
          val resultSign = if (dividend < 0L) == (divisor < 0L) then 1L else -1L
          if twiceRemainder > absDivisor then quotient + resultSign
          else if quotient % 2L == 0L then quotient
          else quotient + resultSign

  private def roundedDivBig(dividend: BigInt, divisor: BigInt): Long =
    if divisor == 0 then 0L
    else
      val quotient       = dividend / divisor
      val remainder      = (dividend % divisor).abs
      val divisorAbs     = divisor.abs
      val twiceRemainder = remainder * 2
      if twiceRemainder < divisorAbs then quotient.toLong
      else
        val resultSign = dividend.signum * divisor.signum
        if twiceRemainder > divisorAbs then (quotient + resultSign).toLong
        else if quotient % 2 == 0 then quotient.toLong
        else (quotient + resultSign).toLong
