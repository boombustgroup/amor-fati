package com.boombustgroup.amorfati.fp

/** Deterministic fixed-point transcendental helpers.
  *
  * All public methods accept and return raw values at [[FixedPointBase.Scale]].
  */
object FixedPointMath:
  private val Zero = BigDecimal(0)
  private val One  = BigDecimal(1)
  private val Two  = BigDecimal(2)

  private val Pi      = BigDecimal("3.1415926535897932384626433832795029")
  private val Tau     = Pi * Two
  private val Ln2     = BigDecimal("0.6931471805599453094172321214581766")
  private val Ln2Raw  = FixedPointBase.parseDecimal("0.6931471805599453094172321214581766")
  private val ExpCap  = FixedPointBase.parseDecimal("20.0")
  private val LnCache = java.util.concurrent.ConcurrentHashMap[Long, java.lang.Long]()

  def expRaw(raw: Long): Long =
    try expRawSeries(raw)
    catch case _: ArithmeticException => FixedPointBase.fromDecimal(exp(toDecimal(raw)))

  def lnRaw(raw: Long): Long =
    if raw <= 0L then 0L
    else
      val cached = LnCache.get(raw)
      if cached != null then cached.longValue
      else
        val computed =
          try lnRawSeries(raw)
          catch case _: ArithmeticException => FixedPointBase.fromDecimal(ln(toDecimal(raw)))
        val existing = LnCache.putIfAbsent(raw, computed)
        if existing == null then computed else existing.longValue

  def powRaw(baseRaw: Long, exponentRaw: Long): Long =
    if exponentRaw == 0L then FixedPointBase.Scale
    else if baseRaw <= 0L then 0L
    else if exponentRaw == FixedPointBase.Scale then baseRaw
    else if exponentRaw == -FixedPointBase.Scale then FixedPointBase.reciprocalRaw(baseRaw)
    else if exponentRaw == FixedPointBase.Scale / 2L then FixedPointBase.sqrtRaw(baseRaw)
    else if exponentRaw == -FixedPointBase.Scale / 2L then FixedPointBase.reciprocalRaw(FixedPointBase.sqrtRaw(baseRaw))
    else if exponentRaw % FixedPointBase.Scale == 0L && exponentRaw / FixedPointBase.Scale >= Int.MinValue && exponentRaw / FixedPointBase.Scale <= Int.MaxValue
    then FixedPointBase.powIntRaw(baseRaw, (exponentRaw / FixedPointBase.Scale).toInt)
    else
      try expRawSeries(FixedPointBase.multiplyRaw(lnRawSeries(baseRaw), exponentRaw))
      catch
        case _: ArithmeticException =>
          val exponent = toDecimal(exponentRaw)
          FixedPointBase.fromDecimal(exp(ln(toDecimal(baseRaw)) * exponent))

  def cosRaw(rawRadians: Long): Long =
    FixedPointBase.fromDecimal(cos(toDecimal(rawRadians)))

  private def toDecimal(raw: Long): BigDecimal =
    BigDecimal(raw) / FixedPointBase.ScaleDecimal

  private def lnRawSeries(raw: Long): Long =
    var x = raw
    var k = 0L
    while x > FixedPointBase.Scale + FixedPointBase.Scale / 2L do
      x = FixedPointBase.divideRaw(x, 2L)
      k += 1L
    while x < (FixedPointBase.Scale * 3L) / 4L do
      x = java.lang.Math.multiplyExact(x, 2L)
      k -= 1L

    val z    = FixedPointBase.ratioRaw(x - FixedPointBase.Scale, x + FixedPointBase.Scale)
    val z2   = FixedPointBase.multiplyRaw(z, z)
    var term = z
    var sum  = 0L
    var n    = 1
    while n <= 31 do
      sum = java.lang.Math.addExact(sum, FixedPointBase.divideRaw(term, n.toLong))
      term = FixedPointBase.multiplyRaw(term, z2)
      n += 2
    java.lang.Math.addExact(java.lang.Math.multiplyExact(sum, 2L), java.lang.Math.multiplyExact(k, Ln2Raw))

  private def expRawSeries(raw: Long): Long =
    if raw > ExpCap || raw < -ExpCap then throw new ArithmeticException("fixed-point exp out of fast range")

    var x       = raw
    var k       = 0
    val halfLn2 = Ln2Raw / 2L
    while x > halfLn2 do
      x -= Ln2Raw
      k += 1
    while x < -halfLn2 do
      x += Ln2Raw
      k -= 1

    var term = FixedPointBase.Scale
    var sum  = FixedPointBase.Scale
    var n    = 1
    while n <= 24 do
      term = FixedPointBase.divideRaw(FixedPointBase.multiplyRaw(term, x), n.toLong)
      sum = java.lang.Math.addExact(sum, term)
      n += 1

    if k >= 0 then java.lang.Math.multiplyExact(sum, 1L << k)
    else FixedPointBase.divideRaw(sum, 1L << -k)

  private def exp(value: BigDecimal): BigDecimal =
    var x = value
    var k = 0
    while x > Ln2 do
      x -= Ln2
      k += 1
    while x < -Ln2 do
      x += Ln2
      k -= 1

    var term = One
    var sum  = One
    var n    = 1
    while n <= 48 do
      term = (term * x) / BigDecimal(n)
      sum += term
      n += 1

    if k >= 0 then sum * BigDecimal(BigInt(2).pow(k))
    else sum / BigDecimal(BigInt(2).pow(-k))

  private def ln(value: BigDecimal): BigDecimal =
    var x = value
    var k = 0
    while x > BigDecimal("1.5") do
      x /= Two
      k += 1
    while x < BigDecimal("0.75") do
      x *= Two
      k -= 1

    val z    = (x - One) / (x + One)
    val z2   = z * z
    var term = z
    var sum  = Zero
    var n    = 1
    while n <= 121 do
      sum += term / BigDecimal(n)
      term *= z2
      n += 2
    sum * Two + BigDecimal(k) * Ln2

  private def cos(value: BigDecimal): BigDecimal =
    var x = value.remainder(Tau)
    if x > Pi then x -= Tau
    if x < -Pi then x += Tau

    val x2   = x * x
    var term = One
    var sum  = One
    var n    = 1
    while n <= 18 do
      val a = BigDecimal(2 * n - 1)
      val b = BigDecimal(2 * n)
      term = -term * x2 / (a * b)
      sum += term
      n += 1
    sum
