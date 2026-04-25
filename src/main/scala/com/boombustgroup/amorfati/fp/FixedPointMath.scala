package com.boombustgroup.amorfati.fp

/** Deterministic fixed-point transcendental helpers.
  *
  * All public methods accept and return raw values at [[FixedPointBase.Scale]].
  */
object FixedPointMath:
  private val Zero = BigDecimal(0)
  private val One  = BigDecimal(1)
  private val Two  = BigDecimal(2)

  private val Pi  = BigDecimal("3.1415926535897932384626433832795029")
  private val Tau = Pi * Two
  private val Ln2 = BigDecimal("0.6931471805599453094172321214581766")

  def expRaw(raw: Long): Long =
    FixedPointBase.fromDecimal(exp(toDecimal(raw)))

  def lnRaw(raw: Long): Long =
    if raw <= 0L then 0L else FixedPointBase.fromDecimal(ln(toDecimal(raw)))

  def powRaw(baseRaw: Long, exponentRaw: Long): Long =
    if exponentRaw == 0L then FixedPointBase.Scale
    else if baseRaw <= 0L then 0L
    else
      val exponent = toDecimal(exponentRaw)
      FixedPointBase.fromDecimal(exp(ln(toDecimal(baseRaw)) * exponent))

  def cosRaw(rawRadians: Long): Long =
    FixedPointBase.fromDecimal(cos(toDecimal(rawRadians)))

  private def toDecimal(raw: Long): BigDecimal =
    BigDecimal(raw) / FixedPointBase.ScaleDecimal

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
