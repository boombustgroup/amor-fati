package com.boombustgroup.amorfati.fp

/** Shared constants and helpers for all fixed-point types. */
object FixedPointBase:
  val Scale: Long     = 10_000L
  val HalfScale: Long = 5_000L
  val ScaleD: Double  = Scale.toDouble

  /** Banker's rounding (half-even) for BigInt intermediate results. */
  def bankerRound(product: BigInt): Long =
    val quotient  = product / Scale
    val remainder = (product % Scale).abs.toLong
    if remainder < HalfScale then quotient.toLong
    else if remainder > HalfScale then (quotient + product.signum).toLong
    else if quotient % 2 == 0 then quotient.toLong
    else (quotient + product.signum).toLong

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
