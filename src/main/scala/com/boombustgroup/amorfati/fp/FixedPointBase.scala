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

  /** Internal: raw Long → real Double. NOT public API. */
  inline def asDouble(raw: Long): Double = raw.toDouble / ScaleD
