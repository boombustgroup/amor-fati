package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.fp.*
import com.boombustgroup.amorfati.montecarlo.MetricValue
import com.boombustgroup.amorfati.types.*
import org.scalacheck.Gen
import scala.annotation.targetName

object FixedPointSpecSupport:
  private val ScaleBD = BigDecimal(FixedPointBase.Scale)
  private val Two     = BigDecimal(2)
  private val Tau     = BigDecimal("6.2831853071795864769252867665590058")

  def rawValue(raw: Long): BigDecimal =
    BigDecimal(raw) / ScaleBD

  @targetName("intDecimal")
  def decimal(value: Int): BigDecimal  = BigDecimal(value)
  @targetName("longDecimal")
  def decimal(value: Long): BigDecimal = BigDecimal(value)

  @targetName("plnDecimal")
  def decimal(value: PLN): BigDecimal               = rawValue(value.toLong)
  @targetName("rateDecimal")
  def decimal(value: Rate): BigDecimal              = rawValue(value.toLong)
  @targetName("shareDecimal")
  def decimal(value: Share): BigDecimal             = rawValue(value.toLong)
  @targetName("scalarDecimal")
  def decimal(value: Scalar): BigDecimal            = rawValue(value.toLong)
  @targetName("multiplierDecimal")
  def decimal(value: Multiplier): BigDecimal        = rawValue(value.toLong)
  @targetName("coefficientDecimal")
  def decimal(value: Coefficient): BigDecimal       = rawValue(value.toLong)
  @targetName("priceIndexDecimal")
  def decimal(value: PriceIndex): BigDecimal        = rawValue(value.toLong)
  @targetName("sigmaDecimal")
  def decimal(value: Sigma): BigDecimal             = rawValue(value.toLong)
  @targetName("exchangeRateDecimal")
  def decimal(value: ExchangeRate): BigDecimal      = rawValue(value.toLong)
  @targetName("exchangeRateShockDecimal")
  def decimal(value: ExchangeRateShock): BigDecimal = rawValue(value.toLong)
  @targetName("metricValueDecimal")
  def decimal(value: MetricValue): BigDecimal       = rawValue(value.toLong)

  def genDecimal(lo: String, hi: String): Gen[BigDecimal] =
    Gen
      .choose(FixedPointBase.parseDecimal(lo), FixedPointBase.parseDecimal(hi))
      .map(rawValue)

  def genDecimalRange(lo: BigDecimal, hi: BigDecimal): Gen[BigDecimal] =
    Gen
      .choose(FixedPointBase.fromDecimal(lo), FixedPointBase.fromDecimal(hi))
      .map(rawValue)

  def fractionDecimal(numerator: Int, denominator: Int): BigDecimal =
    if denominator == 0 then BigDecimal(0)
    else BigDecimal(numerator) / BigDecimal(denominator)

  def powDecimal(base: BigDecimal, exponent: Int): BigDecimal =
    if exponent == 0 then BigDecimal(1)
    else if exponent < 0 then BigDecimal(1) / powDecimal(base, -exponent)
    else (0 until exponent).foldLeft(BigDecimal(1))((acc, _) => acc * base)

  def powDecimal(base: BigDecimal, exponent: BigDecimal): BigDecimal =
    rawValue(FixedPointMath.powRaw(FixedPointBase.fromDecimal(base), FixedPointBase.fromDecimal(exponent)))

  def cosTurns(numerator: Int, denominator: Int): BigDecimal =
    rawValue(FixedPointMath.cosRaw(FixedPointBase.fromDecimal(Tau * BigDecimal(numerator) / BigDecimal(denominator))))

  def sqrtDecimal(value: BigDecimal, scale: Int = 12): BigDecimal =
    if value <= 0 then BigDecimal(0)
    else
      var x = value
      var i = 0
      while i < 32 do
        x = (x + value / x) / Two
        i += 1
      x.setScale(scale, BigDecimal.RoundingMode.HALF_EVEN)

  def roundedLong(value: BigDecimal): Long =
    value.setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong

  object DecimalMath:
    def max[A](left: A, right: A)(using ordering: Ordering[A]): A =
      if ordering.gteq(left, right) then left else right

    def min[A](left: A, right: A)(using ordering: Ordering[A]): A =
      if ordering.lteq(left, right) then left else right

    def abs(value: BigDecimal): BigDecimal =
      value.abs

    def pow(base: BigDecimal, exponent: BigDecimal): BigDecimal =
      powDecimal(base, exponent)

    def pow(base: BigDecimal, exponent: Int): BigDecimal =
      powDecimal(base, exponent)

    def round(value: BigDecimal): Long =
      roundedLong(value)

  extension (value: BigDecimal)
    def isFinite: Boolean = true
    def isNaN: Boolean    = false

  extension (p: PLN) @targetName("plnBd") def bd: BigDecimal                                 = rawValue(p.toLong)
  extension (r: Rate) @targetName("rateBd") def bd: BigDecimal                               = rawValue(r.toLong)
  extension (s: Share) @targetName("shareBd") def bd: BigDecimal                             = rawValue(s.toLong)
  extension (s: Scalar) @targetName("scalarBd") def bd: BigDecimal                           = rawValue(s.toLong)
  extension (m: Multiplier) @targetName("multiplierBd") def bd: BigDecimal                   = rawValue(m.toLong)
  extension (c: Coefficient) @targetName("coefficientBd") def bd: BigDecimal                 = rawValue(c.toLong)
  extension (pi: PriceIndex) @targetName("priceIndexBd") def bd: BigDecimal                  = rawValue(pi.toLong)
  extension (s: Sigma) @targetName("sigmaBd") def bd: BigDecimal                             = rawValue(s.toLong)
  extension (er: ExchangeRate) @targetName("exchangeRateBd") def bd: BigDecimal              = rawValue(er.toLong)
  extension (shock: ExchangeRateShock) @targetName("exchangeRateShockBd") def bd: BigDecimal = rawValue(shock.toLong)
