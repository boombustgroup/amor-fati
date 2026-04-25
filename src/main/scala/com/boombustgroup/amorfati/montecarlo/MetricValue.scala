package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.fp.FixedPointBase
import com.boombustgroup.amorfati.types.*

/** Fixed-point value used only at the Monte Carlo reporting boundary. */
opaque type MetricValue = Long

object MetricValue:
  val Zero: MetricValue = 0L
  val One: MetricValue  = FixedPointBase.Scale

  def apply(value: Int): MetricValue    = fromRaw(value.toLong * FixedPointBase.Scale)
  def apply(value: Long): MetricValue   = fromRaw(value * FixedPointBase.Scale)
  def apply(value: String): MetricValue = fromRaw(FixedPointBase.parseDecimal(value))
  def fromRaw(raw: Long): MetricValue   = raw

  def fraction(num: Int, den: Int): MetricValue =
    if den == 0 then Zero
    else fromRaw(FixedPointBase.ratioRaw(num.toLong * FixedPointBase.Scale, den.toLong * FixedPointBase.Scale))

  def secondsFromMillis(millis: Long): MetricValue =
    fromRaw(((BigInt(millis) * BigInt(FixedPointBase.Scale)) / BigInt(1000L)).toLong)

  def fromInt(value: Int): MetricValue         = MetricValue(value)
  def fromLong(value: Long): MetricValue       = MetricValue(value)
  def fromBoolean(value: Boolean): MetricValue = if value then One else Zero

  extension (value: MetricValue)
    inline def toLong: Long                      = value
    def +(other: MetricValue): MetricValue       = fromRaw(value + other)
    def -(other: MetricValue): MetricValue       = fromRaw(value - other)
    def *(other: MetricValue): MetricValue       = fromRaw(FixedPointBase.multiplyRaw(value, other))
    def *(n: Int): MetricValue                   = fromRaw(value * n.toLong)
    def /(n: Int): MetricValue                   = fromRaw(FixedPointBase.divideRaw(value, n.toLong))
    def ratioTo(other: MetricValue): MetricValue = fromRaw(FixedPointBase.ratioRaw(value, other))
    def format(fractionalDigits: Int): String    = FixedPointBase.format(value, fractionalDigits)
    def percent(fractionalDigits: Int): String   = (value * 100).format(fractionalDigits) + "%"
    def >(other: MetricValue): Boolean           = value > other
    def <(other: MetricValue): Boolean           = value < other
    def >=(other: MetricValue): Boolean          = value >= other
    def <=(other: MetricValue): Boolean          = value <= other

private[montecarlo] trait MetricEncoder[-A]:
  def encode(value: A): MetricValue

private[montecarlo] object MetricEncoder:
  given MetricEncoder[MetricValue] with
    def encode(value: MetricValue): MetricValue = value

  given MetricEncoder[PLN] with
    def encode(value: PLN): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Rate] with
    def encode(value: Rate): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Share] with
    def encode(value: Share): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Scalar] with
    def encode(value: Scalar): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Multiplier] with
    def encode(value: Multiplier): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Coefficient] with
    def encode(value: Coefficient): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[PriceIndex] with
    def encode(value: PriceIndex): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Sigma] with
    def encode(value: Sigma): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[ExchangeRate] with
    def encode(value: ExchangeRate): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[ExchangeRateShock] with
    def encode(value: ExchangeRateShock): MetricValue = MetricValue.fromRaw(value.toLong)

  given MetricEncoder[Int] with
    def encode(value: Int): MetricValue = MetricValue.fromInt(value)

  given MetricEncoder[Long] with
    def encode(value: Long): MetricValue = MetricValue.fromLong(value)

  given MetricEncoder[Boolean] with
    def encode(value: Boolean): MetricValue = MetricValue.fromBoolean(value)
