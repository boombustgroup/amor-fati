package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** CES elasticity of substitution (evolves via logistic dynamics). */
object SigmaProvider:
  opaque type Sigma = Long

  object Sigma:
    val Zero: Sigma                                           = 0L
    def apply(value: Int): Sigma                              = fromRaw(value.toLong * Scale)
    def apply(value: Long): Sigma                             = fromRaw(value * Scale)
    def decimal(unscaled: Long, fractionalDigits: Int): Sigma =
      fromRaw(decimalRaw(unscaled, fractionalDigits))
    def fromRaw(raw: Long): Sigma                             = raw

  extension (s: Sigma)
    inline def toLong: Long                   = s
    def +(other: Sigma): Sigma                = s + other
    def format(fractionalDigits: Int): String = FixedPointBase.format(s, fractionalDigits)
    def compact: String                       = FixedPointBase.formatCompact(s)
    def >(other: Sigma): Boolean              = s > other
    def <(other: Sigma): Boolean              = s < other
    def >=(other: Sigma): Boolean             = s >= other

  given Ordering[Sigma] = Ordering.Long
