package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** CES elasticity of substitution (evolves via logistic dynamics). */
object SigmaProvider:
  opaque type Sigma = Long

  object Sigma:
    val Zero: Sigma             = 0L
    def apply(d: Double): Sigma = Math.round(d * Scale)

  extension (s: Sigma)
    inline def toLong: Long           = s
    def +(other: Sigma): Sigma        = s + other
    def >(other: Sigma): Boolean      = s > other
    def <(other: Sigma): Boolean      = s < other
    def >=(other: Sigma): Boolean     = s >= other

  given Ordering[Sigma] = Ordering.Long
