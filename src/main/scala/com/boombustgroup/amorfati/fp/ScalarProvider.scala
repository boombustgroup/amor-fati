package com.boombustgroup.amorfati.fp

import FixedPointBase.*

/** Dimensionless scalar used for algebraic composition and ratios. */
object ScalarProvider:
  opaque type Scalar = Long

  object Scalar:
    val Zero: Scalar                                                          = 0L
    val One: Scalar                                                           = Scale
    def apply(d: Double): Scalar                                              = Math.round(d * Scale)
    def fromRaw(raw: Long): Scalar                                            = raw
    def fraction(num: Int, den: Int): Scalar                                  =
      if den == 0 then Zero
      else
        val scaled         = BigInt(num.toLong) * BigInt(Scale)
        val denominator    = BigInt(den.toLong)
        val quotient       = scaled / denominator
        val remainder      = (scaled % denominator).abs
        val denominatorAbs = denominator.abs
        val twiceRemainder = remainder * 2
        if twiceRemainder < denominatorAbs then quotient.toLong
        else
          val resultSign = scaled.signum * denominator.signum
          if twiceRemainder > denominatorAbs then (quotient + resultSign).toLong
          else if quotient % 2 == 0 then quotient.toLong
          else (quotient + resultSign).toLong
    def randomBetween(lo: Scalar, hi: Scalar, rng: scala.util.Random): Scalar =
      if hi <= lo then lo
      else Scalar.fromRaw(rng.between(lo.toLong, hi.toLong))

  extension (s: Scalar)
    inline def toLong: Long                   = s
    def unary_- : Scalar                      = -s
    def abs: Scalar                           = math.abs(s)
    def +(other: Scalar): Scalar              = s + other
    def -(other: Scalar): Scalar              = s - other
    def *(other: Scalar): Scalar              = bankerRound(BigInt(s) * BigInt(other))
    def /(n: Int): Scalar                     = Scalar.fromRaw(s / n.toLong)
    def ratioTo(other: Scalar): Scalar        =
      if other == 0L then Scalar.Zero
      else
        val scaled         = BigInt(s) * BigInt(Scale)
        val denominator    = BigInt(other)
        val quotient       = scaled / denominator
        val remainder      = (scaled % denominator).abs
        val denominatorAbs = denominator.abs
        val twiceRemainder = remainder * 2
        if twiceRemainder < denominatorAbs then Scalar.fromRaw(quotient.toLong)
        else
          val resultSign = scaled.signum * denominator.signum
          if twiceRemainder > denominatorAbs then Scalar.fromRaw((quotient + resultSign).toLong)
          else if quotient % 2 == 0 then Scalar.fromRaw(quotient.toLong)
          else Scalar.fromRaw((quotient + resultSign).toLong)
    def reciprocal: Scalar                    =
      if s == 0L then Scalar.Zero
      else
        val scaled         = BigInt(Scale) * BigInt(Scale)
        val denominator    = BigInt(s)
        val quotient       = scaled / denominator
        val remainder      = (scaled % denominator).abs
        val denominatorAbs = denominator.abs
        val twiceRemainder = remainder * 2
        if twiceRemainder < denominatorAbs then Scalar.fromRaw(quotient.toLong)
        else
          val resultSign = scaled.signum * denominator.signum
          if twiceRemainder > denominatorAbs then Scalar.fromRaw((quotient + resultSign).toLong)
          else if quotient % 2 == 0 then Scalar.fromRaw(quotient.toLong)
          else Scalar.fromRaw((quotient + resultSign).toLong)
    def pow(exponent: Int): Scalar            = Scalar(math.pow(s.toDouble / ScaleD, exponent.toDouble))
    def pow(exponent: Scalar): Scalar         = Scalar(math.pow(s.toDouble / ScaleD, exponent.toLong.toDouble / ScaleD))
    def log10: Scalar                         =
      if s <= 0L then Scalar.Zero
      else Scalar(math.log10(s.toDouble / ScaleD))
    def max(other: Scalar): Scalar            = math.max(s, other)
    def min(other: Scalar): Scalar            = math.min(s, other)
    def clamp(lo: Scalar, hi: Scalar): Scalar = math.max(lo, math.min(hi, s))
    def >(other: Scalar): Boolean             = s > other
    def <(other: Scalar): Boolean             = s < other
    def >=(other: Scalar): Boolean            = s >= other
    def <=(other: Scalar): Boolean            = s <= other

  given Ordering[Scalar] = Ordering.Long
