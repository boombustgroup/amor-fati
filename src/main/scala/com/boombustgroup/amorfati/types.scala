package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.fp.FixedPointBase

import scala.annotation.targetName
import scala.util.Random

/** Fixed-point type system for SFC-ABM engine.
  *
  * All types are Long-based (scale 10^4). Each type is defined in an isolated
  * Provider object — inside that object, only its own opaque type is
  * transparent. Cross-type operations are defined here where ALL types are
  * opaque, so the compiler prevents PLN + Rate, Share + Coefficient, etc.
  */
object types:
  // Re-export all types and their companions
  export com.boombustgroup.amorfati.fp.EntityIds.*
  export com.boombustgroup.amorfati.fp.PLNProvider.{PLN, given}
  export com.boombustgroup.amorfati.fp.RateProvider.{Rate, given}
  export com.boombustgroup.amorfati.fp.ShareProvider.{Share, given}
  export com.boombustgroup.amorfati.fp.ScalarProvider.{Scalar, given}
  export com.boombustgroup.amorfati.fp.MultiplierProvider.{Multiplier, given}
  export com.boombustgroup.amorfati.fp.CoefficientProvider.{Coefficient, given}
  export com.boombustgroup.amorfati.fp.PriceIndexProvider.{PriceIndex, given}
  export com.boombustgroup.amorfati.fp.SigmaProvider.{Sigma, given}
  export com.boombustgroup.amorfati.fp.ExchangeRateProvider.{ExchangeRate, given}
  export com.boombustgroup.amorfati.fp.ExchangeRateShockProvider.{ExchangeRateShock, given}

  // Transitional compatibility only. Boundary escapes are being removed from
  // the typed algebra and should not be treated as part of the target API.
  export com.boombustgroup.amorfati.fp.{boundaryEscape, ComputationBoundary}

  import com.boombustgroup.amorfati.fp.FixedPointBase.bankerRound

  private inline def scaledDiv(numerator: Long, denominator: Long): Long =
    if denominator == 0L then 0L
    else
      val scaled         = BigInt(numerator) * BigInt(com.boombustgroup.amorfati.fp.FixedPointBase.Scale)
      val den            = BigInt(denominator)
      val quotient       = scaled / den
      val remainder      = (scaled % den).abs
      val denominatorAbs = den.abs
      val twiceRemainder = remainder * 2
      if twiceRemainder < denominatorAbs then quotient.toLong
      else
        val resultSign = scaled.signum * den.signum
        if twiceRemainder > denominatorAbs then (quotient + resultSign).toLong
        else if quotient % 2 == 0 then quotient.toLong
        else (quotient + resultSign).toLong

  // === Cross-type operations ===
  // Defined HERE where all types are opaque — compiler enforces type safety.
  // Each operation explicitly uses .toLong to access the raw value.

  // --- PLN × typed ---
  extension (p: PLN)
    @targetName("plnTimesInt")
    def *(n: Int): PLN              = PLN.fromRaw((BigInt(p.toLong) * BigInt(n.toLong)).toLong)
    @targetName("plnTimesLong")
    def *(n: Long): PLN             = PLN.fromRaw((BigInt(p.toLong) * BigInt(n)).toLong)
    @targetName("plnTimesRate")
    def *(r: Rate): PLN             = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(r.toLong)))
    @targetName("plnTimesShare")
    def *(s: Share): PLN            = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(s.toLong)))
    @targetName("plnTimesMultiplier")
    def *(m: Multiplier): PLN       = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(m.toLong)))
    @targetName("plnTimesCoefficient")
    def *(c: Coefficient): PLN      = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(c.toLong)))
    @targetName("plnTimesScalar")
    def *(s: Scalar): PLN           = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(s.toLong)))
    @targetName("plnDivShare")
    def /(s: Share): PLN            = PLN.fromRaw(scaledDiv(p.toLong, s.toLong))
    @targetName("plnDivMultiplier")
    def /(m: Multiplier): PLN       = PLN.fromRaw(scaledDiv(p.toLong, m.toLong))
    @targetName("plnDivInt")
    def /(n: Int): PLN              = PLN.fromRaw(FixedPointBase.divideRaw(p.toLong, n.toLong))
    @targetName("plnRatioToPln")
    def ratioTo(other: PLN): Scalar = Scalar.fromRaw(scaledDiv(p.toLong, other.toLong))

    /** Divides this amount by a count using [[FixedPointBase.divideRaw]] on raw
      * fixed-point values.
      *
      * `divideRaw` returns `0L` when `count` is zero, so `divideBy` gracefully
      * returns `PLN.fromRaw(0)` in that case. Callers should guard against a
      * zero divisor themselves if silent zero is not the desired behavior.
      */
    @targetName("plnDivideByCount")
    def divideBy(count: Int): PLN = PLN.fromRaw(FixedPointBase.divideRaw(p.toLong, count.toLong))
    @targetName("plnToDistributeRaw")
    def distributeRaw: Long       = p.toLong

  // --- Rate × typed ---
  extension (r: Rate)
    @targetName("rateTimesMultiplier")
    def *(m: Multiplier): Rate       = Rate.fromRaw(bankerRound(BigInt(r.toLong) * BigInt(m.toLong)))
    @targetName("rateTimesShare")
    def *(s: Share): Rate            = Rate.fromRaw(bankerRound(BigInt(r.toLong) * BigInt(s.toLong)))
    @targetName("rateTimesCoefficient")
    def *(c: Coefficient): Rate      = Rate.fromRaw(bankerRound(BigInt(r.toLong) * BigInt(c.toLong)))
    @targetName("rateApplyToCount")
    def applyTo(n: Int): Int         = bankerRound(BigInt(n.toLong) * BigInt(r.toLong)).toInt
    @targetName("rateToMultiplier")
    def toMultiplier: Multiplier     = Multiplier.fromRaw(r.toLong)
    @targetName("rateToScalar")
    def toScalar: Scalar             = Scalar.fromRaw(r.toLong)
    @targetName("rateToCoefficient")
    def toCoefficient: Coefficient   = Coefficient.fromRaw(r.toLong)
    @targetName("rateGrowthMultiplier")
    def growthMultiplier: Multiplier = Multiplier.fromRaw(FixedPointBase.Scale + r.toLong)
    @targetName("rateRatioToRate")
    def ratioTo(other: Rate): Scalar = Scalar.fromRaw(scaledDiv(r.toLong, other.toLong))

  // --- Share × typed ---
  extension (s: Share)
    @targetName("shareTimesMultiplier")
    def *(m: Multiplier): Multiplier   = Multiplier.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(m.toLong)))
    @targetName("shareTimesCoefficient")
    def *(c: Coefficient): Coefficient = Coefficient.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(c.toLong)))
    @targetName("shareTimesPln")
    def *(p: PLN): PLN                 = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(s.toLong)))
    @targetName("shareToRate")
    def toRate: Rate                   = Rate.fromRaw(s.toLong)
    @targetName("shareToMultiplier")
    def toMultiplier: Multiplier       = Multiplier.fromRaw(s.toLong)
    @targetName("shareToCoefficient")
    def toCoefficient: Coefficient     = Coefficient.fromRaw(s.toLong)
    @targetName("shareRatioToShare")
    def ratioTo(other: Share): Scalar  = Scalar.fromRaw(scaledDiv(s.toLong, other.toLong))
    @targetName("shareApplyToInt")
    def applyTo(n: Int): Int           = bankerRound(BigInt(n.toLong) * BigInt(s.toLong)).toInt
    @targetName("shareCeilApplyToInt")
    def ceilApplyTo(n: Int): Int       =
      val product = BigInt(n.toLong) * BigInt(s.toLong)
      val scale   = BigInt(FixedPointBase.Scale)
      if product >= 0 then ((product + scale - 1) / scale).toInt
      else (product / scale).toInt
    @targetName("shareToScalar")
    def toScalar: Scalar               = Scalar.fromRaw(s.toLong)
    @targetName("shareToMultiplierExact")
    def exactMultiplier: Multiplier    = Multiplier.fromRaw(s.toLong)
    @targetName("shareToComplement")
    def complement: Share              = Share.One - s
    @targetName("shareToDistributeRaw")
    def distributeRaw: Long            = s.toLong

  extension (s: Scalar)
    @targetName("scalarTimesPln")
    def *(p: PLN): PLN                 = PLN.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(p.toLong)))
    @targetName("scalarTimesRate")
    def *(r: Rate): Rate               = Rate.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(r.toLong)))
    @targetName("scalarTimesShare")
    def *(sh: Share): Share            = Share.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(sh.toLong)))
    @targetName("scalarTimesMultiplier")
    def *(m: Multiplier): Multiplier   = Multiplier.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(m.toLong)))
    @targetName("scalarTimesCoefficient")
    def *(c: Coefficient): Coefficient = Coefficient.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(c.toLong)))
    @targetName("scalarToShare")
    def toShare: Share                 = Share.fromRaw(s.toLong)
    @targetName("scalarToRate")
    def toRate: Rate                   = Rate.fromRaw(s.toLong)
    @targetName("scalarToMultiplier")
    def toMultiplier: Multiplier       = Multiplier.fromRaw(s.toLong)
    @targetName("scalarToCoefficient")
    def toCoefficient: Coefficient     = Coefficient.fromRaw(s.toLong)
    @targetName("scalarClampToShare")
    def clampToShare: Share            = Share.fromRaw(scala.math.max(Share.Zero.toLong, scala.math.min(Share.One.toLong, s.toLong)))

  // --- Multiplier × typed ---
  extension (m: Multiplier)
    @targetName("multTimesShare")
    def *(s: Share): Multiplier            = Multiplier.fromRaw(bankerRound(BigInt(m.toLong) * BigInt(s.toLong)))
    @targetName("multTimesPln")
    def *(p: PLN): PLN                     = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(m.toLong)))
    @targetName("multRatio")
    def ratioTo(other: Multiplier): Scalar = Scalar.fromRaw(scaledDiv(m.toLong, other.toLong))
    @targetName("multToRate")
    def toRate: Rate                       = Rate.fromRaw(m.toLong)
    @targetName("multToShare")
    def toShare: Share                     = Share.fromRaw(m.toLong)
    @targetName("multToCoefficient")
    def toCoefficient: Coefficient         = Coefficient.fromRaw(m.toLong)
    @targetName("multToScalar")
    def toScalar: Scalar                   = Scalar.fromRaw(m.toLong)
    @targetName("multToPriceIndex")
    def toPriceIndex: PriceIndex           = PriceIndex.fromRaw(m.toLong)
    @targetName("multDeviationFromOne")
    def deviationFromOne: Coefficient      = Coefficient.fromRaw(m.toLong - Multiplier.One.toLong)
    @targetName("multDivideByInt")
    def divideBy(n: Int): Multiplier       = Multiplier.fromRaw(FixedPointBase.divideRaw(m.toLong, n.toLong))
    @targetName("multApplyToCount")
    def applyTo(n: Int): Int               = bankerRound(BigInt(n.toLong) * BigInt(m.toLong)).toInt

  // --- Coefficient × typed ---
  extension (c: Coefficient)
    @targetName("coefTimesShare")
    def *(s: Share): Coefficient                             = Coefficient.fromRaw(bankerRound(BigInt(c.toLong) * BigInt(s.toLong)))
    @targetName("coefTimesMultiplier")
    def *(m: Multiplier): Share                              = Share.fromRaw(bankerRound(BigInt(c.toLong) * BigInt(m.toLong)))
    @targetName("coefTimesPln")
    def *(p: PLN): PLN                                       = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(c.toLong)))
    @targetName("coefToShare")
    def toShare: Share                                       = Share.fromRaw(c.toLong)
    @targetName("coefToMultiplier")
    def toMultiplier: Multiplier                             = Multiplier.fromRaw(c.toLong)
    @targetName("coefToScalar")
    def toScalar: Scalar                                     = Scalar.fromRaw(c.toLong)
    @targetName("coefToRate")
    def toRate: Rate                                         = Rate.fromRaw(c.toLong)
    @targetName("coefToExchangeRateShock")
    def toExchangeRateShock: ExchangeRateShock               = ExchangeRateShock.fromRaw(c.toLong)
    @targetName("coefDivInt")
    def /(n: Int): Coefficient                               = Coefficient.fromRaw(FixedPointBase.divideRaw(c.toLong, n.toLong))
    @targetName("coefGrowthMultiplier")
    def growthMultiplier: Multiplier                         = Multiplier.fromRaw(FixedPointBase.Scale + c.toLong)
    @targetName("coefClamp")
    def clamp(lo: Coefficient, hi: Coefficient): Coefficient =
      require(lo.toLong <= hi.toLong, s"Coefficient.clamp requires lo <= hi, got lo=$lo hi=$hi")
      Coefficient.fromRaw(scala.math.max(lo.toLong, scala.math.min(hi.toLong, c.toLong)))

  // --- PriceIndex × typed ---
  extension (pi: PriceIndex)
    @targetName("priceIdxTimesRate")
    def *(r: Rate): PriceIndex                       = PriceIndex.fromRaw(bankerRound(BigInt(pi.toLong) * BigInt(r.toLong)))
    @targetName("priceIdxDivRate")
    def /(r: Rate): PriceIndex                       = PriceIndex.fromRaw(scaledDiv(pi.toLong, r.toLong))
    @targetName("priceIdxTimesMultiplier")
    def *(m: Multiplier): PriceIndex                 = PriceIndex.fromRaw(bankerRound(BigInt(pi.toLong) * BigInt(m.toLong)))
    @targetName("priceIdxTimesPln")
    def *(p: PLN): PLN                               = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(pi.toLong)))
    @targetName("priceIdxToMultiplier")
    def toMultiplier: Multiplier                     = Multiplier.fromRaw(pi.toLong)
    @targetName("priceIdxRatioToPriceIdx")
    def ratioTo(other: PriceIndex): Scalar           = Scalar.fromRaw(scaledDiv(pi.toLong, other.toLong))
    @targetName("priceIdxApplyGrowth")
    def applyGrowth(growth: Coefficient): PriceIndex = PriceIndex.fromRaw(bankerRound(BigInt(pi.toLong) * BigInt(FixedPointBase.Scale + growth.toLong)))

  extension (s: Sigma)
    @targetName("sigmaToScalar")
    def toScalar: Scalar           = Scalar.fromRaw(s.toLong)
    @targetName("sigmaToCoefficient")
    def toCoefficient: Coefficient = Coefficient.fromRaw(s.toLong)

  // --- ExchangeRate / ExchangeRateShock ---
  extension (rate: ExchangeRate)
    @targetName("exchangeRateDeviationFrom")
    def deviationFrom(base: ExchangeRate): ExchangeRateShock = ExchangeRateShock.fromRaw(scaledDiv(rate.toLong - base.toLong, base.toLong))
    @targetName("exchangeRateRatioTo")
    def ratioTo(base: ExchangeRate): Multiplier              = Multiplier.fromRaw(scaledDiv(rate.toLong, base.toLong))
    @targetName("exchangeRateApplyShock")
    def applyShock(shock: ExchangeRateShock): ExchangeRate   =
      ExchangeRate.fromRaw(bankerRound(BigInt(rate.toLong) * BigInt(FixedPointBase.Scale + shock.toLong)))

  extension (shock: ExchangeRateShock)
    @targetName("exchangeRateShockToScalar")
    def toScalar: Scalar           = Scalar.fromRaw(shock.toLong)
    @targetName("exchangeRateShockToCoefficient")
    def toCoefficient: Coefficient = Coefficient.fromRaw(shock.toLong)

  extension (n: Int)
    @targetName("intRatioToInt")
    def ratioTo(denominator: Int): Scalar = Scalar.fromRaw(scaledDiv(n.toLong, denominator.toLong))

  object TypedRandom:
    @targetName("randomShareBetween")
    def randomBetween(lo: Share, hi: Share, rng: Random): Share =
      if hi < lo then throw IllegalArgumentException(s"Share.randomBetween requires lo <= hi, got lo=$lo hi=$hi")
      else if hi == lo then lo
      else Share.fromRaw(rng.between(lo.toLong, hi.toLong))

    def withGaussianNoise(base: Share, stddev: Share, rng: Random): Share =
      val raw        = base.toLong + scala.math.round(rng.nextGaussian() * stddev.toLong)
      val clampedRaw = scala.math.max(Share.Zero.toLong, scala.math.min(Share.One.toLong, raw))
      Share.fromRaw(clampedRaw)

    @targetName("randomMultiplierBetween")
    def randomBetween(lo: Multiplier, hi: Multiplier, rng: Random): Multiplier =
      if hi < lo then throw IllegalArgumentException(s"Multiplier.randomBetween requires lo <= hi, got lo=$lo hi=$hi")
      else if hi == lo then lo
      else Multiplier.fromRaw(rng.between(lo.toLong, hi.toLong))

  object WeightedSelection:
    def choose(weights: Vector[Multiplier], rng: Random): Int =
      if weights.isEmpty then return -1
      weights.foreach: weight =>
        if weight < Multiplier.Zero then throw IllegalArgumentException(s"WeightedSelection requires non-negative weights, got: $weight")
      val total = weights.foldLeft(0L)(_ + _.toLong)
      if total == 0L then throw IllegalArgumentException("WeightedSelection requires at least one positive weight")
      else
        val threshold = rng.between(0L, total)
        weights.indices
          .foldLeft((-1, 0L)) { case ((picked, cumulative), idx) =>
            if picked >= 0 then (picked, cumulative)
            else
              val next = cumulative + weights(idx).toLong
              if next > threshold then (idx, next) else (-1, next)
          } match
          case (idx, _) if idx >= 0 => idx
          case _                    => weights.length - 1
