package com.boombustgroup.amorfati.util

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.fp.FixedPointBase

import com.boombustgroup.amorfati.random.RandomStream

/** Sampling helpers backed by fixed-point arithmetic. */
object Distributions:

  /** Sample a categorical index from non-negative fixed-point weights. */
  private[amorfati] def cdfSample(shares: Vector[Share], rng: RandomStream): Int =
    require(shares.nonEmpty, "shares must be non-empty")
    require(shares.forall(_ >= Share.Zero), s"shares must be non-negative: $shares")
    val total = shares.foldLeft(Share.Zero)(_ + _)
    require(total > Share.Zero, s"shares must contain at least one positive weight: $shares")

    val draw       = Share.random(rng) * total
    var cumulative = Share.Zero
    var i          = 0
    while i < shares.length - 1 do
      cumulative = cumulative + shares(i)
      if draw < cumulative then return i
      i += 1
    shares.length - 1

  /** Sample a share uniformly from [lo, hi]. */
  def randomShareBetween(lo: Share, hi: Share, rng: RandomStream): Share =
    if hi <= lo then lo
    else Share.fromRaw(rng.between(lo.toLong, hi.toLong))

  /** Sample from Poisson(lambda) using Knuth algorithm (small lambda). */
  def poissonSample(lambda: Scalar, rng: RandomStream): Int =
    if lambda <= Scalar.Zero then 0
    else
      val threshold = (-lambda.toCoefficient).exp.toLong
      var k         = 0
      var product   = Share.random(rng).toLong
      while product > threshold do
        k += 1
        product = FixedPointBase.multiplyRaw(product, Share.random(rng).toLong)
      k

  /** Beta-like share around alpha/(alpha+beta), with fixed-point bounded noise.
    */
  def betaSample(alpha: Coefficient, beta: Coefficient, rng: RandomStream): Share =
    val total = alpha + beta
    if total <= Coefficient.Zero then Share("0.5")
    else
      val mean       = alpha.toScalar.ratioTo(total.toScalar).clampToShare
      val maxStd     = mean.min(mean.complement)
      val noiseScale = maxStd.toScalar / 3
      Share.fromRaw((mean.toLong + gaussianNoiseRaw(noiseScale, rng)).max(Share.Zero.toLong).min(Share.One.toLong))

  /** Sample a raw fixed-point bell-shaped perturbation with dimensionless
    * stddev.
    */
  def gaussianNoiseRaw(std: Scalar, rng: RandomStream): Long =
    FixedPointBase.multiplyRaw(normalCoefficient(rng).toLong, std.toLong)

  private def normalCoefficient(rng: RandomStream): Coefficient =
    var sum = 0L
    var i   = 0
    while i < 12 do
      sum += Share.random(rng).toLong
      i += 1
    Coefficient.fromRaw(sum - 6L * FixedPointBase.Scale)

  /** Sample a share around mean with Gaussian noise and clamp to bounds. */
  def gaussianShare(mean: Share, std: Scalar, lo: Share, hi: Share, rng: RandomStream): Share =
    Share.fromRaw((mean.toLong + gaussianNoiseRaw(std, rng)).max(lo.toLong).min(hi.toLong))

  /** Sample a PLN value around mean with Gaussian noise and clamp to floor. */
  def gaussianPlnAtLeast(mean: PLN, std: PLN, floor: PLN, rng: RandomStream): PLN =
    PLN.fromRaw((mean + (std * normalCoefficient(rng))).max(floor).toLong)

  /** Sample a PLN value from a lognormal distribution. */
  def lognormalPln(mu: Coefficient, sigma: Coefficient, rng: RandomStream): PLN =
    PLN.fromRaw((mu + (sigma * normalCoefficient(rng))).exp.toLong)

  /** Sample a PLN value uniformly from [0, maxExclusive). */
  def randomPlnBelow(maxExclusive: PLN, rng: RandomStream): PLN =
    PLN.fromRaw(rng.nextLong(maxExclusive.toLong.max(1L)))
