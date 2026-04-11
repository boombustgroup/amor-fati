package com.boombustgroup.amorfati.util

import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** Sampling helpers for standard distributions (Poisson, Beta, Gamma). */
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

  /** Sample from Poisson(lambda) using Knuth algorithm (small λ). */
  def poissonSample(lambda: Double, rng: RandomStream): Int =
    if lambda <= 0 then 0
    else
      val L = Math.exp(-lambda)
      var k = 0
      var p = rng.nextDouble()
      while p > L do
        k += 1
        p *= rng.nextDouble()
      k

  /** Sample from Beta(alpha, beta) using two Gamma samples. */
  def betaSample(alpha: Double, beta: Double, rng: RandomStream): Double =
    val x = gammaSample(alpha, rng)
    val y = gammaSample(beta, rng)
    if x + y > 0 then x / (x + y) else 0.5

  /** Sample from Gamma(shape, 1) using Marsaglia-Tsang method. */
  def gammaSample(shape: Double, rng: RandomStream): Double =
    if shape < 1.0 then gammaSample(shape + 1.0, rng) * Math.pow(rng.nextDouble(), 1.0 / shape)
    else
      val d      = shape - 1.0 / 3.0
      val c      = 1.0 / Math.sqrt(9.0 * d)
      var result = 0.0
      var done   = false
      while !done do
        var x = rng.nextGaussian()
        var v = 1.0 + c * x
        while v <= 0 do
          x = rng.nextGaussian()
          v = 1.0 + c * x
        v = v * v * v
        val u = rng.nextDouble()
        if u < 1.0 - 0.0331 * x * x * x * x then
          result = d * v
          done = true
        else if Math.log(u) < 0.5 * x * x + d * (1.0 - v + Math.log(v)) then
          result = d * v
          done = true
      result

  /** Sample a raw fixed-point Gaussian perturbation with dimensionless stddev.
    */
  def gaussianNoiseRaw(std: Scalar, rng: RandomStream): Long =
    math.round(rng.nextGaussian() * std.toLong)

  /** Sample a share around mean with Gaussian noise and clamp to bounds. */
  def gaussianShare(mean: Share, std: Scalar, lo: Share, hi: Share, rng: RandomStream): Share =
    Share.fromRaw((mean.toLong + gaussianNoiseRaw(std, rng)).max(lo.toLong).min(hi.toLong))

  /** Sample a PLN value around mean with Gaussian noise and clamp to floor. */
  def gaussianPlnAtLeast(mean: PLN, std: PLN, floor: PLN, rng: RandomStream): PLN =
    PLN.fromRaw((mean.toLong + math.round(std.toLong.toDouble * rng.nextGaussian())).max(floor.toLong))

  /** Sample a PLN value from a lognormal distribution. */
  def lognormalPln(mu: Double, sigma: Double, rng: RandomStream): PLN =
    PLN(Math.exp(mu + sigma * rng.nextGaussian()))

  /** Sample a PLN value uniformly from [0, maxExclusive). */
  def randomPlnBelow(maxExclusive: PLN, rng: RandomStream): PLN =
    PLN.fromRaw(rng.nextLong(maxExclusive.toLong.max(1L)))
