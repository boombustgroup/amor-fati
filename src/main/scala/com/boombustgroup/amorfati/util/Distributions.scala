package com.boombustgroup.amorfati.util

import scala.util.Random

/** Sampling helpers for standard distributions (Poisson, Beta, Gamma). */
object Distributions:

  /** Sample from Poisson(lambda) using Knuth algorithm (small λ). */
  def poissonSample(lambda: Double, rng: Random): Int =
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
  def betaSample(alpha: Double, beta: Double, rng: Random): Double =
    val x = gammaSample(alpha, rng)
    val y = gammaSample(beta, rng)
    if x + y > 0 then x / (x + y) else 0.5

  /** Sample from Gamma(shape, 1) using Marsaglia-Tsang method. */
  def gammaSample(shape: Double, rng: Random): Double =
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
