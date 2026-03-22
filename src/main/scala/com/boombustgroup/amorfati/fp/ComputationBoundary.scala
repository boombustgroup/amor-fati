package com.boombustgroup.amorfati.fp

import FixedPointBase.asDouble
import PLNProvider.PLN
import RateProvider.Rate
import ShareProvider.Share
import MultiplierProvider.Multiplier
import CoefficientProvider.Coefficient
import PriceIndexProvider.PriceIndex
import SigmaProvider.Sigma

/** Marker annotation: this function deliberately converts fixed-point to Double
  * at a computation boundary (CES, Math.pow, CSV output). Grep for
  * `@computationBoundary` to audit all escape points in the codebase.
  */
class computationBoundary extends scala.annotation.StaticAnnotation

/** Explicit Double conversion — requires `import ComputationBoundary.toDouble`
  * and `@computationBoundary` annotation on the enclosing function. No
  * extension methods — cannot be called as `value.toDouble`, only as
  * `toDouble(value)`. This makes every Double escape grep-able and
  * code-review visible.
  */
object ComputationBoundary:
  def toDouble(p: PLN): Double         = asDouble(p.toLong)
  def toDouble(r: Rate): Double        = asDouble(r.toLong)
  def toDouble(s: Share): Double       = asDouble(s.toLong)
  def toDouble(m: Multiplier): Double  = asDouble(m.toLong)
  def toDouble(c: Coefficient): Double = asDouble(c.toLong)
  def toDouble(p: PriceIndex): Double  = asDouble(p.toLong)
  def toDouble(s: Sigma): Double       = asDouble(s.toLong)
