package com.boombustgroup.amorfati.fp

import scala.annotation.targetName
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
  * `@boundaryEscape` to audit all escape points in the codebase.
  */
class boundaryEscape extends scala.annotation.StaticAnnotation

/** Explicit Double conversion — requires `import ComputationBoundary.toDouble`
  * and `@boundaryEscape` annotation on the enclosing function. No extension
  * methods — cannot be called as `value.toDouble`, only as `toDouble(value)`.
  * This makes every Double escape grep-able and code-review visible.
  */
object ComputationBoundary:
  @targetName("plnToDouble")
  def toDouble(p: PLN): Double         = asDouble(p.toLong)
  @targetName("rateToDouble")
  def toDouble(r: Rate): Double        = asDouble(r.toLong)
  @targetName("shareToDouble")
  def toDouble(s: Share): Double       = asDouble(s.toLong)
  @targetName("multiplierToDouble")
  def toDouble(m: Multiplier): Double  = asDouble(m.toLong)
  @targetName("coefficientToDouble")
  def toDouble(c: Coefficient): Double = asDouble(c.toLong)
  @targetName("priceIndexToDouble")
  def toDouble(p: PriceIndex): Double  = asDouble(p.toLong)
  @targetName("sigmaToDouble")
  def toDouble(s: Sigma): Double       = asDouble(s.toLong)
