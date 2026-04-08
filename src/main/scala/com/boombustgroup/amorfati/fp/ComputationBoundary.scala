package com.boombustgroup.amorfati.fp

import scala.annotation.targetName
import FixedPointBase.ScaleD
import PLNProvider.PLN
import RateProvider.Rate
import ShareProvider.Share
import ScalarProvider.Scalar
import MultiplierProvider.Multiplier
import CoefficientProvider.Coefficient
import PriceIndexProvider.PriceIndex
import SigmaProvider.Sigma
import ExchangeRateProvider.ExchangeRate

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
  def toDouble(p: PLN): Double           = p.toLong.toDouble / ScaleD
  @targetName("rateToDouble")
  def toDouble(r: Rate): Double          = r.toLong.toDouble / ScaleD
  @targetName("shareToDouble")
  def toDouble(s: Share): Double         = s.toLong.toDouble / ScaleD
  @targetName("scalarToDouble")
  def toDouble(s: Scalar): Double        = s.toLong.toDouble / ScaleD
  @targetName("multiplierToDouble")
  def toDouble(m: Multiplier): Double    = m.toLong.toDouble / ScaleD
  @targetName("coefficientToDouble")
  def toDouble(c: Coefficient): Double   = c.toLong.toDouble / ScaleD
  @targetName("priceIndexToDouble")
  def toDouble(p: PriceIndex): Double    = p.toLong.toDouble / ScaleD
  @targetName("sigmaToDouble")
  def toDouble(s: Sigma): Double         = s.toLong.toDouble / ScaleD
  @targetName("exchangeRateToDouble")
  def toDouble(er: ExchangeRate): Double = er.toLong.toDouble / ScaleD
