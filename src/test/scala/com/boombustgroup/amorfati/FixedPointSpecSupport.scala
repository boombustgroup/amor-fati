package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.fp.*
import com.boombustgroup.amorfati.types.*
import scala.annotation.targetName

object FixedPointSpecSupport:
  private val ScaleBD = BigDecimal(FixedPointBase.Scale)

  private def rawValue(raw: Long): BigDecimal =
    BigDecimal(raw) / ScaleBD

  extension (p: PLN) @targetName("plnBd") def bd: BigDecimal                 = rawValue(p.toLong)
  extension (r: Rate) @targetName("rateBd") def bd: BigDecimal               = rawValue(r.toLong)
  extension (s: Share) @targetName("shareBd") def bd: BigDecimal             = rawValue(s.toLong)
  extension (s: Scalar) @targetName("scalarBd") def bd: BigDecimal           = rawValue(s.toLong)
  extension (m: Multiplier) @targetName("multiplierBd") def bd: BigDecimal   = rawValue(m.toLong)
  extension (c: Coefficient) @targetName("coefficientBd") def bd: BigDecimal = rawValue(c.toLong)
  extension (pi: PriceIndex) @targetName("priceIndexBd") def bd: BigDecimal  = rawValue(pi.toLong)
  extension (s: Sigma) @targetName("sigmaBd") def bd: BigDecimal             = rawValue(s.toLong)
