package com.boombustgroup.amorfati

import scala.annotation.targetName

/** Fixed-point type system for SFC-ABM engine.
  *
  * All types are Long-based (scale 10^4). Each type is defined in an isolated
  * Provider object — inside that object, only its own opaque type is
  * transparent. Cross-type operations are defined here where ALL types are
  * opaque, so the compiler prevents PLN + Rate, Share + Coefficient, etc.
  *
  * No `.toDouble` in public API. Use `ComputationBoundary.toDouble(value)` +
  * `@computationBoundary` annotation for CES/CSV escape points.
  */
object types:
  // Re-export all types and their companions
  export com.boombustgroup.amorfati.fp.EntityIds.*
  export com.boombustgroup.amorfati.fp.PLNProvider.{PLN, given}
  export com.boombustgroup.amorfati.fp.RateProvider.{Rate, given}
  export com.boombustgroup.amorfati.fp.ShareProvider.{Share, given}
  export com.boombustgroup.amorfati.fp.MultiplierProvider.{Multiplier, given}
  export com.boombustgroup.amorfati.fp.CoefficientProvider.{Coefficient, given}
  export com.boombustgroup.amorfati.fp.PriceIndexProvider.{PriceIndex, given}
  export com.boombustgroup.amorfati.fp.SigmaProvider.{Sigma, given}

  // Re-export boundary tools
  export com.boombustgroup.amorfati.fp.{computationBoundary, ComputationBoundary}

  import com.boombustgroup.amorfati.fp.FixedPointBase.{asDouble, bankerRound}

  // === Cross-type operations ===
  // Defined HERE where all types are opaque — compiler enforces type safety.
  // Each operation explicitly uses .toLong to access the raw value.

  // --- PLN × typed ---
  extension (p: PLN)
    @targetName("plnTimesRate")
    def *(r: Rate): PLN        = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(r.toLong)))
    @targetName("plnTimesShare")
    def *(s: Share): PLN       = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(s.toLong)))
    @targetName("plnTimesMultiplier")
    def *(m: Multiplier): PLN  = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(m.toLong)))
    @targetName("plnTimesCoefficient")
    def *(c: Coefficient): PLN = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(c.toLong)))
    @targetName("plnDivShare")
    def /(s: Share): PLN       = if s.toLong != 0L then PLN(asDouble(p.toLong) / asDouble(s.toLong)) else PLN.Zero
    @targetName("plnDivMultiplier")
    def /(m: Multiplier): PLN  = if m.toLong != 0L then PLN(asDouble(p.toLong) / asDouble(m.toLong)) else PLN.Zero

  // --- Rate × typed ---
  extension (r: Rate)
    @targetName("rateTimesMultiplier")
    def *(m: Multiplier): Rate   = Rate(asDouble(r.toLong) * asDouble(m.toLong))
    @targetName("rateTimesShare")
    def *(s: Share): Rate        = Rate(asDouble(r.toLong) * asDouble(s.toLong))
    @targetName("rateTimesCoefficient")
    def *(c: Coefficient): Rate  = Rate(asDouble(r.toLong) * asDouble(c.toLong))
    @targetName("rateToMultiplier")
    def toMultiplier: Multiplier = Multiplier.fromRaw(r.toLong)

  // --- Share × typed ---
  extension (s: Share)
    @targetName("shareTimesMultiplier")
    def *(m: Multiplier): Multiplier   = Multiplier.fromRaw(bankerRound(BigInt(s.toLong) * BigInt(m.toLong)))
    @targetName("shareTimesCoefficient")
    def *(c: Coefficient): Coefficient = Coefficient(asDouble(s.toLong) * asDouble(c.toLong))
    @targetName("shareTimesPln")
    def *(p: PLN): PLN                 = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(s.toLong)))
    @targetName("shareToRate")
    def toRate: Rate                   = Rate(asDouble(s.toLong))
    @targetName("shareToMultiplier")
    def toMultiplier: Multiplier       = Multiplier.fromRaw(s.toLong)

  // --- Multiplier × typed ---
  extension (m: Multiplier)
    @targetName("multTimesShare")
    def *(s: Share): Multiplier = Multiplier.fromRaw(bankerRound(BigInt(m.toLong) * BigInt(s.toLong)))
    @targetName("multTimesPln")
    def *(p: PLN): PLN          = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(m.toLong)))
    @targetName("multToRate")
    def toRate: Rate            = Rate(asDouble(m.toLong))
    @targetName("multToShare")
    def toShare: Share          = Share(asDouble(m.toLong))

  // --- Coefficient × typed ---
  extension (c: Coefficient)
    @targetName("coefTimesShare")
    def *(s: Share): Coefficient = Coefficient(asDouble(c.toLong) * asDouble(s.toLong))
    @targetName("coefTimesMultiplier")
    def *(m: Multiplier): Share  = Share(asDouble(c.toLong) * asDouble(m.toLong))
    @targetName("coefTimesPln")
    def *(p: PLN): PLN           = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(c.toLong)))
    @targetName("coefToMultiplier")
    def toMultiplier: Multiplier = Multiplier.fromRaw(c.toLong)

  // --- PriceIndex × typed ---
  extension (pi: PriceIndex)
    @targetName("priceIdxTimesRate")
    def *(r: Rate): PriceIndex       = PriceIndex(asDouble(pi.toLong) * asDouble(r.toLong))
    @targetName("priceIdxTimesMultiplier")
    def *(m: Multiplier): PriceIndex = PriceIndex(asDouble(pi.toLong) * asDouble(m.toLong))
    @targetName("priceIdxTimesPln")
    def *(p: PLN): PLN               = PLN.fromRaw(bankerRound(BigInt(p.toLong) * BigInt(pi.toLong)))
