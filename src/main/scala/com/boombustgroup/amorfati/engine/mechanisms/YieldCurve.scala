package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.types.*

/** Interbank term structure: WIRON O/N → WIBOR 1M/3M/6M with fixed term premia.
  *
  * Flat-spread model: each tenor = O/N rate + constant term premium. Premia
  * calibrated to NBP/GPW Benchmark 2024: ~6/9/12bp.
  */
object YieldCurve:

  // Term premia over O/N (annual, public — used by tests). NBP/GPW Benchmark 2024.
  val TermPremium1M: Double = 0.0006 // 6bp
  val TermPremium3M: Double = 0.0009 // 9bp
  val TermPremium6M: Double = 0.0012 // 12bp

  case class State(
      overnight: Rate, // O/N rate (WIRON)
      wibor1m: Rate,   // WIBOR 1M
      wibor3m: Rate,   // WIBOR 3M
      wibor6m: Rate,   // WIBOR 6M
  )

  /** Compute term structure from overnight (O/N) interbank rate. */
  def compute(overnightRate: Double): State =
    State(
      overnight = Rate(overnightRate),
      wibor1m = Rate(overnightRate + TermPremium1M),
      wibor3m = Rate(overnightRate + TermPremium3M),
      wibor6m = Rate(overnightRate + TermPremium6M),
    )
