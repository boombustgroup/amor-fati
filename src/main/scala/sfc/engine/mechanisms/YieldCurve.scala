package sfc.engine.mechanisms

import sfc.types.*

object YieldCurve:

  /** Interbank yield curve: O/N + WIBOR term rates (1M, 3M, 6M). Calibrated to
    * NBP/GPW Benchmark 2024 with elevated premiums for visible heterogeneity.
    */
  case class State(
      overnight: Rate, // O/N rate (WIRON)
      wibor1m: Rate,   // WIBOR 1M
      wibor3m: Rate,   // WIBOR 3M
      wibor6m: Rate,   // WIBOR 6M
  )
  // Term premiums over O/N (annual).
  // NBP/GPW Benchmark 2024 actual: ~6/9/12bp.
  // Model uses 15/40/80bp for dynamic range and visible heterogeneity.
  val TermPremium1M: Double = 0.0015 // 15bp
  val TermPremium3M: Double = 0.0040 // 40bp
  val TermPremium6M: Double = 0.0080 // 80bp

  /** Compute term structure from overnight (O/N) interbank rate. */
  def compute(overnightRate: Double): State =
    State(
      overnight = Rate(overnightRate),
      wibor1m = Rate(overnightRate + TermPremium1M),
      wibor3m = Rate(overnightRate + TermPremium3M),
      wibor6m = Rate(overnightRate + TermPremium6M),
    )
