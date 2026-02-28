package sfc.engine

/** Interbank yield curve: O/N + WIBOR term rates (1M, 3M, 6M).
  * Calibrated to NBP/GPW Benchmark 2024 with elevated premiums for visible heterogeneity. */
case class InterbankCurve(
  overnight: Double,  // O/N rate (WIRON)
  wibor1m: Double,    // WIBOR 1M
  wibor3m: Double,    // WIBOR 3M
  wibor6m: Double     // WIBOR 6M
)

object YieldCurve:
  // Term premiums over O/N (annual).
  // NBP/GPW Benchmark 2024 actual: ~6/9/12bp.
  // Model uses 15/40/80bp for dynamic range and visible heterogeneity.
  val TermPremium1M: Double = 0.0015  // 15bp
  val TermPremium3M: Double = 0.0040  // 40bp
  val TermPremium6M: Double = 0.0080  // 80bp

  /** Compute term structure from overnight (O/N) interbank rate. */
  def compute(overnightRate: Double): InterbankCurve =
    InterbankCurve(
      overnight = overnightRate,
      wibor1m = overnightRate + TermPremium1M,
      wibor3m = overnightRate + TermPremium3M,
      wibor6m = overnightRate + TermPremium6M
    )
