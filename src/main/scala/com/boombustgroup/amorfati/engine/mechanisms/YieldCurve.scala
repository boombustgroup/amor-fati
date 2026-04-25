package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.types.*

/** WIBOR term structure: O/N → 1M → 3M → 6M. Term premium over O/N reflects
  * credit stress (NPL) and inflation expectations anchoring (credibility). In
  * calm markets premium is near base (6/9/12 bps). In stress: NPL rises →
  * credit component widens; expectations de-anchor → credibility component
  * widens.
  */
object YieldCurve:

  // Base term premia over O/N (annual). NBP/GPW Benchmark 2024 calm-market values.
  val BasePremium1M: Rate = Rate.decimal(6, 4)  // 6bp
  val BasePremium3M: Rate = Rate.decimal(9, 4)  // 9bp
  val BasePremium6M: Rate = Rate.decimal(12, 4) // 12bp

  // Sensitivity to credit stress (NPL ratio): premium += nplRatio × sensitivity
  private val CreditSensitivity1M: Rate = Rate.decimal(5, 3)  // 50bp per 10% NPL
  private val CreditSensitivity3M: Rate = Rate.decimal(10, 3) // 100bp per 10% NPL
  private val CreditSensitivity6M: Rate = Rate.decimal(15, 3) // 150bp per 10% NPL

  // Sensitivity to de-anchored expectations: premium += (1 − κ) × |πᵉ − π*| × sensitivity
  private val ExpSensitivity1M: Coefficient = Coefficient.decimal(10, 2) // 10bp per 1pp de-anchoring gap
  private val ExpSensitivity3M: Coefficient = Coefficient.decimal(20, 2) // 20bp per 1pp de-anchoring gap
  private val ExpSensitivity6M: Coefficient = Coefficient.decimal(30, 2) // 30bp per 1pp de-anchoring gap

  case class State(
      overnight: Rate, // O/N rate (WIRON)
      wibor1m: Rate,   // WIBOR 1M
      wibor3m: Rate,   // WIBOR 3M
      wibor6m: Rate,   // WIBOR 6M
  )

  /** Compute term structure from O/N rate, credit stress, and expectations. */
  def compute(
      overnightRate: Rate,
      nplRatio: Share = Share.Zero,
      credibility: Share = Share.One,
      expectedInflation: Rate = Rate.decimal(25, 3),
      targetInflation: Rate = Rate.decimal(25, 3),
  ): State =
    val creditAdj1M = CreditSensitivity1M * nplRatio
    val creditAdj3M = CreditSensitivity3M * nplRatio
    val creditAdj6M = CreditSensitivity6M * nplRatio

    val expGap   = (expectedInflation - targetInflation).abs
    val deAnchor = credibility.complement
    val expAdj1M = expGap * deAnchor * ExpSensitivity1M
    val expAdj3M = expGap * deAnchor * ExpSensitivity3M
    val expAdj6M = expGap * deAnchor * ExpSensitivity6M

    State(
      overnight = overnightRate,
      wibor1m = overnightRate + BasePremium1M + creditAdj1M + expAdj1M,
      wibor3m = overnightRate + BasePremium3M + creditAdj3M + expAdj3M,
      wibor6m = overnightRate + BasePremium6M + creditAdj6M + expAdj6M,
    )
