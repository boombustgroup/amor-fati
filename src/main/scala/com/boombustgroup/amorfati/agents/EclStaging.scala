package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** IFRS 9 Expected Credit Loss (ECL) staging for bank loan portfolios.
  *
  * Three-stage model replacing instantaneous NPL capital hit:
  *
  *   - '''Stage 1''' (performing, 12-month ECL): loans with no significant
  *     increase in credit risk. Provision = portfolio × eclRate1 (~1%).
  *   - '''Stage 2''' (watch, lifetime ECL): loans with significant increase in
  *     credit risk (GDP decline, unemployment spike). Provision = portfolio ×
  *     eclRate2 (~5-10%). Macro trigger shifts loans S1→S2 en masse.
  *   - '''Stage 3''' (default): non-performing loans. Provision = portfolio ×
  *     eclRate3 (= 1 − recovery rate). This is the current NPL treatment.
  *
  * Pro-cyclical amplification: GDP downturn → mass S1→S2 migration →
  * provisioning cliff → capital hit → lending restriction → deeper downturn.
  * Forward-looking: provisions are booked BEFORE defaults materialize.
  *
  * Pure functions — no mutable state. Per-bank staging computed from macro
  * signals (unemployment, GDP growth) and bank-level loan quality.
  *
  * Calibration: KNF IFRS 9 implementation guidelines, NBP Financial Stability
  * Report, EBA stress test methodology.
  */
object EclStaging:

  /** Per-bank ECL staging state. */
  case class State(
      stage1: PLN, // performing loans (12-month ECL)
      stage2: PLN, // watch loans (lifetime ECL)
      stage3: PLN, // defaulted loans (full provision)
  )
  object State:
    val zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero)

  /** ECL staging result: updated stages + total provision change. */
  case class StepResult(
      newStaging: State,
      provisionChange: PLN, // Δ provision this month (positive = additional provision → capital hit)
  )

  /** Compute macro-driven S1→S2 migration rate.
    *
    * When unemployment rises above NAIRU or GDP contracts, a fraction of
    * performing loans migrates to Stage 2 (significant credit risk increase).
    *
    * migrationRate = sensitivity × max(0, unemployment − nairu) +
    * gdpSensitivity × max(0, −gdpGrowth) Clamped to [0, maxMigration].
    */
  @boundaryEscape
  private[amorfati] def migrationRate(
      unemployment: Share,
      gdpGrowthMonthly: Double,
  )(using p: SimParams): Share =
    inline def shareRaw(s: Share): Double             = s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
    inline def coefficientRaw(c: Coefficient): Double = c.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
    val unempExcess                                   = shareRaw((unemployment - p.monetary.nairu).max(Share.Zero))
    val gdpContraction                                = Math.max(0.0, -gdpGrowthMonthly)
    val raw                                           = coefficientRaw(p.banking.eclMigrationSensitivity) * unempExcess + coefficientRaw(p.banking.eclGdpSensitivity) * gdpContraction
    Share(raw.min(shareRaw(p.banking.eclMaxMigration)).max(0.0))

  /** Monthly ECL staging step for a single bank.
    *
    * @param prev
    *   previous staging state
    * @param totalLoans
    *   total loan book (corporate + consumer)
    * @param nplNew
    *   new defaults this month (→ Stage 3)
    * @param unemployment
    *   current unemployment rate
    * @param gdpGrowthMonthly
    *   month-on-month GDP growth
    */
  def step(
      prev: State,
      totalLoans: PLN,
      nplNew: PLN,
      unemployment: Share,
      gdpGrowthMonthly: Double,
  )(using p: SimParams): StepResult =
    val migration = migrationRate(unemployment, gdpGrowthMonthly)

    // Stage transitions
    val s1ToS2 = prev.stage1 * migration             // macro-driven migration
    val s2ToS3 = nplNew                              // actual defaults enter S3
    val s3Cure = prev.stage3 * p.banking.eclCureRate // some S3 loans recover

    // Updated stages
    val newS3 = (prev.stage3 + s2ToS3 - s3Cure).max(PLN.Zero)
    val newS2 = (prev.stage2 + s1ToS2 - s2ToS3 + s3Cure).max(PLN.Zero)
    val newS1 = (totalLoans - newS2 - newS3).max(PLN.Zero)

    val newStaging = State(newS1, newS2, newS3)

    // Provision = Σ(stage × eclRate) — change vs previous month
    val prevProvision   = prev.stage1 * p.banking.eclRate1 + prev.stage2 * p.banking.eclRate2 + prev.stage3 * p.banking.eclRate3
    val newProvision    = newS1 * p.banking.eclRate1 + newS2 * p.banking.eclRate2 + newS3 * p.banking.eclRate3
    val provisionChange = newProvision - prevProvision

    StepResult(newStaging, provisionChange)
