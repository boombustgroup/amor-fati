package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.*

/** Explicit same-month operational signal surface for month-`t` execution.
  *
  * This is the counterpart to [[DecisionSignals]]:
  *
  *   - [[DecisionSignals]] are inherited `pre` inputs visible at the start of a
  *     month and persisted across month boundaries.
  *   - [[OperationalSignals]] are current-month artifacts produced inside the
  *     active step and consumed only by same-month operational blocks.
  *
  * The type is intentionally narrower than full stage outputs. It captures the
  * timing-sensitive surface currently needed by incumbent firm processing
  * without treating `PipelineState` as the owner of same-month semantics.
  */
case class OperationalSignals(
    sectorDemandMult: Vector[Multiplier],
    sectorDemandPressure: Vector[Multiplier],
    sectorHiringSignal: Vector[Multiplier],
    operationalHiringSlack: Share,
)

object OperationalSignals:

  def zero(sectorCount: Int): OperationalSignals =
    OperationalSignals(
      sectorDemandMult = Vector.fill(sectorCount)(Multiplier.One),
      sectorDemandPressure = Vector.fill(sectorCount)(Multiplier.One),
      sectorHiringSignal = Vector.fill(sectorCount)(Multiplier.One),
      operationalHiringSlack = Share.One,
    )

  def fromDecisionSignals(signals: DecisionSignals, operationalHiringSlack: Share): OperationalSignals =
    OperationalSignals(
      sectorDemandMult = signals.sectorDemandMult,
      sectorDemandPressure = signals.sectorDemandPressure,
      sectorHiringSignal = signals.sectorHiringSignal,
      operationalHiringSlack = operationalHiringSlack,
    )
