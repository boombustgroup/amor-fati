package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.{Banking, Firm, HhStatus, Household}
import com.boombustgroup.amorfati.types.*

import scala.reflect.ClassTag

/** Minimal month-level audit artifact for timing-sensitive execution.
  *
  * The trace is intentionally narrower than event sourcing: it captures a
  * stable month boundary core plus an extensible interior of typed timing
  * envelopes.
  */
case class MonthTrace(
    month: Int,
    boundary: MonthBoundaryTrace,
    seedTransition: SeedTransitionTrace,
    timing: MonthTimingTrace,
    executedFlows: Sfc.SemanticFlows,
    validations: Vector[MonthValidation],
)

object MonthTrace:
  def timingEnvelope(
      key: MonthTimingEnvelopeKey,
      payload: MonthTimingPayload,
  ): MonthTimingEnvelope =
    MonthTimingEnvelope(key, payload)

/** Stable start/end month boundary snapshots. */
case class MonthBoundaryTrace(
    startSnapshot: MonthBoundarySnapshot,
    endSnapshot: MonthBoundarySnapshot,
)

/** Explicit post-to-pre boundary observed by the trace. */
case class SeedTransitionTrace(
    seedIn: DecisionSignals,
    seedOut: DecisionSignals,
    provenance: SeedOutProvenance,
)

/** Compact boundary snapshot used at the start and end of a month trace. */
case class MonthBoundarySnapshot(
    stock: Sfc.StockState,
    firmCount: Int,
    livingFirmCount: Int,
    householdCount: Int,
    bankCount: Int,
    activeBankCount: Int,
    employedHouseholds: Int,
    unemploymentRate: Share,
    inflation: Rate,
    priceLevel: PriceIndex,
    monthlyGdpProxy: PLN,
)

object MonthBoundarySnapshot:
  def capture(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  ): MonthBoundarySnapshot =
    val employedHouseholds = households.count: hh =>
      hh.status match
        case HhStatus.Employed(_, _, _) => true
        case _                          => false

    MonthBoundarySnapshot(
      stock = Sfc.snapshot(world, firms, households, banks),
      firmCount = firms.length,
      livingFirmCount = firms.count(Firm.isAlive),
      householdCount = households.length,
      bankCount = banks.length,
      activeBankCount = banks.count(b => !b.failed),
      employedHouseholds = employedHouseholds,
      unemploymentRate = world.unemploymentRate(employedHouseholds),
      inflation = world.inflation,
      priceLevel = world.priceLevel,
      monthlyGdpProxy = world.flows.monthlyGdpProxy,
    )

/** Extensible envelope keys for typed same-month timing artifacts. */
enum MonthTimingEnvelopeKey:
  case LaborSignals
  case DemandSignals
  case NominalSignals
  case FirmDynamics

/** Marker trait for typed same-month timing payloads. */
sealed trait MonthTimingPayload

object MonthTimingPayload:
  case class LaborSignals(
      operationalHiringSlack: Share,
  ) extends MonthTimingPayload

  case class DemandSignals(
      sectorDemandMult: Vector[Multiplier],
      sectorDemandPressure: Vector[Multiplier],
      sectorHiringSignal: Vector[Multiplier],
  ) extends MonthTimingPayload

  case class NominalSignals(
      realizedInflation: Rate,
      expectedInflation: Rate,
  ) extends MonthTimingPayload

  case class FirmDynamics(
      startupAbsorptionRate: Share,
      firmBirths: Int,
      firmDeaths: Int,
      netFirmBirths: Int,
  ) extends MonthTimingPayload

/** One typed timing envelope attached to the month trace interior. */
case class MonthTimingEnvelope(
    key: MonthTimingEnvelopeKey,
    payload: MonthTimingPayload,
)

/** Extensible same-month timing interior for audit and testing. */
case class MonthTimingTrace(
    envelopes: Vector[MonthTimingEnvelope],
):
  private val duplicateKeys =
    envelopes
      .groupMapReduce(_.key)(_ => 1)(_ + _)
      .collect { case (key, count) if count > 1 => key }
      .toVector

  require(
    duplicateKeys.isEmpty,
    s"MonthTimingTrace requires unique envelope keys, found duplicates: ${duplicateKeys.mkString(", ")}",
  )

  def envelope(key: MonthTimingEnvelopeKey): Option[MonthTimingEnvelope] =
    envelopes.find(_.key == key)

  def payload[A <: MonthTimingPayload](key: MonthTimingEnvelopeKey)(using ct: ClassTag[A]): Option[A] =
    envelope(key).flatMap: env =>
      if ct.runtimeClass.isInstance(env.payload) then Some(env.payload.asInstanceOf[A]) else None

  def requirePayload[A <: MonthTimingPayload](key: MonthTimingEnvelopeKey)(using ct: ClassTag[A]): A =
    payload[A](key).getOrElse:
      throw new IllegalStateException(s"MonthTimingTrace missing payload ${ct.runtimeClass.getSimpleName} at envelope $key")

  def laborSignals: MonthTimingPayload.LaborSignals =
    requirePayload[MonthTimingPayload.LaborSignals](MonthTimingEnvelopeKey.LaborSignals)

  def demandSignals: MonthTimingPayload.DemandSignals =
    requirePayload[MonthTimingPayload.DemandSignals](MonthTimingEnvelopeKey.DemandSignals)

  def nominalSignals: MonthTimingPayload.NominalSignals =
    requirePayload[MonthTimingPayload.NominalSignals](MonthTimingEnvelopeKey.NominalSignals)

  def firmDynamics: MonthTimingPayload.FirmDynamics =
    requirePayload[MonthTimingPayload.FirmDynamics](MonthTimingEnvelopeKey.FirmDynamics)

enum MonthValidationKind:
  case Sfc

case class MonthValidationFailure(
    identity: Sfc.SfcIdentity,
    message: String,
    expected: PLN,
    actual: PLN,
)

case class MonthValidation(
    kind: MonthValidationKind,
    failures: Vector[MonthValidationFailure],
):
  def passed: Boolean = failures.isEmpty

object MonthValidation:
  def fromSfcResult(result: Sfc.SfcResult): MonthValidation =
    result match
      case Right(())    => MonthValidation(MonthValidationKind.Sfc, Vector.empty)
      case Left(errors) =>
        MonthValidation(
          MonthValidationKind.Sfc,
          errors.map(e => MonthValidationFailure(e.identity, e.msg, e.expected, e.actual)),
        )
