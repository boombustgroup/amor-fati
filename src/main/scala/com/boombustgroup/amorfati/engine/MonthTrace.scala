package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.{Banking, Firm, HhStatus, Household}
import com.boombustgroup.amorfati.types.*

/** Minimal month-level audit artifact for timing-sensitive execution.
  *
  * The trace is intentionally narrower than event sourcing: it captures the
  * month boundary, selected same-month outputs, extracted next-month signals,
  * and validation summaries.
  */
case class MonthTrace(
    month: Int,
    startSnapshot: MonthBoundarySnapshot,
    seedIn: DecisionSignals,
    stages: MonthStageTrace,
    executedFlows: Sfc.SemanticFlows,
    endSnapshot: MonthBoundarySnapshot,
    seedOut: DecisionSignals,
    seedOutProvenance: SeedOutProvenance,
    validations: Vector[MonthValidation],
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

/** Selected same-month outputs that matter for signal timing and FirmEntry. */
case class MonthStageTrace(
    operationalHiringSlack: Share,
    sectorDemandMult: Vector[Multiplier],
    sectorDemandPressure: Vector[Multiplier],
    sectorHiringSignal: Vector[Multiplier],
    realizedInflation: Rate,
    expectedInflation: Rate,
    startupAbsorptionRate: Share,
    firmBirths: Int,
    firmDeaths: Int,
    netFirmBirths: Int,
)

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
