package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.random.{RandomStream, SeedDerivation}

/** Explicit month-step randomness contract.
  *
  * Deterministic replay of one monthly step starts when both the state boundary
  * and the month `rootSeed` are fixed. All stage and mechanism streams are
  * derived deterministically from that explicit seed and stay independent.
  */
object MonthRandomness:

  enum StreamKey:
    case HouseholdIncomeEconomics
    case FirmEconomics
    case HouseholdFinancialEconomics
    case OpenEconEconomics
    case BankingEconomics
    case FdiMa
    case FirmEntry
    case StartupStaffing
    case RegionalMigration

  case class StreamSeed(
      key: StreamKey,
      seed: Long,
  ):
    def newStream(): RandomStream =
      RandomStream.seeded(seed)

  case class StageStreams(
      householdIncomeEconomics: RandomStream,
      firmEconomics: RandomStream,
      householdFinancialEconomics: RandomStream,
      openEconEconomics: RandomStream,
      bankingEconomics: RandomStream,
  )

  case class AssemblyStreams(
      fdiMa: RandomStream,
      firmEntry: RandomStream,
      startupStaffing: RandomStream,
      regionalMigration: RandomStream,
  )

  case class StageSeeds(
      householdIncomeEconomics: StreamSeed,
      firmEconomics: StreamSeed,
      householdFinancialEconomics: StreamSeed,
      openEconEconomics: StreamSeed,
      bankingEconomics: StreamSeed,
  ):
    def all: Vector[StreamSeed] =
      Vector(
        householdIncomeEconomics,
        firmEconomics,
        householdFinancialEconomics,
        openEconEconomics,
        bankingEconomics,
      )

    def newStreams(): StageStreams =
      StageStreams(
        householdIncomeEconomics = householdIncomeEconomics.newStream(),
        firmEconomics = firmEconomics.newStream(),
        householdFinancialEconomics = householdFinancialEconomics.newStream(),
        openEconEconomics = openEconEconomics.newStream(),
        bankingEconomics = bankingEconomics.newStream(),
      )

  case class AssemblySeeds(
      fdiMa: StreamSeed,
      firmEntry: StreamSeed,
      startupStaffing: StreamSeed,
      regionalMigration: StreamSeed,
  ):
    def all: Vector[StreamSeed] =
      Vector(
        fdiMa,
        firmEntry,
        startupStaffing,
        regionalMigration,
      )

    def newStreams(): AssemblyStreams =
      AssemblyStreams(
        fdiMa = fdiMa.newStream(),
        firmEntry = firmEntry.newStream(),
        startupStaffing = startupStaffing.newStream(),
        regionalMigration = regionalMigration.newStream(),
      )

  case class Contract(
      rootSeed: Long,
      stages: StageSeeds,
      assembly: AssemblySeeds,
  ):
    def all: Vector[StreamSeed] =
      stages.all ++ assembly.all

  object Contract:
    def fromSeed(rootSeed: Long): Contract =
      Contract(
        rootSeed = rootSeed,
        stages = StageSeeds(
          householdIncomeEconomics = deriveStream(rootSeed, StreamKey.HouseholdIncomeEconomics),
          firmEconomics = deriveStream(rootSeed, StreamKey.FirmEconomics),
          householdFinancialEconomics = deriveStream(rootSeed, StreamKey.HouseholdFinancialEconomics),
          openEconEconomics = deriveStream(rootSeed, StreamKey.OpenEconEconomics),
          bankingEconomics = deriveStream(rootSeed, StreamKey.BankingEconomics),
        ),
        assembly = AssemblySeeds(
          fdiMa = deriveStream(rootSeed, StreamKey.FdiMa),
          firmEntry = deriveStream(rootSeed, StreamKey.FirmEntry),
          startupStaffing = deriveStream(rootSeed, StreamKey.StartupStaffing),
          regionalMigration = deriveStream(rootSeed, StreamKey.RegionalMigration),
        ),
      )

  private def deriveStream(rootSeed: Long, key: StreamKey): StreamSeed =
    StreamSeed(key, SeedDerivation.derive(rootSeed, key.ordinal.toLong))
