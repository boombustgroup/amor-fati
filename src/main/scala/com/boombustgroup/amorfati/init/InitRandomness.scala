package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.random.{RandomStream, SeedDerivation}

/** Explicit initialization randomness contract.
  *
  * Bootstrapping replay becomes deterministic once both the config and the
  * `rootSeed` are fixed.
  */
object InitRandomness:

  enum StreamKey:
    case FirmNetwork
    case FirmSectorAssignments
    case FirmSkeleton
    case FirmRegions
    case FirmCapitalAndBank
    case FirmForeignOwnership
    case FirmStateOwnership
    case HouseholdNetwork
    case HouseholdAttributes
    case HouseholdInitialUnemployment
    case InitialImmigrantStock

  case class StreamSeed(
      key: StreamKey,
      seed: Long,
  ):
    def newStream(): RandomStream =
      RandomStream.seeded(seed)

  case class FirmStreams(
      network: RandomStream,
      sectorAssignments: RandomStream,
      skeleton: RandomStream,
      regions: RandomStream,
      capitalAndBank: RandomStream,
      foreignOwnership: RandomStream,
      stateOwnership: RandomStream,
  )

  case class HouseholdStreams(
      network: RandomStream,
      attributes: RandomStream,
      initialUnemployment: RandomStream,
  )

  case class ImmigrationStreams(
      initialStock: RandomStream,
  )

  case class FirmSeeds(
      network: StreamSeed,
      sectorAssignments: StreamSeed,
      skeleton: StreamSeed,
      regions: StreamSeed,
      capitalAndBank: StreamSeed,
      foreignOwnership: StreamSeed,
      stateOwnership: StreamSeed,
  ):
    def all: Vector[StreamSeed] =
      Vector(network, sectorAssignments, skeleton, regions, capitalAndBank, foreignOwnership, stateOwnership)

    def newStreams(): FirmStreams =
      FirmStreams(
        network = network.newStream(),
        sectorAssignments = sectorAssignments.newStream(),
        skeleton = skeleton.newStream(),
        regions = regions.newStream(),
        capitalAndBank = capitalAndBank.newStream(),
        foreignOwnership = foreignOwnership.newStream(),
        stateOwnership = stateOwnership.newStream(),
      )

  case class HouseholdSeeds(
      network: StreamSeed,
      attributes: StreamSeed,
      initialUnemployment: StreamSeed,
  ):
    def all: Vector[StreamSeed] =
      Vector(network, attributes, initialUnemployment)

    def newStreams(): HouseholdStreams =
      HouseholdStreams(
        network = network.newStream(),
        attributes = attributes.newStream(),
        initialUnemployment = initialUnemployment.newStream(),
      )

  case class ImmigrationSeeds(
      initialStock: StreamSeed,
  ):
    def all: Vector[StreamSeed] =
      Vector(initialStock)

    def newStreams(): ImmigrationStreams =
      ImmigrationStreams(
        initialStock = initialStock.newStream(),
      )

  case class Contract(
      rootSeed: Long,
      firms: FirmSeeds,
      households: HouseholdSeeds,
      immigration: ImmigrationSeeds,
  ):
    def all: Vector[StreamSeed] =
      firms.all ++ households.all ++ immigration.all

  object Contract:
    def fromSeed(rootSeed: Long): Contract =
      Contract(
        rootSeed = rootSeed,
        firms = FirmSeeds(
          network = deriveStream(rootSeed, StreamKey.FirmNetwork),
          sectorAssignments = deriveStream(rootSeed, StreamKey.FirmSectorAssignments),
          skeleton = deriveStream(rootSeed, StreamKey.FirmSkeleton),
          regions = deriveStream(rootSeed, StreamKey.FirmRegions),
          capitalAndBank = deriveStream(rootSeed, StreamKey.FirmCapitalAndBank),
          foreignOwnership = deriveStream(rootSeed, StreamKey.FirmForeignOwnership),
          stateOwnership = deriveStream(rootSeed, StreamKey.FirmStateOwnership),
        ),
        households = HouseholdSeeds(
          network = deriveStream(rootSeed, StreamKey.HouseholdNetwork),
          attributes = deriveStream(rootSeed, StreamKey.HouseholdAttributes),
          initialUnemployment = deriveStream(rootSeed, StreamKey.HouseholdInitialUnemployment),
        ),
        immigration = ImmigrationSeeds(
          initialStock = deriveStream(rootSeed, StreamKey.InitialImmigrantStock),
        ),
      )

  private def deriveStream(rootSeed: Long, key: StreamKey): StreamSeed =
    StreamSeed(key, SeedDerivation.derive(rootSeed, key.ordinal.toLong))
