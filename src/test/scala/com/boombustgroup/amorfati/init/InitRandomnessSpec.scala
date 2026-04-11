package com.boombustgroup.amorfati.init

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InitRandomnessSpec extends AnyFlatSpec with Matchers:

  "InitRandomness.Contract" should "derive stable named streams from one root seed" in {
    val first  = InitRandomness.Contract.fromSeed(1234L)
    val second = InitRandomness.Contract.fromSeed(1234L)
    val third  = InitRandomness.Contract.fromSeed(4321L)

    first shouldBe second
    first.rootSeed shouldBe 1234L
    first.all.map(_.key).toSet shouldBe Set(
      InitRandomness.StreamKey.FirmNetwork,
      InitRandomness.StreamKey.FirmSectorAssignments,
      InitRandomness.StreamKey.FirmSkeleton,
      InitRandomness.StreamKey.FirmRegions,
      InitRandomness.StreamKey.FirmCapitalAndBank,
      InitRandomness.StreamKey.FirmForeignOwnership,
      InitRandomness.StreamKey.FirmStateOwnership,
      InitRandomness.StreamKey.HouseholdNetwork,
      InitRandomness.StreamKey.HouseholdAttributes,
      InitRandomness.StreamKey.HouseholdInitialUnemployment,
      InitRandomness.StreamKey.InitialImmigrantStock,
    )
    first.all.map(_.seed).distinct should have size first.all.size
    first.all.map(_.seed) should not equal third.all.map(_.seed)
    first.firms.network.newStream().nextLong() shouldBe second.firms.network.newStream().nextLong()
    first.immigration.initialStock.newStream().nextDouble() shouldBe second.immigration.initialStock.newStream().nextDouble()
  }
