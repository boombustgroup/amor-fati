package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonthRandomnessSpec extends AnyFlatSpec with Matchers:

  "MonthRandomness.Contract" should "derive stable named streams from one root seed" in {
    val first  = MonthRandomness.Contract.fromSeed(1234L)
    val second = MonthRandomness.Contract.fromSeed(1234L)
    val third  = MonthRandomness.Contract.fromSeed(1235L)

    first shouldBe second
    first.rootSeed shouldBe 1234L
    first.all.map(_.key).toSet shouldBe Set(
      MonthRandomness.StreamKey.HouseholdIncomeEconomics,
      MonthRandomness.StreamKey.FirmEconomics,
      MonthRandomness.StreamKey.HouseholdFinancialEconomics,
      MonthRandomness.StreamKey.OpenEconEconomics,
      MonthRandomness.StreamKey.BankingEconomics,
      MonthRandomness.StreamKey.FdiMa,
      MonthRandomness.StreamKey.FirmEntry,
      MonthRandomness.StreamKey.StartupStaffing,
      MonthRandomness.StreamKey.RegionalMigration,
    )
    first.all.map(_.seed).distinct should have size first.all.size
    first.all.map(_.seed) should not equal third.all.map(_.seed)
    first.stages.openEconEconomics.seed shouldBe second.stages.openEconEconomics.seed
    first.stages.openEconEconomics.seed should not equal third.stages.openEconEconomics.seed
    first.assembly.regionalMigration.seed shouldBe second.assembly.regionalMigration.seed
    first.assembly.regionalMigration.seed should not equal third.assembly.regionalMigration.seed
  }
