package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.config.PopulationControlSchema.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class PopulationControlSchemaSpec extends AnyFlatSpec with Matchers:

  private val RegionClassification = ClassificationRef("fixture-region", "v1")
  private val AgeClassification    = ClassificationRef("fixture-age", "v1")
  private val SectorClassification = ClassificationRef("fixture-sector", "v1")

  private val North    = RegionCode("north")
  private val South    = RegionCode("south")
  private val Child    = AgeBand("0-14", 0, Some(14))
  private val Adult    = AgeBand("15-74", 15, Some(74))
  private val Goods    = ProductionSectorCode("goods")
  private val Services = ProductionSectorCode("services")

  private val classifications = Classifications(
    region = RegionClassification,
    age = AgeClassification,
    productionSector = SectorClassification,
    regions = Vector(North, South),
    ageBands = Vector(Child, Adult),
    productionSectors = Vector(Goods, Services),
  )

  private val source = SourceProvenance(
    provider = "synthetic-fixture",
    sourceLocation = "test://population-controls",
    observationPeriod = "fixture-period",
    release = "fixture-release",
    accessedAt = LocalDate.of(2026, 1, 1),
    transformation = "hand-constructed reconciliation fixture",
  )

  private def metadata(family: ControlFamily, classifications: Vector[ClassificationRef]): TableMetadata =
    TableMetadata(
      family = family,
      statisticalUniverse = "synthetic resident population",
      strength = ControlStrength.Hard,
      absoluteTolerance = 0L,
      classifications = classifications,
      source = source,
    )

  private val validBundle = Bundle(
    classifications = classifications,
    persons = ControlTable(
      metadata(ControlFamily.Persons, Vector(RegionClassification, AgeClassification)),
      Vector(
        PersonRow(North, DemographicSex.Female, Child, ResidenceType.PrivateHousehold, RepresentedCount(10)),
        PersonRow(North, DemographicSex.Male, Child, ResidenceType.PrivateHousehold, RepresentedCount(10)),
        PersonRow(North, DemographicSex.Female, Adult, ResidenceType.PrivateHousehold, RepresentedCount(30)),
        PersonRow(North, DemographicSex.Male, Adult, ResidenceType.PrivateHousehold, RepresentedCount(30)),
        PersonRow(North, DemographicSex.Male, Adult, ResidenceType.CollectiveResidence, RepresentedCount(2)),
        PersonRow(South, DemographicSex.Female, Child, ResidenceType.PrivateHousehold, RepresentedCount(8)),
        PersonRow(South, DemographicSex.Male, Child, ResidenceType.PrivateHousehold, RepresentedCount(8)),
        PersonRow(South, DemographicSex.Female, Adult, ResidenceType.PrivateHousehold, RepresentedCount(20)),
        PersonRow(South, DemographicSex.Male, Adult, ResidenceType.PrivateHousehold, RepresentedCount(20)),
      ),
    ),
    households = ControlTable(
      metadata(ControlFamily.Households, Vector(RegionClassification)),
      Vector(
        HouseholdRow(North, 1, HouseholdComposition.OnePerson, RepresentedCount(20)),
        HouseholdRow(North, 2, HouseholdComposition.CoupleWithoutDependentChildren, RepresentedCount(10)),
        HouseholdRow(North, 4, HouseholdComposition.CoupleWithDependentChildren, RepresentedCount(5)),
        HouseholdRow(North, 3, HouseholdComposition.LoneParent, RepresentedCount(4)),
        HouseholdRow(North, 4, HouseholdComposition.OtherMultiPerson, RepresentedCount(2)),
        HouseholdRow(South, 1, HouseholdComposition.OnePerson, RepresentedCount(16)),
        HouseholdRow(South, 2, HouseholdComposition.CoupleWithoutDependentChildren, RepresentedCount(5)),
        HouseholdRow(South, 4, HouseholdComposition.CoupleWithDependentChildren, RepresentedCount(5)),
        HouseholdRow(South, 3, HouseholdComposition.LoneParent, RepresentedCount(2)),
        HouseholdRow(South, 4, HouseholdComposition.OtherMultiPerson, RepresentedCount(1)),
      ),
    ),
    householdMembership = ControlTable(
      metadata(ControlFamily.HouseholdMembership, Vector(AgeClassification)),
      Vector(
        HouseholdMembershipRow(HouseholdComposition.OnePerson, HouseholdMemberRole.Adult, Adult, RepresentedCount(36)),
        HouseholdMembershipRow(HouseholdComposition.CoupleWithoutDependentChildren, HouseholdMemberRole.Adult, Adult, RepresentedCount(30)),
        HouseholdMembershipRow(HouseholdComposition.CoupleWithDependentChildren, HouseholdMemberRole.Adult, Adult, RepresentedCount(20)),
        HouseholdMembershipRow(HouseholdComposition.CoupleWithDependentChildren, HouseholdMemberRole.DependentChild, Child, RepresentedCount(20)),
        HouseholdMembershipRow(HouseholdComposition.LoneParent, HouseholdMemberRole.Adult, Adult, RepresentedCount(6)),
        HouseholdMembershipRow(HouseholdComposition.LoneParent, HouseholdMemberRole.DependentChild, Child, RepresentedCount(12)),
        HouseholdMembershipRow(HouseholdComposition.OtherMultiPerson, HouseholdMemberRole.Adult, Adult, RepresentedCount(8)),
        HouseholdMembershipRow(HouseholdComposition.OtherMultiPerson, HouseholdMemberRole.DependentChild, Child, RepresentedCount(4)),
      ),
    ),
    demographicLabour = ControlTable(
      metadata(ControlFamily.Labour, Vector(AgeClassification)),
      Vector(
        DemographicLabourRow(DemographicSex.Female, Child, LabourStatus.NotApplicable, RepresentedCount(18)),
        DemographicLabourRow(DemographicSex.Male, Child, LabourStatus.NotApplicable, RepresentedCount(18)),
        DemographicLabourRow(DemographicSex.Female, Adult, LabourStatus.Employed, RepresentedCount(40)),
        DemographicLabourRow(DemographicSex.Female, Adult, LabourStatus.Unemployed, RepresentedCount(5)),
        DemographicLabourRow(DemographicSex.Female, Adult, LabourStatus.Inactive, RepresentedCount(5)),
        DemographicLabourRow(DemographicSex.Male, Adult, LabourStatus.Employed, RepresentedCount(42)),
        DemographicLabourRow(DemographicSex.Male, Adult, LabourStatus.Unemployed, RepresentedCount(5)),
        DemographicLabourRow(DemographicSex.Male, Adult, LabourStatus.Inactive, RepresentedCount(5)),
      ),
    ),
    regionalLabour = ControlTable(
      metadata(ControlFamily.Labour, Vector(RegionClassification)),
      Vector(
        RegionalLabourRow(North, LabourStatus.Employed, RepresentedCount(50)),
        RegionalLabourRow(North, LabourStatus.Unemployed, RepresentedCount(6)),
        RegionalLabourRow(North, LabourStatus.Inactive, RepresentedCount(6)),
        RegionalLabourRow(North, LabourStatus.NotApplicable, RepresentedCount(20)),
        RegionalLabourRow(South, LabourStatus.Employed, RepresentedCount(32)),
        RegionalLabourRow(South, LabourStatus.Unemployed, RepresentedCount(4)),
        RegionalLabourRow(South, LabourStatus.Inactive, RepresentedCount(4)),
        RegionalLabourRow(South, LabourStatus.NotApplicable, RepresentedCount(16)),
      ),
    ),
    employment = ControlTable(
      metadata(ControlFamily.Employment, Vector(RegionClassification, SectorClassification)),
      Vector(
        EmploymentRow(North, North, Goods, RepresentedCount(30)),
        EmploymentRow(North, South, Services, RepresentedCount(20)),
        EmploymentRow(South, North, Goods, RepresentedCount(12)),
        EmploymentRow(South, South, Services, RepresentedCount(20)),
      ),
    ),
  )

  "PopulationControlSchema.Validator" should "accept a reconciled synthetic bundle without an economic scale multiplier" in {
    val report = Validator.validate(validBundle)

    withClue(s"Validation errors: ${report.errors.mkString(", ")}") {
      report.isValid shouldBe true
    }
    report.reconciliations.map(_.id) should contain("private-person-membership")
    report.reconciliations.foreach(_.passes shouldBe true)
  }

  it should "require the non-BAEL residual for residents aged 90 and over" in {
    val Oldest     = AgeBand("90+", 90, None)
    val withOldest = validBundle.copy(
      classifications = validBundle.classifications.copy(ageBands = validBundle.classifications.ageBands :+ Oldest),
      persons = validBundle.persons.copy(
        rows = validBundle.persons.rows :+ PersonRow(North, DemographicSex.Female, Oldest, ResidenceType.CollectiveResidence, RepresentedCount(1)),
      ),
      demographicLabour = validBundle.demographicLabour.copy(
        rows = validBundle.demographicLabour.rows :+ DemographicLabourRow(DemographicSex.Female, Oldest, LabourStatus.NonBaelResidual, RepresentedCount(1)),
      ),
      regionalLabour = validBundle.regionalLabour.copy(
        rows = validBundle.regionalLabour.rows :+ RegionalLabourRow(North, LabourStatus.NonBaelResidual, RepresentedCount(1)),
      ),
    )

    Validator.validate(withOldest).isValid shouldBe true

    val invalid = withOldest.copy(
      demographicLabour = withOldest.demographicLabour.copy(
        rows = withOldest.demographicLabour.rows.updated(
          withOldest.demographicLabour.rows.size - 1,
          DemographicLabourRow(DemographicSex.Female, Oldest, LabourStatus.Inactive, RepresentedCount(1)),
        ),
      ),
    )

    Validator.validate(invalid).errors should contain(
      ValidationError.InvalidLabourStatusAgeBand(LabourStatus.Inactive, Oldest),
    )
  }

  it should "reject a primary employment-assignment total that differs from employed residents" in {
    val invalidEmployment = validBundle.employment.copy(
      rows = validBundle.employment.rows.updated(3, EmploymentRow(South, South, Services, RepresentedCount(19))),
    )
    val report            = Validator.validate(validBundle.copy(employment = invalidEmployment))

    report.isValid shouldBe false
    report.errors.collect { case ValidationError.FailedReconciliation(reconciliation) => reconciliation.id } should contain(
      "employed-residents-to-primary-job-assignments:RegionCode(south)",
    )
  }

  it should "apply a reconciliation tolerance only to its participating tables" in {
    val tolerantEmployment       = validBundle.employment.copy(
      metadata = validBundle.employment.metadata.copy(absoluteTolerance = 1L),
    )
    val invalidDemographicLabour = validBundle.demographicLabour.copy(
      rows = validBundle.demographicLabour.rows.updated(
        2,
        DemographicLabourRow(DemographicSex.Female, Adult, LabourStatus.Employed, RepresentedCount(39)),
      ),
    )
    val report                   = Validator.validate(
      validBundle.copy(employment = tolerantEmployment, demographicLabour = invalidDemographicLabour),
    )

    report.errors.collect { case ValidationError.FailedReconciliation(reconciliation) => reconciliation.id } should contain(
      "person-to-demographic-labour:(Female,AgeBand(15-74,15,Some(74)))",
    )
  }

  it should "reject an undeclared axis value and duplicate control cell" in {
    val unknownRegion             = RegionCode("undeclared")
    val unknownPersonAge          = AgeBand("undeclared-person-age", 75, Some(80))
    val unknownHouseholdRegion    = RegionCode("undeclared-household-region")
    val unknownMembershipAge      = AgeBand("undeclared-membership-age", 75, Some(80))
    val unknownDemographicAge     = AgeBand("undeclared-demographic-age", 75, Some(80))
    val unknownRegionalLabourArea = RegionCode("undeclared-regional-labour-area")
    val unknownEmploymentHome     = RegionCode("undeclared-employment-home")
    val unknownEmploymentWork     = RegionCode("undeclared-employment-work")
    val unknownEmploymentSector   = ProductionSectorCode("undeclared-employment-sector")
    val invalidPersons            = validBundle.persons.copy(
      rows = validBundle.persons.rows ++ Vector(
        PersonRow(unknownRegion, DemographicSex.Female, Child, ResidenceType.PrivateHousehold, RepresentedCount(1)),
        PersonRow(North, DemographicSex.Female, unknownPersonAge, ResidenceType.PrivateHousehold, RepresentedCount(1)),
      ),
    )
    val invalidBundle             = validBundle.copy(
      persons = invalidPersons,
      households = validBundle.households.copy(
        rows = validBundle.households.rows :+ HouseholdRow(unknownHouseholdRegion, 1, HouseholdComposition.OnePerson, RepresentedCount(1)),
      ),
      householdMembership = validBundle.householdMembership.copy(
        rows = validBundle.householdMembership.rows :+ HouseholdMembershipRow(
          HouseholdComposition.OnePerson,
          HouseholdMemberRole.Adult,
          unknownMembershipAge,
          RepresentedCount(1),
        ),
      ),
      demographicLabour = validBundle.demographicLabour.copy(
        rows = validBundle.demographicLabour.rows :+ DemographicLabourRow(
          DemographicSex.Female,
          unknownDemographicAge,
          LabourStatus.Inactive,
          RepresentedCount(1),
        ),
      ),
      regionalLabour = validBundle.regionalLabour.copy(
        rows = validBundle.regionalLabour.rows :+ RegionalLabourRow(
          unknownRegionalLabourArea,
          LabourStatus.Inactive,
          RepresentedCount(1),
        ),
      ),
      employment = validBundle.employment.copy(
        rows = validBundle.employment.rows :+ EmploymentRow(
          unknownEmploymentHome,
          unknownEmploymentWork,
          unknownEmploymentSector,
          RepresentedCount(1),
        ),
      ),
    )
    val report                    = Validator.validate(invalidBundle)

    report.errors should contain(ValidationError.UnknownRegion("persons", unknownRegion))
    report.errors should contain(ValidationError.UnknownAgeBand("persons", unknownPersonAge))
    report.errors should contain(ValidationError.UnknownRegion("households", unknownHouseholdRegion))
    report.errors should contain(ValidationError.UnknownAgeBand("household-membership", unknownMembershipAge))
    report.errors should contain(ValidationError.UnknownAgeBand("demographic-labour", unknownDemographicAge))
    report.errors should contain(ValidationError.UnknownRegion("regional-labour", unknownRegionalLabourArea))
    report.errors should contain(ValidationError.UnknownRegion("employment residence", unknownEmploymentHome))
    report.errors should contain(ValidationError.UnknownRegion("employment workplace", unknownEmploymentWork))
    report.errors should contain(ValidationError.UnknownProductionSector("employment", unknownEmploymentSector))

    val duplicatePersons = validBundle.persons.copy(rows = validBundle.persons.rows :+ validBundle.persons.rows.head)
    Validator.validate(validBundle.copy(persons = duplicatePersons)).errors.exists {
      case ValidationError.DuplicateRow("persons", _) => true
      case _                                          => false
    } shouldBe true
  }

  it should "require each table to declare the classifications it uses" in {
    val incompleteMetadata = validBundle.employment.metadata.copy(classifications = Vector(RegionClassification))
    val report             = Validator.validate(validBundle.copy(employment = validBundle.employment.copy(metadata = incompleteMetadata)))

    report.errors should contain(ValidationError.MissingClassification(ControlFamily.Employment, SectorClassification))
  }

  it should "enforce the declared labour-status age convention" in {
    val invalidLabour = validBundle.demographicLabour.copy(
      rows = validBundle.demographicLabour.rows.updated(
        0,
        DemographicLabourRow(DemographicSex.Female, Child, LabourStatus.Employed, RepresentedCount(18)),
      ),
    )
    val report        = Validator.validate(validBundle.copy(demographicLabour = invalidLabour))

    report.errors should contain(ValidationError.InvalidLabourStatusAgeBand(LabourStatus.Employed, Child))
  }

  it should "reject collective persons outside a private-household population scope" in {
    val report = Validator.validate(validBundle.copy(populationScope = PopulationScope.PrivateHouseholdResidents))

    report.errors should contain(
      ValidationError.PopulationScopeViolation(PopulationScope.PrivateHouseholdResidents, ResidenceType.CollectiveResidence),
    )
  }

  it should "ignore zero-count collective rows outside a private-household scope" in {
    val zeroCollective = validBundle.persons.rows.map: row =>
      if row.residence == ResidenceType.CollectiveResidence then row.copy(count = RepresentedCount(0L)) else row
    val report         = Validator.validate(
      validBundle.copy(
        persons = validBundle.persons.copy(rows = zeroCollective),
        populationScope = PopulationScope.PrivateHouseholdResidents,
      ),
    )

    report.errors should not contain ValidationError.PopulationScopeViolation(
      PopulationScope.PrivateHouseholdResidents,
      ResidenceType.CollectiveResidence,
    )
  }

  it should "reject overlapping age-band classifications before a bundle is built" in {
    an[IllegalArgumentException] should be thrownBy
      classifications.copy(ageBands = Vector(AgeBand("0-20", 0, Some(20)), AgeBand("15+", 15, None)))
  }

end PopulationControlSchemaSpec
