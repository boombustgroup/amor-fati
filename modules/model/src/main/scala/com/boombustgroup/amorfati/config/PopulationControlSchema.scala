package com.boombustgroup.amorfati.config

import java.time.LocalDate

/** Versioned input contract for the person, household, labour, and employment
  * control tables consumed by the future population compiler.
  *
  * A control is a count in the reference economy, not an agent count and not a
  * scaling coefficient. The compiler will turn these controls into a finite
  * represented population and explicit representation weights. It must not use
  * this schema to derive an economy-wide monetary multiplier.
  *
  * The contract is deliberately independent of the current agent population and
  * `SimParams`. Supplying a [[Bundle]] says only that a source bundle has been
  * described and structurally reconciled; it does not claim that a particular
  * country or period has been empirically qualified or compiled.
  */
object PopulationControlSchema:

  /** Version of the logical population-control contract. Persisted control
    * bundles must declare their compatible version once a data-only loader is
    * introduced; this constant is also recorded by compilation manifests.
    */
  val SchemaVersion: Int = 1

  /** An integer count in the reference statistical population before any
    * sampling or representation weighting.
    *
    * This type is intentionally distinct from a number of simulated agents: one
    * simulated person can later represent many of these persons. It is also
    * intentionally not a monetary quantity or a continuous weight.
    */
  final case class RepresentedCount(value: Long):
    require(value >= 0L, s"represented count must be non-negative: $value")

  enum ControlStrength:
    /** The compiler must reproduce the row total within the table's declared
      * absolute tolerance. Hard controls constrain generated populations.
      */
    case Hard

    /** The row is an empirically useful margin but is not a binding population
      * total. A compiler may use it for synthesis or calibration while
      * reporting any resulting residual explicitly.
      */
    case CalibratedMargin

  /** Broad family of a table. This prevents a table with plausible columns but
    * the wrong economic meaning from being used in a control slot.
    */
  enum ControlFamily:
    /** Resident persons, including persons in collective residences. */
    case Persons

    /** Private households, not the people living in them. */
    case Households

    /** Person positions within private households, not household counts. */
    case HouseholdMembership

    /** Labour-status population margins, by demographic or regional axis. */
    case Labour

    /** Primary employment assignments: one main-job assignment per employed
      * resident, distinct from all job positions, FTE, vacancies, and secondary
      * jobs.
      */
    case Employment

  /** Identity and version of a classification system used by a table, such as a
    * territorial, age-band, or production-sector classification.
    *
    * Values in the rows must be interpreted against this reference; a label
    * alone is not sufficient for reproducibility when classifications change.
    */
  final case class ClassificationRef(id: String, version: String):
    require(id.trim.nonEmpty, "classification ID must be non-empty")
    require(version.trim.nonEmpty, s"classification $id must have a version")

  /** Provenance of a source table after any transformation required to make it
    * a population control.
    *
    * `observationPeriod` is when the measured population applies, `release`
    * identifies the published vintage, and `accessedAt` records when Amor Fati
    * retrieved it. `transformation` must explain operations such as category
    * aggregation, interpolation, or an adjustment of statistical universes.
    */
  final case class SourceProvenance(
      provider: String,
      sourceLocation: String,
      observationPeriod: String,
      release: String,
      accessedAt: LocalDate,
      transformation: String,
  ):
    require(provider.trim.nonEmpty, "source provider must be non-empty")
    require(sourceLocation.trim.nonEmpty, s"source location for $provider must be non-empty")
    require(observationPeriod.trim.nonEmpty, s"observation period for $provider must be non-empty")
    require(release.trim.nonEmpty, s"release for $provider must be non-empty")
    require(transformation.trim.nonEmpty, s"transformation for $provider must be non-empty")

  /** Metadata that defines a table's statistical meaning and makes its use
    * reproducible.
    *
    * `statisticalUniverse` names the population actually counted, for example
    * resident persons or private households. `absoluteTolerance` is expressed
    * in [[RepresentedCount]] units and applies only to reconciliations that use
    * this table; it is not a generic percentage error or a licence to rescale
    * the economy.
    */
  final case class TableMetadata(
      family: ControlFamily,
      statisticalUniverse: String,
      strength: ControlStrength,
      absoluteTolerance: Long,
      classifications: Vector[ClassificationRef],
      source: SourceProvenance,
  ):
    require(statisticalUniverse.trim.nonEmpty, s"${family.toString} statistical universe must be non-empty")
    require(absoluteTolerance >= 0L, s"${family.toString} absolute tolerance must be non-negative")
    require(classifications.nonEmpty, s"${family.toString} must declare at least one classification")
    require(
      classifications.distinct.size == classifications.size,
      s"${family.toString} must not repeat a classification reference",
    )

  /** One non-empty control table. The row type fixes its dimensions while the
    * metadata fixes its universe, source, classifications, and strength.
    */
  final case class ControlTable[A](metadata: TableMetadata, rows: Vector[A]):
    require(rows.nonEmpty, s"${metadata.family.toString} controls must not be empty")

  /** Stable code from the bundle's declared territorial classification, not a
    * `Region` enum from the current simulation runtime.
    */
  final case class RegionCode(value: String):
    require(value.trim.nonEmpty, "region code must be non-empty")

  /** Stable code from the bundle's declared production classification. It
    * constrains jobs and firms, rather than being a current runtime sector ID.
    */
  final case class ProductionSectorCode(value: String):
    require(value.trim.nonEmpty, "production sector code must be non-empty")

  /** A closed age interval, or an upper-open interval such as `90+`.
    *
    * Bands are values rather than bare labels so the validator can reject
    * overlapping classifications and enforce labour-statistics age universes.
    */
  final case class AgeBand(code: String, minInclusive: Int, maxInclusive: Option[Int]):
    require(code.trim.nonEmpty, "age-band code must be non-empty")
    require(minInclusive >= 0, s"age-band minimum must be non-negative: $minInclusive")
    require(maxInclusive.forall(_ >= minInclusive), s"age-band maximum must be at least $minInclusive")

  enum DemographicSex:
    /** Female category in the baseline's declared demographic classification.
      */
    case Female

    /** Male category in the baseline's declared demographic classification. */
    case Male

  /** Residence perimeter for person controls. Only private-household persons
    * reconcile to household-membership positions.
    */
  enum ResidenceType:
    /** Person resides in a private household represented by household controls.
      */
    case PrivateHousehold

    /** Person resides outside private households, for example in an
      * institutional or other collective residence.
      */
    case CollectiveResidence

  /** Runtime perimeter represented by a population-control bundle. This is a
    * domain boundary, not a reference to any particular national survey.
    */
  enum PopulationScope:
    /** All usual residents represented by the bundle. */
    case AllUsualResidents

    /** Only residents of private households are materialized as persons. */
    case PrivateHouseholdResidents

  /** Household composition used for both household counts and member-position
    * controls. Their shared value permits the capacity reconciliation.
    */
  enum HouseholdComposition:
    case OnePerson
    case CoupleWithoutDependentChildren
    case CoupleWithDependentChildren
    case LoneParent
    case OtherMultiPerson

  /** Role of a represented member position within a household-composition
    * margin. These roles are not legal relationships or runtime agent classes.
    */
  enum HouseholdMemberRole:
    case Adult
    case DependentChild
    case OtherMember

  /** Labour-force status used to partition demographic and regional population
    * margins. The permitted age bands follow the stated BAEL convention; a
    * baseline that uses another convention must make that change explicit.
    */
  enum LabourStatus:
    /** In work within the employment age universe. */
    case Employed

    /** In the labour force without work, within the unemployment age universe.
      */
    case Unemployed

    /** Outside the labour force, but within the inactive-population age
      * universe.
      */
    case Inactive

    /** Persons younger than the labour-force universe. They still reconcile to
      * the resident population but are neither employed, unemployed, nor
      * inactive.
      */
    case NotApplicable

    /** Explicit 90+ residual outside the BAEL labour-status universe. Every
      * resident in that band uses this model-specific status: it remains in the
      * person, demographic, and regional reconciliation totals, but is excluded
      * from BAEL rates and primary employment assignments.
      */
    case NonBaelResidual

  /** Closed vocabularies accepted by this bundle's rows.
    *
    * The compiler receives these axes from a baseline rather than current
    * runtime enums. Rows using an undeclared code or band are invalid even when
    * their labels look familiar.
    */
  final case class Classifications(
      region: ClassificationRef,
      age: ClassificationRef,
      productionSector: ClassificationRef,
      regions: Vector[RegionCode],
      ageBands: Vector[AgeBand],
      productionSectors: Vector[ProductionSectorCode],
  ):
    require(regions.nonEmpty, "population controls require at least one region")
    require(ageBands.nonEmpty, "population controls require at least one age band")
    require(productionSectors.nonEmpty, "population controls require at least one production sector")
    require(regions.distinct.size == regions.size, "population-control regions must be unique")
    require(ageBands.map(_.code).distinct.size == ageBands.size, "population-control age-band codes must be unique")
    require(productionSectors.distinct.size == productionSectors.size, "population-control production sectors must be unique")
    require(nonOverlapping(ageBands), "population-control age bands must not overlap")

  /** Resident-person count by territory, demographic sex, age band, and
    * residence perimeter. Counts from `PrivateHousehold` rows must reconcile to
    * household-member positions; collective-residence rows remain part of the
    * total person population but have no household membership counterpart.
    */
  final case class PersonRow(
      region: RegionCode,
      sex: DemographicSex,
      ageBand: AgeBand,
      residence: ResidenceType,
      count: RepresentedCount,
  )

  /** Private-household count by region, observed size, and composition. The
    * product of `size` and `count` is the household capacity used to reconcile
    * against [[HouseholdMembershipRow]] controls.
    */
  final case class HouseholdRow(
      region: RegionCode,
      size: Int,
      composition: HouseholdComposition,
      count: RepresentedCount,
  ):
    require(size > 0, s"household size must be positive: $size")

  /** Count of represented person positions in private households, not a count
    * of households. A composition's rows must sum to the capacity implied by
    * corresponding [[HouseholdRow]] controls.
    */
  final case class HouseholdMembershipRow(
      composition: HouseholdComposition,
      memberRole: HouseholdMemberRole,
      ageBand: AgeBand,
      count: RepresentedCount,
  )

  /** Labour-status population margin by demographic sex and age. Together, its
    * statuses must partition the corresponding person count and reconcile to
    * the independent regional labour margin.
    */
  final case class DemographicLabourRow(
      sex: DemographicSex,
      ageBand: AgeBand,
      status: LabourStatus,
      count: RepresentedCount,
  )

  /** Labour-status population margin by residence region. Its employed total
    * reconciles to primary employment assignments by workers' residence, not
    * necessarily by workplace because commuters are represented explicitly.
    */
  final case class RegionalLabourRow(
      region: RegionCode,
      status: LabourStatus,
      count: RepresentedCount,
  )

  /** Primary employment assignments classified by worker residence, workplace,
    * and production sector. Each count is a jobholder/person count for one
    * employed resident's main job, not a total-position, FTE, vacancy, or
    * secondary-job count. The residence axis reconciles this table to employed
    * residents; the workplace axis preserves commuting without treating jobs as
    * residents.
    */
  final case class EmploymentRow(
      residenceRegion: RegionCode,
      workplaceRegion: RegionCode,
      productionSector: ProductionSectorCode,
      count: RepresentedCount,
  )

  /** Coherent set of the five logical control families for one baseline.
    *
    * Labour has two physical tables because demographic and regional margins
    * independently constrain the same status population. They are both
    * required: neither is derived from the other by this schema.
    */
  final case class Bundle(
      classifications: Classifications,
      persons: ControlTable[PersonRow],
      households: ControlTable[HouseholdRow],
      householdMembership: ControlTable[HouseholdMembershipRow],
      demographicLabour: ControlTable[DemographicLabourRow],
      regionalLabour: ControlTable[RegionalLabourRow],
      employment: ControlTable[EmploymentRow],
      populationScope: PopulationScope = PopulationScope.AllUsualResidents,
  )

  /** Evidence for one accounting-style count reconciliation. `expected` is the
    * authoritative side named by the reconciliation, `actual` is the compared
    * side, and `residual` is actual minus expected. `BigInt` avoids overflow
    * when aggregating national counts.
    */
  final case class Reconciliation(
      id: String,
      expected: BigInt,
      actual: BigInt,
      tolerance: Long,
  ):
    def residual: BigInt = actual - expected
    def passes: Boolean  = residual.abs <= BigInt(tolerance)

  /** Structural or reconciliation failure in a [[Bundle]]. These errors say
    * that a bundle cannot safely enter a population compiler; they do not judge
    * the empirical credibility of its source or calibration.
    */
  enum ValidationError:
    case WrongControlFamily(expected: ControlFamily, actual: ControlFamily)
    case MissingClassification(table: ControlFamily, required: ClassificationRef)
    case DuplicateRow(table: String, key: String)
    case UnknownRegion(table: String, region: RegionCode)
    case UnknownAgeBand(table: String, ageBand: AgeBand)
    case UnknownProductionSector(table: String, sector: ProductionSectorCode)
    case InvalidLabourStatusAgeBand(status: LabourStatus, ageBand: AgeBand)
    case PopulationScopeViolation(scope: PopulationScope, residence: ResidenceType)
    case FailedReconciliation(reconciliation: Reconciliation)

  /** Complete structural-validation evidence. Reconciliations are retained even
    * when they pass so a baseline manifest can report the residuals.
    */
  final case class ValidationReport(
      reconciliations: Vector[Reconciliation],
      errors: Vector[ValidationError],
  ):
    def isValid: Boolean = errors.isEmpty

  object Validator:

    /** Validate a bundle's declared classification axes, duplicate cells,
      * labour-age convention, and cross-table count identities.
      *
      * This is a pre-compilation gate. It does not synthesize agents, estimate
      * missing margins, validate source methodology, or qualify a baseline for
      * scientific use.
      */
    def validate(bundle: Bundle): ValidationReport =
      val classifications = bundle.classifications
      val errors          =
        metadataErrors(bundle, classifications) ++
          duplicateErrors(bundle) ++
          axisErrors(bundle, classifications) ++
          labourAgeErrors(bundle) ++
          populationScopeErrors(bundle)
      val reconciliations = reconciliationChecks(bundle)
      val allErrors       = errors ++ reconciliations.filterNot(_.passes).map(ValidationError.FailedReconciliation.apply)
      ValidationReport(reconciliations, allErrors)

    private def metadataErrors(bundle: Bundle, classifications: Classifications): Vector[ValidationError] =
      val expected = Vector(
        (bundle.persons.metadata, ControlFamily.Persons, Vector(classifications.region, classifications.age)),
        (bundle.households.metadata, ControlFamily.Households, Vector(classifications.region)),
        (bundle.householdMembership.metadata, ControlFamily.HouseholdMembership, Vector(classifications.age)),
        (bundle.demographicLabour.metadata, ControlFamily.Labour, Vector(classifications.age)),
        (bundle.regionalLabour.metadata, ControlFamily.Labour, Vector(classifications.region)),
        (bundle.employment.metadata, ControlFamily.Employment, Vector(classifications.region, classifications.productionSector)),
      )
      expected.flatMap: (metadata, family, requiredClassifications) =>
        val familyErrors         = Option.when(metadata.family != family)(ValidationError.WrongControlFamily(family, metadata.family))
        val classificationErrors = requiredClassifications
          .filterNot(metadata.classifications.contains)
          .map(required => ValidationError.MissingClassification(family, required))
        familyErrors.toVector ++ classificationErrors

    private def duplicateErrors(bundle: Bundle): Vector[ValidationError] =
      duplicateRows("persons", bundle.persons.rows)(row => s"${row.region.value}|${row.sex}|${row.ageBand.code}|${row.residence}") ++
        duplicateRows("households", bundle.households.rows)(row => s"${row.region.value}|${row.size}|${row.composition}") ++
        duplicateRows("household-membership", bundle.householdMembership.rows)(row => s"${row.composition}|${row.memberRole}|${row.ageBand.code}") ++
        duplicateRows("demographic-labour", bundle.demographicLabour.rows)(row => s"${row.sex}|${row.ageBand.code}|${row.status}") ++
        duplicateRows("regional-labour", bundle.regionalLabour.rows)(row => s"${row.region.value}|${row.status}") ++
        duplicateRows("employment", bundle.employment.rows)(row => s"${row.residenceRegion.value}|${row.workplaceRegion.value}|${row.productionSector.value}")

    private def axisErrors(bundle: Bundle, classifications: Classifications): Vector[ValidationError] =
      val knownRegions = classifications.regions.toSet
      val knownAges    = classifications.ageBands.toSet
      val knownSectors = classifications.productionSectors.toSet
      bundle.persons.rows.flatMap: row =>
        Vector(
          Option.when(!knownRegions(row.region))(ValidationError.UnknownRegion("persons", row.region)),
          Option.when(!knownAges(row.ageBand))(ValidationError.UnknownAgeBand("persons", row.ageBand)),
        ).flatten
      ++ bundle.households.rows.flatMap: row =>
        Vector(Option.when(!knownRegions(row.region))(ValidationError.UnknownRegion("households", row.region))).flatten
      ++ bundle.householdMembership.rows.flatMap: row =>
        Vector(Option.when(!knownAges(row.ageBand))(ValidationError.UnknownAgeBand("household-membership", row.ageBand))).flatten
      ++ bundle.demographicLabour.rows.flatMap: row =>
        Vector(Option.when(!knownAges(row.ageBand))(ValidationError.UnknownAgeBand("demographic-labour", row.ageBand))).flatten
      ++ bundle.regionalLabour.rows.flatMap: row =>
        Vector(Option.when(!knownRegions(row.region))(ValidationError.UnknownRegion("regional-labour", row.region))).flatten
      ++ bundle.employment.rows.flatMap: row =>
        Vector(
          Option.when(!knownRegions(row.residenceRegion))(ValidationError.UnknownRegion("employment residence", row.residenceRegion)),
          Option.when(!knownRegions(row.workplaceRegion))(ValidationError.UnknownRegion("employment workplace", row.workplaceRegion)),
          Option.when(!knownSectors(row.productionSector))(ValidationError.UnknownProductionSector("employment", row.productionSector)),
        ).flatten

    private def labourAgeErrors(bundle: Bundle): Vector[ValidationError] =
      bundle.demographicLabour.rows.collect:
        case row if !labourStatusPermitted(row.status, row.ageBand) =>
          ValidationError.InvalidLabourStatusAgeBand(row.status, row.ageBand)

    private def populationScopeErrors(bundle: Bundle): Vector[ValidationError] =
      bundle.populationScope match
        case PopulationScope.AllUsualResidents => Vector.empty
        case scope                             =>
          bundle.persons.rows.collect:
            case row if row.residence == ResidenceType.CollectiveResidence =>
              ValidationError.PopulationScopeViolation(scope, row.residence)

    private def reconciliationChecks(bundle: Bundle): Vector[Reconciliation] =
      val membershipByComposition             = totalBy(bundle.householdMembership.rows)(_.composition)(row => BigInt(row.count.value))
      val householdCapacityByComposition      = totalBy(bundle.households.rows)(_.composition)(row => BigInt(row.count.value) * row.size)
      val personBySexAge                      = totalBy(bundle.persons.rows)(row => row.sex -> row.ageBand)(row => BigInt(row.count.value))
      val demographicLabourBySexAge           = totalBy(bundle.demographicLabour.rows)(row => row.sex -> row.ageBand)(row => BigInt(row.count.value))
      val personByRegion                      = totalBy(bundle.persons.rows)(_.region)(row => BigInt(row.count.value))
      val regionalLabourByRegion              = totalBy(bundle.regionalLabour.rows)(_.region)(row => BigInt(row.count.value))
      val demographicLabourByStatus           = totalBy(bundle.demographicLabour.rows)(_.status)(row => BigInt(row.count.value))
      val regionalLabourByStatus              = totalBy(bundle.regionalLabour.rows)(_.status)(row => BigInt(row.count.value))
      val employedByRegion                    = totalBy(bundle.regionalLabour.rows.filter(_.status == LabourStatus.Employed))(_.region)(row => BigInt(row.count.value))
      val primaryAssignmentsByResidenceRegion = totalBy(bundle.employment.rows)(_.residenceRegion)(row => BigInt(row.count.value))
      val personMembershipTolerance           = combinedTolerance(bundle.persons, bundle.householdMembership)
      val householdMembershipTolerance        = combinedTolerance(bundle.households, bundle.householdMembership)
      val personLabourTolerance               = combinedTolerance(bundle.persons, bundle.demographicLabour)
      val regionalLabourTolerance             = combinedTolerance(bundle.persons, bundle.regionalLabour)
      val labourMarginTolerance               = combinedTolerance(bundle.demographicLabour, bundle.regionalLabour)
      val employmentTolerance                 = combinedTolerance(bundle.regionalLabour, bundle.employment)

      Vector(
        Reconciliation(
          "private-person-membership",
          total(bundle.persons.rows.filter(_.residence == ResidenceType.PrivateHousehold).map(row => BigInt(row.count.value))),
          total(bundle.householdMembership.rows.map(row => BigInt(row.count.value))),
          personMembershipTolerance,
        ),
      ) ++
        reconcileMaps("household-member-capacity", householdCapacityByComposition, membershipByComposition, householdMembershipTolerance) ++
        reconcileMaps("person-to-demographic-labour", personBySexAge, demographicLabourBySexAge, personLabourTolerance) ++
        reconcileMaps("person-to-regional-labour", personByRegion, regionalLabourByRegion, regionalLabourTolerance) ++
        reconcileMaps("demographic-to-regional-labour", demographicLabourByStatus, regionalLabourByStatus, labourMarginTolerance) ++
        reconcileMaps("employed-residents-to-primary-job-assignments", employedByRegion, primaryAssignmentsByResidenceRegion, employmentTolerance)

    private def duplicateRows[A](table: String, rows: Vector[A])(key: A => String): Vector[ValidationError] =
      rows
        .groupBy(key)
        .collect { case (duplicate, values) if values.size > 1 => ValidationError.DuplicateRow(table, duplicate) }
        .toVector
        .sortBy(_.toString)

    private def totalBy[A, K](rows: Vector[A])(key: A => K)(count: A => BigInt): Map[K, BigInt] =
      rows.groupMapReduce(key)(count)(_ + _)

    private def total(counts: Iterable[BigInt]): BigInt =
      counts.iterator.foldLeft(BigInt(0))(_ + _)

    private def reconcileMaps[K](id: String, expected: Map[K, BigInt], actual: Map[K, BigInt], tolerance: Long): Vector[Reconciliation] =
      (expected.keySet ++ actual.keySet).toVector
        .sortBy(_.toString)
        .map: key =>
          Reconciliation(s"$id:$key", expected.getOrElse(key, BigInt(0)), actual.getOrElse(key, BigInt(0)), tolerance)

    private def combinedTolerance[A, B](left: ControlTable[A], right: ControlTable[B]): Long =
      math.max(left.metadata.absoluteTolerance, right.metadata.absoluteTolerance)

    private def labourStatusPermitted(status: LabourStatus, ageBand: AgeBand): Boolean =
      status match
        case LabourStatus.Employed        => containedIn(ageBand, minimum = 15, maximum = 89)
        case LabourStatus.Unemployed      => containedIn(ageBand, minimum = 15, maximum = 74)
        case LabourStatus.Inactive        => containedIn(ageBand, minimum = 15, maximum = 89)
        case LabourStatus.NotApplicable   => ageBand.maxInclusive.exists(_ < 15)
        case LabourStatus.NonBaelResidual => ageBand.minInclusive >= 90

    private def containedIn(ageBand: AgeBand, minimum: Int, maximum: Int): Boolean =
      ageBand.minInclusive >= minimum && ageBand.maxInclusive.exists(_ <= maximum)

  private def nonOverlapping(ageBands: Vector[AgeBand]): Boolean =
    val sorted = ageBands.sortBy(_.minInclusive)
    sorted
      .zip(sorted.drop(1))
      .forall: (left, right) =>
        left.maxInclusive.exists(_ < right.minInclusive)

end PopulationControlSchema
