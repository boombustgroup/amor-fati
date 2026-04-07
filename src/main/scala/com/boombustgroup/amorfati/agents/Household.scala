package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.networks.Network
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

import scala.util.Random

import com.boombustgroup.amorfati.fp.FixedPointBase.bankerRound

// ---- Top-level types (widely referenced, kept flat) ----

/** Employment/activity status of an individual household. */
enum HhStatus:
  case Employed(firmId: FirmId, sectorIdx: SectorIdx, wage: PLN)       // employed at firm, earning wage
  case Unemployed(monthsUnemployed: Int)                               // unemployed for N months (zasilek eligible)
  case Retraining(monthsLeft: Int, targetSector: SectorIdx, cost: PLN) // transitioning to target sector
  case Bankrupt // absorbing barrier

/** Per-bank lending and deposit rates for individual HH mode. */
case class BankRates(
    lendingRates: Vector[Rate], // annual lending rate per bank (index = BankId)
    depositRates: Vector[Rate], // annual deposit rate per bank (index = BankId)
)

/** Per-bank HH flow accumulator for multi-bank mode (one per BankId). */
case class PerBankFlow(
    income: PLN,              // total income (incl. deposit interest)
    consumption: PLN,         // total consumption (goods + rent)
    debtService: PLN,         // total mortgage/secured debt service
    depositInterest: PLN,     // total deposit interest paid
    consumerDebtService: PLN, // consumer (unsecured) debt service
    consumerOrigination: PLN, // new consumer loans originated
    consumerDefault: PLN,     // consumer loan defaults (bankruptcy write-offs)
    consumerPrincipal: PLN,   // consumer loan principal repaid
)

object PerBankFlow:
  val zero: PerBankFlow = PerBankFlow(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

object Household:
  private inline def scaledDivRaw(numerator: BigInt, denominator: BigInt): Long =
    if denominator == 0 then 0L
    else
      val scaled         = numerator * BigInt(com.boombustgroup.amorfati.fp.FixedPointBase.Scale)
      val quotient       = scaled / denominator
      val remainder      = (scaled % denominator).abs
      val denominatorAbs = denominator.abs
      val twiceRemainder = remainder * 2
      if twiceRemainder < denominatorAbs then quotient.toLong
      else
        val resultSign = scaled.signum * denominator.signum
        if twiceRemainder > denominatorAbs then (quotient + resultSign).toLong
        else if quotient % 2 == 0 then quotient.toLong
        else (quotient + resultSign).toLong

  // ---- Named constants ----

  // MPC sampling bounds (Beta distribution, NBP household survey)
  private val MpcFloor   = Share(0.5)
  private val MpcCeiling = Share(0.98)

  // Social network precautionary saving
  private val NeighborDistressThreshold    = Share(0.30) // fraction of neighbors in distress that triggers adj
  private val NeighborDistressConsAdj      = Share(0.90) // consumption multiplier when distress exceeds threshold
  private val NeighborDistressRetrainBoost = Share(0.05) // additional retraining prob when neighbors distressed

  // Labor market
  private val UnemploymentRetrainingThreshold = 6 // months unemployed before eligible for retraining
  private val PostFailedRetrainingMonths      = 7 // months assigned after failed retraining (duration + 1)

  // Consumer credit
  private val DisposableWageThreshold = Share(0.3) // disposable/wage ratio below which HH may borrow
  private val MinConsumerLoanSize     = PLN(100.0) // minimum loan size (PLN)
  private val ConsumerDebtInitFrac    = Share(0.3) // init consumer debt as fraction of mortgage draw

  // Init sampling
  private val GpwEquityInitFrac     = Share(0.05)  // fraction of savings allocated to GPW equity at init
  private val SectorSkillBonusCoeff = Scalar(0.02) // coefficient for sector-specific skill bonus (log sigma)
  private val SectorSkillBonusMax   = Scalar(0.1)  // maximum sector-specific skill bonus

  // Aggregates
  private val ImportRatioCap   = Share(0.65) // cap on import ratio applied to goods consumption
  private val PovertyRate50Pct = Share(0.50) // poverty line at 50% of median income (EU AROP)
  private val PovertyRate30Pct = Share(0.30) // poverty line at 30% of median income (deep poverty)
  private val ConsumptionP10   = Share(0.10) // P10 percentile index
  private val ConsumptionP90   = Share(0.90) // P90 percentile index

  // ---- Individual household ----

  /** Full state of a single household agent, carried across simulation months.
    */
  case class State(
      id: HhId,                                            // unique household identifier
      savings: PLN,                                        // liquid savings (bank deposits)
      debt: PLN,                                           // outstanding secured (mortgage) debt
      monthlyRent: PLN,                                    // monthly rent payment (to landlord / housing market)
      skill: Share,                                        // labor productivity multiplier [0,1], decays during unemployment
      healthPenalty: Share,                                // cumulative health penalty from long-term unemployment (scarring)
      mpc: Share,                                          // marginal propensity to consume (Beta-sampled at init)
      status: HhStatus,                                    // current employment/activity status
      socialNeighbors: Array[HhId],                        // Watts-Strogatz social network neighbor IDs
      bankId: BankId,                                      // index into Banking.State.banks (multi-bank)
      equityWealth: PLN,                                   // value of GPW equity holdings
      lastSectorIdx: SectorIdx,                            // last sector employed in (-1 = never)
      isImmigrant: Boolean,                                // immigrant status for wage discount + remittances
      numDependentChildren: Int,                           // children ≤ 18 for 800+ social transfers
      consumerDebt: PLN,                                   // outstanding unsecured consumer loan
      education: Int,                                      // education level: 0=Primary, 1=Vocational, 2=Secondary, 3=Tertiary
      taskRoutineness: Share,                              // how routine is this worker's task bundle [0,1] (Acemoglu & Restrepo 2020)
      wageScar: Share,                                     // persistent wage penalty from unemployment spell (Jacobson et al. 1993)
      financialDistressMonths: Int = 0,                    // consecutive months of deep financial distress
      contractType: ContractType = ContractType.Permanent, // employment contract type (Kodeks Pracy / umowa zlecenie / B2B)
      region: Region = Region.Central,                     // NUTS-1 macroregion (geographic labor market)
  )

  /** Aggregate statistics computed from individual households (Paper-06). */
  case class Aggregates(
      employed: Int,                 // count of employed HH
      unemployed: Int,               // count of unemployed HH
      retraining: Int,               // count of HH in retraining
      bankrupt: Int,                 // count of bankrupt HH
      totalIncome: PLN,              // aggregate income (wages + benefits + interest + transfers)
      consumption: PLN,              // aggregate consumption (goods + rent)
      domesticConsumption: PLN,      // domestic component of consumption
      importConsumption: PLN,        // import component of consumption
      marketWage: PLN,               // current market-clearing wage
      reservationWage: PLN,          // minimum acceptable wage for job search
      giniIndividual: Share,         // Gini of income distribution
      giniWealth: Share,             // Gini of wealth (savings) distribution
      meanSavings: PLN,              // mean savings across all HH
      medianSavings: PLN,            // median savings across all HH
      povertyRate50: Share,          // share with income < 50% median (EU AROP)
      bankruptcyRate: Share,         // share of bankrupt HH
      meanSkill: Share,              // mean skill of alive (non-bankrupt) HH
      meanHealthPenalty: Share,      // mean health scarring of alive HH
      retrainingAttempts: Int,       // retraining attempts this month
      retrainingSuccesses: Int,      // successful retraining completions this month
      consumptionP10: PLN,           // 10th percentile of consumption
      consumptionP50: PLN,           // median consumption
      consumptionP90: PLN,           // 90th percentile of consumption
      meanMonthsToRuin: Scalar,      // mean months until bankruptcy (placeholder)
      povertyRate30: Share,          // share with income < 30% median (deep poverty)
      totalRent: PLN,                // aggregate rent payments
      totalDebtService: PLN,         // aggregate secured debt service
      totalUnempBenefits: PLN,       // aggregate unemployment benefits paid
      totalDepositInterest: PLN,     // aggregate deposit interest received
      crossSectorHires: Int,         // cross-sector hires this month
      voluntaryQuits: Int,           // voluntary quits (cross-sector search)
      sectorMobilityRate: Share,     // fraction employed in different sector than last
      totalRemittances: PLN,         // aggregate remittances sent abroad
      totalPit: PLN,                 // aggregate PIT paid
      totalSocialTransfers: PLN,     // aggregate 800+ social transfers
      totalConsumerDebtService: PLN, // aggregate consumer debt service
      totalConsumerOrigination: PLN, // aggregate new consumer loans
      totalConsumerDefault: PLN,     // aggregate consumer loan defaults
      totalConsumerPrincipal: PLN,   // aggregate consumer loan principal repaid
  ):
    def unemploymentRate(totalPopulation: Int): Share =
      if totalPopulation <= 0 then Share.Zero
      else Share.fraction(totalPopulation - employed, totalPopulation)

  // ---- Init ----

  object Init:

    /** Create individual households with multi-bank assignment. A
      * NAIRU-fraction of households start as Unemployed(0) so the economy
      * initializes near steady state rather than overheated.
      */
    @boundaryEscape
    def create(rng: Random, firms: Vector[Firm.State])(using p: SimParams): Vector[State] =
      inline def shareRaw(s: Share): Double = s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
      val hhCount                           = firms.map(Firm.workerCount).sum
      val hhNetwork                         = Network.wattsStrogatz(hhCount, p.household.socialK, shareRaw(p.household.socialP), rng)
      val hhs                               = initialize(hhCount, firms, hhNetwork, rng)
      // Assign households to same bank as their employer
      val banked                            = hhs.map: h =>
        h.status match
          case HhStatus.Employed(fid, _, _) if fid.toInt < firms.length => h.copy(bankId = firms(fid.toInt).bankId)
          case _                                                        => h
      // Set NAIRU fraction as unemployed — prevents overheated init
      val nUnemployed                       = (hhCount.toLong * p.monetary.nairu.toLong / 10000L).toInt
      val toUnemploy                        = rng.shuffle(banked.indices.toVector).take(nUnemployed).toSet
      banked.zipWithIndex.map: (h, i) =>
        if toUnemploy.contains(i) then
          h.status match
            case HhStatus.Employed(_, sector, _) => h.copy(status = HhStatus.Unemployed(0), lastSectorIdx = sector)
            case _                               => h
        else h

    /** Initialize households, all employed, assigned proportionally to firm
      * sizes.
      */
    def initialize(
        nHouseholds: Int,
        firms: Vector[Firm.State],
        socialNetwork: Array[Array[Int]],
        rng: Random,
    )(using p: SimParams): Vector[State] =
      // Expand alive firms into (firm, sectorIdx) per worker slot, capped at nHouseholds
      val assignments: Vector[(Firm.State, SectorIdx)] =
        firms
          .filter(Firm.isAlive)
          .flatMap(f => Vector.fill(Firm.workerCount(f))((f, f.sector)))
          .take(nHouseholds)

      assignments.zipWithIndex.map { case ((firm, sectorIdx), hhId) =>
        sampleHousehold(hhId, firm, sectorIdx, socialNetwork, rng)
      }

    /** Sample attributes for a single household from init distributions. */
    @boundaryEscape
    private def sampleHousehold(
        hhId: Int,
        firm: Firm.State,
        sectorIdx: SectorIdx,
        socialNetwork: Array[Array[Int]],
        rng: Random,
    )(using p: SimParams): State =
      inline def plnRaw(amount: PLN): Double = amount.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
      val savings: PLN                       = PLN(Math.exp(p.household.savingsMu + p.household.savingsSigma * rng.nextGaussian()))
      val debt: PLN                          =
        if p.household.debtFraction.sampleBelow(rng) then PLN(Math.exp(p.household.debtMu + p.household.debtSigma * rng.nextGaussian()))
        else PLN.Zero
      val baseRent: PLN                      = PLN((plnRaw(p.household.rentMean) + plnRaw(p.household.rentStd) * rng.nextGaussian()).max(plnRaw(p.household.rentFloor)))
      val rent: PLN                          = baseRent * firm.region.housingCostIndex
      val mpc                                = Distributions.betaSample(p.household.mpcAlpha, p.household.mpcBeta, rng)
      val (edu, skill)                       = sampleEducationAndSkill(sectorIdx, rng)
      val wage: PLN                          = p.household.baseWage * Region.normalizedWageMultiplier(firm.region) * p.sectorDefs(sectorIdx.toInt).wageMultiplier * skill
      val eqWealth: PLN                      =
        if p.equity.hhEquityFrac.sampleBelow(rng) then savings * GpwEquityInitFrac
        else PLN.Zero
      val numChildren                        = Distributions.poissonSample(p.fiscal.social800ChildrenPerHh, rng)
      val consDebt: PLN                      =
        if p.household.debtFraction.sampleBelow(rng) then PLN(Math.exp(p.household.debtMu + p.household.debtSigma * rng.nextGaussian())) * ConsumerDebtInitFrac
        else PLN.Zero
      val routineness                        = sampleTaskRoutineness(edu, sectorIdx, rng)
      State(
        id = HhId(hhId),
        savings = savings,
        debt = debt,
        monthlyRent = rent,
        skill = skill,
        healthPenalty = Share.Zero,
        mpc = Share(mpc).clamp(MpcFloor, MpcCeiling),
        status = HhStatus.Employed(firm.id, sectorIdx, wage),
        socialNeighbors =
          if hhId < socialNetwork.length then socialNetwork(hhId).map(HhId(_)) else Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = eqWealth,
        lastSectorIdx = sectorIdx,
        isImmigrant = false,
        numDependentChildren = numChildren,
        consumerDebt = consDebt,
        education = edu,
        taskRoutineness = routineness,
        wageScar = Share.Zero,
        region = firm.region,
      )

    /** Sample education level and skill for a sector, clamped to edu range. */
    @boundaryEscape
    private def sampleEducationAndSkill(sectorIdx: SectorIdx, rng: Random)(using p: SimParams): (Int, Share) =
      inline def shareRaw(s: Share): Double = s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
      inline def sigmaRaw(s: Sigma): Double = s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
      val edu                               = p.social.drawEducation(sectorIdx.toInt, rng)
      val (skillFloorS, skillCeilingS)      = p.social.eduSkillRange(edu)
      val skillFloor                        = shareRaw(skillFloorS)
      val skillCeiling                      = shareRaw(skillCeilingS)
      val sectorSigma                       = sigmaRaw(p.sectorDefs(sectorIdx.toInt).sigma)
      val baseSkill                         = skillFloor + (skillCeiling - skillFloor) * rng.nextDouble()
      val sectorBonus                       = Math.min(
        SectorSkillBonusMax.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD,
        (SectorSkillBonusCoeff * Scalar(Math.log(sectorSigma))).toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD,
      )
      val skill                             = Math.max(skillFloor, Math.min(skillCeiling, baseSkill + sectorBonus))
      (edu, Share(skill))

    /** Sample task routineness based on education and sector sigma.
      *
      * High-σ sectors (BPO) have more automatable routine tasks; high education
      * workers tend toward cognitive (non-routine) tasks. Acemoglu & Restrepo
      * 2020.
      *
      * routineness = base(edu) + σ-bonus, clamped to [0.05, 0.95], with noise.
      */
    @boundaryEscape
    private[agents] def sampleTaskRoutineness(edu: Int, sectorIdx: SectorIdx, rng: Random)(using p: SimParams): Share =
      inline def shareRaw(s: Share): Double = s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
      inline def sigmaRaw(s: Sigma): Double = s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
      val eduBase                           = shareRaw(p.labor.sbtcEduRoutineness(edu))
      val sigma                             = sigmaRaw(p.sectorDefs(sectorIdx.toInt).sigma)
      val sigmaAdj                          = 0.05 * Math.log(sigma) / Math.log(10.0)
      val noise                             = 0.05 * (rng.nextDouble() - 0.5)
      Share(eduBase + sigmaAdj + noise).clamp(Share(0.05), Share(0.95))

  // ---- Step flow totals (immutable, folded from per-HH results) ----

  /** Accumulated flow totals from one step. PLN is Long — addition is exact, no
    * Kahan compensation needed.
    */
  private class StepTotals:
    private var incomeAcc: PLN      = PLN.Zero
    private var benefitAcc: PLN     = PLN.Zero
    private var debtSvcAcc: PLN     = PLN.Zero
    private var depIntAcc: PLN      = PLN.Zero
    private var goodsConsAcc: PLN   = PLN.Zero
    private var rentAcc: PLN        = PLN.Zero
    private var remitAcc: PLN       = PLN.Zero
    private var pitAcc: PLN         = PLN.Zero
    private var socialAcc: PLN      = PLN.Zero
    private var ccDebtSvcAcc: PLN   = PLN.Zero
    private var ccOrigAcc: PLN      = PLN.Zero
    private var ccDefaultAcc: PLN   = PLN.Zero
    private var ccPrincipalAcc: PLN = PLN.Zero
    var retrainingAttempts: Int     = 0
    var retrainingSuccesses: Int    = 0
    var voluntaryQuits: Int         = 0

    def add(r: HhMonthlyResult): Unit =
      incomeAcc = incomeAcc + r.income
      benefitAcc = benefitAcc + r.benefit
      debtSvcAcc = debtSvcAcc + r.debtService
      depIntAcc = depIntAcc + r.depositInterest
      goodsConsAcc = goodsConsAcc + r.consumption
      rentAcc = rentAcc + r.rent
      remitAcc = remitAcc + r.remittance
      pitAcc = pitAcc + r.pitTax
      socialAcc = socialAcc + r.socialTransfer
      ccDebtSvcAcc = ccDebtSvcAcc + r.credit.debtService
      ccOrigAcc = ccOrigAcc + r.credit.newLoan
      ccDefaultAcc = ccDefaultAcc + r.credit.defaultAmt
      ccPrincipalAcc = ccPrincipalAcc + r.credit.principal
      retrainingAttempts += r.retrainingAttempt
      retrainingSuccesses += r.retrainingSuccess
      voluntaryQuits += r.voluntaryQuit

    def income: PLN              = incomeAcc
    def unempBenefits: PLN       = benefitAcc
    def debtService: PLN         = debtSvcAcc
    def depositInterest: PLN     = depIntAcc
    def goodsConsumption: PLN    = goodsConsAcc
    def rent: PLN                = rentAcc
    def remittances: PLN         = remitAcc
    def pit: PLN                 = pitAcc
    def socialTransfers: PLN     = socialAcc
    def consumerDebtService: PLN = ccDebtSvcAcc
    def consumerOrigination: PLN = ccOrigAcc
    def consumerDefault: PLN     = ccDefaultAcc
    def consumerPrincipal: PLN   = ccPrincipalAcc

  /** Build per-bank flow vector from (BankId, HhMonthlyResult) pairs. PLN is
    * Long — addition is exact, no Kahan needed.
    */
  private def buildPerBankFlows(flows: Vector[(BankId, HhMonthlyResult)], nBanks: Int): Vector[PerBankFlow] =
    val acc = Array.fill(nBanks)(PerBankFlow.zero)
    flows.foreach: (bankId, r) =>
      val b   = bankId.toInt
      val cur = acc(b)
      acc(b) = PerBankFlow(
        income = cur.income + r.income,
        consumption = cur.consumption + r.consumption + r.rent,
        debtService = cur.debtService + r.debtService,
        depositInterest = cur.depositInterest + r.depositInterest,
        consumerDebtService = cur.consumerDebtService + r.credit.debtService,
        consumerOrigination = cur.consumerOrigination + r.credit.newLoan,
        consumerDefault = cur.consumerDefault + r.credit.defaultAmt,
        consumerPrincipal = cur.consumerPrincipal + r.credit.principal,
      )
    acc.toVector

  // ---- Extracted per-HH pipeline types ----

  /** Consumer credit result for a single household in one month. */
  private case class CreditResult(
      debtService: PLN, // total consumer debt service (amortization + interest)
      principal: PLN,   // principal component of debt service
      newLoan: PLN,     // newly originated consumer loan amount
      defaultAmt: PLN,  // amount defaulted on bankruptcy (0 if not bankrupt)
      updatedDebt: PLN, // outstanding consumer debt after this month's flows
  )

  /** Per-HH monthly result — updated state + all flow variables for
    * aggregation.
    */
  private case class HhMonthlyResult(
      newState: State,        // updated household state
      income: PLN,            // gross income net of PIT plus social transfers
      benefit: PLN,           // unemployment benefit component
      consumption: PLN,       // total consumption (goods + wealth effect, before rent)
      debtService: PLN,       // secured (mortgage) debt service
      depositInterest: PLN,   // deposit interest received
      remittance: PLN,        // remittance sent abroad (immigrants only)
      pitTax: PLN,            // PIT paid
      socialTransfer: PLN,    // 800+ social transfer received
      credit: CreditResult,   // consumer credit flows
      voluntaryQuit: Int,     // 1 if voluntary cross-sector quit, 0 otherwise
      retrainingAttempt: Int, // 1 if retraining attempted, 0 otherwise
      retrainingSuccess: Int, // 1 if retraining succeeded, 0 otherwise
      equityWealth: PLN,      // updated equity wealth after revaluation
      rent: PLN,              // monthly rent payment
  )

  // ---- Logic ----

  /** Monthly PIT: progressive Polish brackets (12%/32%), minus kwota wolna. PIT
    * base = gross income − ZUS employee contribution (Art. 26 ustawy o PIT).
    */
  def computeMonthlyPit(monthlyIncome: PLN)(using p: SimParams): PLN =
    if monthlyIncome <= PLN.Zero then PLN.Zero
    else
      val afterZus   = monthlyIncome - monthlyIncome * p.social.zusEmployeeRate
      val annualized = afterZus * Multiplier(12.0)
      val grossTax   =
        if annualized <= p.fiscal.pitBracket1Annual then annualized * p.fiscal.pitRate1
        else
          p.fiscal.pitBracket1Annual * p.fiscal.pitRate1 +
            (annualized - p.fiscal.pitBracket1Annual) * p.fiscal.pitRate2
      (grossTax - p.fiscal.pitTaxCreditAnnual).max(PLN.Zero) / 12L

  /** Compute 800+ social transfer (PIT-exempt, lump-sum per child ≤ 18). */
  def computeSocialTransfer(numChildren: Int)(using p: SimParams): PLN =
    if numChildren <= 0 then PLN.Zero
    else numChildren * p.fiscal.social800

  /** Unemployment benefit (zasilek): 1500 PLN m1-3, 1200 PLN m4-6, 0 after. */
  def computeBenefit(monthsUnemployed: Int)(using p: SimParams): PLN =
    if monthsUnemployed <= p.fiscal.govBenefitDuration / 2 then p.fiscal.govBenefitM1to3
    else if monthsUnemployed <= p.fiscal.govBenefitDuration then p.fiscal.govBenefitM4to6
    else PLN.Zero

  /** Voluntary cross-sector search for employed HH → (newStatus, quitFlag). */
  private def tryVoluntarySearch(
      hh: State,
      status: HhStatus.Employed,
      sectorWages: Vector[PLN],
      sectorVacancies: Vector[Int],
      rng: Random,
  )(using p: SimParams): (HhStatus, Int) =
    if !p.labor.voluntarySearchProb.sampleBelow(rng) then return (status, 0)
    val targetSector      =
      SectoralMobility.selectTargetSector(status.sectorIdx.toInt, sectorWages, sectorVacancies, p.labor.frictionMatrix, p.labor.vacancyWeight, rng)
    val targetAvgWage     = sectorWages(targetSector)
    val wageThresholdMult = Multiplier.One + p.labor.voluntaryWageThreshold.toMultiplier
    if targetAvgWage <= status.wage * wageThresholdMult then return (status, 0)
    val friction          = p.labor.frictionMatrix(status.sectorIdx.toInt)(targetSector)
    if friction < p.labor.adjacentFrictionMax then (HhStatus.Unemployed(0), 1)
    else
      val rp = SectoralMobility.frictionAdjustedParams(friction, p.labor.frictionDurationMult, p.labor.frictionCostMult)
      if hh.savings > rp.cost then (HhStatus.Retraining(rp.duration, SectorIdx(targetSector), rp.cost), 1)
      else (status, 0)

  /** Retraining for unemployed HH → (newStatus, attemptFlag, successFlag). */
  private def tryRetraining(
      hh: State,
      status: HhStatus,
      neighborDistress: Share,
      sectorWages: Option[Vector[PLN]],
      sectorVacancies: Option[Vector[Int]],
      rng: Random,
  )(using p: SimParams): (HhStatus, Int, Int) =
    status match
      case HhStatus.Unemployed(months) if months > UnemploymentRetrainingThreshold && p.household.retrainingEnabled =>
        val retrainProb = p.household.retrainingProb +
          (if neighborDistress > NeighborDistressThreshold then NeighborDistressRetrainBoost else Share.Zero)
        if hh.savings > p.household.retrainingCost && retrainProb.sampleBelow(rng) then
          if sectorWages.isDefined then
            val sw           = sectorWages.get
            val sv           = sectorVacancies.get
            val fromSector   = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
            val targetSector = SectoralMobility.selectTargetSector(fromSector, sw, sv, p.labor.frictionMatrix, p.labor.vacancyWeight, rng)
            val friction     = p.labor.frictionMatrix(fromSector)(targetSector)
            val rp           = SectoralMobility.frictionAdjustedParams(friction, p.labor.frictionDurationMult, p.labor.frictionCostMult)
            if hh.savings > rp.cost then (HhStatus.Retraining(rp.duration, SectorIdx(targetSector), rp.cost), 1, 0)
            else (status, 0, 0)
          else
            val targetSector = rng.nextInt(p.sectorDefs.length)
            (HhStatus.Retraining(p.household.retrainingDuration, SectorIdx(targetSector), p.household.retrainingCost), 1, 0)
        else (status, 0, 0)

      case HhStatus.Retraining(monthsLeft, targetSector, cost) =>
        if monthsLeft <= 1 then
          val afterSkill      = applySkillDecay(hh, status)
          val afterHealth     = applyHealthScarring(hh, status)
          // baseSuccess × skill × (1 − healthPenalty) × eduMultiplier
          val skillHealthProb = p.household.retrainingBaseSuccess * afterSkill * (Share.One - afterHealth)
          val eduMult         = p.social.eduRetrainMultiplier(hh.education)
          val baseSuccessProb = (skillHealthProb * eduMult).toShare // Multiplier → Share (probability)
          val successProb     =
            val fromSector = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
            val friction   = p.labor.frictionMatrix(fromSector)(targetSector.toInt)
            baseSuccessProb * (Share.One - friction * SectoralMobility.FrictionSuccessDiscount)
          if successProb.sampleBelow(rng) then (HhStatus.Unemployed(0), 0, 1)
          else (HhStatus.Unemployed(PostFailedRetrainingMonths), 0, 0)
        else (HhStatus.Retraining(monthsLeft - 1, targetSector, cost), 0, 0)

      case _ => (status, 0, 0)

  /** Consumer credit for one HH: debt service, origination, principal. */
  private def processConsumerCredit(
      hh: State,
      income: PLN,
      disposable: PLN,
      debtService: PLN,
      world: World,
      bankRates: Option[BankRates],
      rng: Random,
  )(using p: SimParams): CreditResult =
    val consumerRate: Rate = bankRates match
      case Some(br) => br.lendingRates(hh.bankId.toInt) + p.household.ccSpread
      case None     => world.nbp.referenceRate + p.household.ccSpread
    val consumerDebtSvc    = hh.consumerDebt * (p.household.ccAmortRate + consumerRate.monthly)
    val consumerPrin       = hh.consumerDebt * p.household.ccAmortRate

    val newConsumerLoan = hh.status match
      case HhStatus.Employed(_, _, wage)                                             =>
        val stressed = disposable < wage * DisposableWageThreshold
        val eligible = stressed && p.household.ccEligRate.sampleBelow(rng)
        if !eligible then PLN.Zero
        else
          val totalDbtSvc   = debtService + consumerDebtSvc
          val existingDti   = if income > PLN.Zero then Share(totalDbtSvc / income) else Share.One
          val headroomShare = (p.household.ccMaxDti - existingDti).max(Share.Zero)
          val headroom      = income * headroomShare
          val desired       = headroom.min(p.household.ccMaxLoan)
          if desired > MinConsumerLoanSize then desired else PLN.Zero
      case HhStatus.Unemployed(_) | HhStatus.Retraining(_, _, _) | HhStatus.Bankrupt => PLN.Zero

    val updatedDebt = (hh.consumerDebt + newConsumerLoan - consumerDebtSvc).max(PLN.Zero)

    CreditResult(
      debtService = consumerDebtSvc,
      principal = consumerPrin,
      newLoan = newConsumerLoan,
      defaultAmt = PLN.Zero,
      updatedDebt = updatedDebt,
    )

  /** Intermediate result after income/consumption pipeline, before branching.
    */
  private case class MonthlyFlows(
      hh: State,
      income: PLN,
      benefit: PLN,
      newStatus: HhStatus,
      debtService: PLN,
      depositInterest: PLN,
      remittance: PLN,
      pitTax: PLN,
      socialTransfer: PLN,
      credit: CreditResult,
      consumption: PLN,
      newEquityWealth: PLN,
      newSavings: PLN,
      newDebt: PLN,
      neighborDistress: Share,
  )

  private def bankruptcyFloor(f: MonthlyFlows)(using p: SimParams): PLN =
    val essentialOutflows = (f.hh.monthlyRent + f.debtService + f.credit.debtService).max(f.hh.monthlyRent)
    essentialOutflows * Multiplier(p.household.bankruptcyThreshold)

  /** Per-HH monthly pipeline: income → tax → credit → consumption → equity. */
  private def computeMonthlyFlows(
      hh: State,
      world: World,
      rng: Random,
      bankRates: Option[BankRates],
      equityIndexReturn: Rate,
      distressedIds: java.util.BitSet,
  )(using p: SimParams): MonthlyFlows =
    val (baseIncome, benefit, newStatus) = computeIncome(hh)

    // Variable-rate debt service (monetary transmission channel 1)
    val debtSvcRate: Rate = bankRates match
      case Some(br) => p.household.baseAmortRate.toRate + br.lendingRates(hh.bankId.toInt).monthly
      case None     => p.household.debtServiceRate.toRate

    // Deposit interest (monetary transmission channel 2)
    val depInterest: PLN = bankRates match
      case Some(br) => hh.savings * br.depositRates(hh.bankId.toInt).monthly
      case None     => PLN.Zero

    val grossIncome     = baseIncome + depInterest.max(PLN.Zero)
    val pitTax          = computeMonthlyPit(grossIncome)
    val socialTransfer  = computeSocialTransfer(hh.numDependentChildren)
    val income          = grossIncome - pitTax + socialTransfer
    val thisDebtService = hh.debt * debtSvcRate

    val remittance =
      if hh.isImmigrant then income * p.immigration.remitRate
      else PLN.Zero

    val obligations         = hh.monthlyRent + thisDebtService + remittance
    val disposablePreCredit = (income - obligations).max(PLN.Zero)
    val credit              = processConsumerCredit(hh, income, disposablePreCredit, thisDebtService, world, bankRates, rng)
    val fullObligations     = obligations + credit.debtService
    val disposable          = (income - fullObligations).max(PLN.Zero)
    val savingsDrawdown     = computeSavingsDrawdown(hh, income, newStatus)
    val consumptionBudget   = disposable + credit.newLoan + savingsDrawdown
    val consumption         = consumptionBudget * hh.mpc

    // Social network precautionary effect
    val neighborDistress = neighborDistressRatioFast(hh, distressedIds)
    val consumptionAdj   =
      if neighborDistress > NeighborDistressThreshold then consumption * NeighborDistressConsAdj else consumption

    // Wealth effects: equity (GPW) + housing (Meen HPI)
    val newEquityWealth       = (hh.equityWealth * (Multiplier.One + equityIndexReturn.toMultiplier)).max(PLN.Zero)
    val equityGain            = newEquityWealth - hh.equityWealth
    val equityBoost           =
      if equityGain > PLN.Zero then equityGain * p.equity.wealthEffectMpc
      else PLN.Zero
    val housingBoost          = world.real.housing.lastWealthEffect / world.derivedTotalPopulation.toLong.max(1L)
    val consumptionWithWealth = consumptionAdj + equityBoost + housingBoost

    MonthlyFlows(
      hh = hh,
      income = income,
      benefit = benefit,
      newStatus = newStatus,
      debtService = thisDebtService,
      depositInterest = depInterest.max(PLN.Zero),
      remittance = remittance,
      pitTax = pitTax,
      socialTransfer = socialTransfer,
      credit = credit,
      consumption = consumptionWithWealth,
      newEquityWealth = newEquityWealth,
      newSavings = hh.savings + income - fullObligations + credit.newLoan - consumptionWithWealth,
      newDebt = (hh.debt - thisDebtService).max(PLN.Zero),
      neighborDistress = neighborDistress,
    )

  /** Resolve flows into final HhMonthlyResult: bankruptcy or survival branch.
    */
  private def processHousehold(
      hh: State,
      world: World,
      rng: Random,
      bankRates: Option[BankRates],
      equityIndexReturn: Rate,
      sectorWages: Option[Vector[PLN]],
      sectorVacancies: Option[Vector[Int]],
      distressedIds: java.util.BitSet,
  )(using p: SimParams): HhMonthlyResult =
    val f              = computeMonthlyFlows(hh, world, rng, bankRates, equityIndexReturn, distressedIds)
    val distressMonths =
      if f.newSavings < bankruptcyFloor(f) then hh.financialDistressMonths + 1 else 0
    if distressMonths >= p.household.bankruptcyDistressMonths then resolveBankruptcy(f, distressMonths)
    else resolveSurvival(f, sectorWages, sectorVacancies, rng, distressMonths)

  /** Bankruptcy branch: write off consumer debt, zero equity. */
  private def resolveBankruptcy(f: MonthlyFlows, distressMonths: Int)(using p: SimParams): HhMonthlyResult =
    val ccDefaultAmt  = f.hh.consumerDebt * (Rate(1.0) - p.household.ccAmortRate) + f.credit.newLoan
    val creditWithDef = f.credit.copy(defaultAmt = ccDefaultAmt, updatedDebt = PLN.Zero)
    HhMonthlyResult(
      newState = f.hh.copy(
        savings = f.newSavings,
        debt = f.newDebt,
        consumerDebt = PLN.Zero,
        status = HhStatus.Bankrupt,
        equityWealth = PLN.Zero,
        financialDistressMonths = distressMonths,
      ),
      income = f.income,
      benefit = f.benefit,
      consumption = f.consumption,
      debtService = f.debtService,
      depositInterest = f.depositInterest,
      remittance = f.remittance,
      pitTax = f.pitTax,
      socialTransfer = f.socialTransfer,
      credit = creditWithDef,
      voluntaryQuit = 0,
      retrainingAttempt = 0,
      retrainingSuccess = 0,
      equityWealth = PLN.Zero,
      rent = f.hh.monthlyRent,
    )

  /** Survival branch: skill decay, labor transitions, state update. */
  private def resolveSurvival(
      f: MonthlyFlows,
      sectorWages: Option[Vector[PLN]],
      sectorVacancies: Option[Vector[Int]],
      rng: Random,
      distressMonths: Int,
  )(using p: SimParams): HhMonthlyResult =
    val afterSkill    = applySkillDecay(f.hh, f.newStatus)
    val afterHealth   = applyHealthScarring(f.hh, f.newStatus)
    val afterWageScar = applyWageScar(f.hh, f.newStatus)
    val afterMpc      = updateMpc(f.hh, f.income, f.newStatus)

    val (afterVoluntary, vQuit) = f.newStatus match
      case emp: HhStatus.Employed if sectorWages.isDefined =>
        tryVoluntarySearch(f.hh, emp, sectorWages.get, sectorVacancies.get, rng)
      case _                                               => (f.newStatus, 0)

    val (finalStatus, rAttempt, rSuccess) =
      tryRetraining(f.hh, afterVoluntary, f.neighborDistress, sectorWages, sectorVacancies, rng)

    val retrainingCostThisMonth = finalStatus match
      case HhStatus.Retraining(ml, _, cost) if ml == p.household.retrainingDuration - 1 => cost
      case _                                                                            => PLN.Zero

    HhMonthlyResult(
      newState = f.hh.copy(
        savings = f.newSavings - retrainingCostThisMonth,
        debt = f.newDebt,
        consumerDebt = f.credit.updatedDebt,
        skill = afterSkill,
        healthPenalty = afterHealth,
        wageScar = afterWageScar,
        mpc = afterMpc,
        status = finalStatus,
        equityWealth = f.newEquityWealth,
        financialDistressMonths = distressMonths,
      ),
      income = f.income,
      benefit = f.benefit,
      consumption = f.consumption,
      debtService = f.debtService,
      depositInterest = f.depositInterest,
      remittance = f.remittance,
      pitTax = f.pitTax,
      socialTransfer = f.socialTransfer,
      credit = f.credit,
      voluntaryQuit = vQuit,
      retrainingAttempt = rAttempt,
      retrainingSuccess = rSuccess,
      equityWealth = f.newEquityWealth,
      rent = f.hh.monthlyRent,
    )

  /** Monthly entry point: map processHousehold + accumulate + aggregate. */
  def step(
      households: Vector[State],
      world: World,
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Share,
      rng: Random,
      nBanks: Int = 1,
      bankRates: Option[BankRates] = None,
      equityIndexReturn: Rate = Rate.Zero,
      sectorWages: Option[Vector[PLN]] = None,
      sectorVacancies: Option[Vector[Int]] = None,
  )(using p: SimParams): (Vector[State], Aggregates, Option[Vector[PerBankFlow]]) =
    val distressedIds = buildDistressedSet(households)

    val mapped = households.map: hh =>
      if hh.status == HhStatus.Bankrupt then (hh, None) // absorbing barrier
      else
        val result = processHousehold(hh, world, rng, bankRates, equityIndexReturn, sectorWages, sectorVacancies, distressedIds)
        (result.newState, Some((hh.bankId, result)))

    val updated = mapped.map(_._1)
    val flows   = mapped.flatMap(_._2)
    val totals  = { val t = StepTotals(); flows.foreach((_, r) => t.add(r)); t }
    val agg     = computeAggregates(updated, marketWage, reservationWage, importAdj, totals)
    val pbf     = if bankRates.isDefined then Some(buildPerBankFlows(flows, nBanks)) else None
    (updated, agg, pbf)

  /** Pre-compute distressed HH set for O(1) neighbor lookups. */
  private def buildDistressedSet(households: Vector[State]): java.util.BitSet =
    val bits = new java.util.BitSet(households.length)
    var i    = 0
    while i < households.length do
      households(i).status match
        case HhStatus.Bankrupt | HhStatus.Unemployed(_) => bits.set(i)
        case _                                          =>
      i += 1
    bits

  /** Sector mobility rate: fraction of employed in different sector than last.
    */
  private def sectorMobilityRate(updated: Vector[State]): Share =
    val employed    = updated.flatMap: hh =>
      hh.status match
        case HhStatus.Employed(_, sec, _) => Some((hh.lastSectorIdx, sec))
        case _                            => None
    if employed.isEmpty then return Share.Zero
    val crossSector = employed.count((last, cur) => last.toInt >= 0 && last != cur)
    Share.fraction(crossSector, employed.length)

  /** Base income, benefit, and updated status for one HH. */
  private def computeIncome(hh: State)(using SimParams): (PLN, PLN, HhStatus) =
    hh.status match
      case HhStatus.Employed(firmId, sectorIdx, wage) =>
        (wage, PLN.Zero, hh.status)
      case HhStatus.Unemployed(months)                =>
        val benefit = computeBenefit(months)
        (benefit, benefit, HhStatus.Unemployed(months + 1))
      case HhStatus.Retraining(monthsLeft, _, cost)   =>
        (PLN.Zero, PLN.Zero, hh.status)
      case HhStatus.Bankrupt                          =>
        (PLN.Zero, PLN.Zero, HhStatus.Bankrupt)

  /** Skill decay for long-term unemployed (onset after scarringOnset months).
    */
  private def applySkillDecay(hh: State, status: HhStatus)(using p: SimParams): Share =
    status match
      case HhStatus.Unemployed(months) if months >= p.household.scarringOnset =>
        hh.skill * (Share.One - p.household.skillDecayRate)
      case _                                                                  => hh.skill

  /** Apply health scarring for long-term unemployed (cumulative, capped). */
  private def applyHealthScarring(hh: State, status: HhStatus)(using p: SimParams): Share =
    status match
      case HhStatus.Unemployed(months) if months >= p.household.scarringOnset =>
        (hh.healthPenalty + p.household.scarringRate).min(p.household.scarringCap)
      case _                                                                  => hh.healthPenalty

  /** Wage scar: accumulates during long-term unemployment, decays slowly once
    * reemployed. Jacobson, LaLonde & Sullivan 1993; Davis & von Wachter 2011.
    */
  private def applyWageScar(hh: State, status: HhStatus)(using p: SimParams): Share =
    status match
      case HhStatus.Unemployed(months) if months >= p.household.scarringOnset =>
        (hh.wageScar + p.household.wageScarRate).min(p.household.wageScarCap)
      case _: HhStatus.Employed                                               =>
        (hh.wageScar - p.household.wageScarDecay).max(Share.Zero)
      case _                                                                  => hh.wageScar

  /** State-dependent MPC: Carroll (1997) buffer-stock model.
    *
    * When savings/income > target → buffer is fat → MPC falls (more saving).
    * When buffer depleted → MPC rises (spend everything). Unemployed get an
    * additional boost (desperate spending from depleted buffers).
    */
  private[amorfati] def updateMpc(hh: State, income: PLN, status: HhStatus)(using p: SimParams): Share =
    val baseMpc = hh.mpc
    if income <= PLN.Zero then baseMpc
    else
      val targetSavings = income * Multiplier(p.household.bufferTargetMonths)
      val bufferRatio   = hh.savings.ratioTo(targetSavings).toMultiplier
      val deviation     = bufferRatio - Multiplier.One              // >0 = fat, <0 = depleted
      val adjustment    = p.household.bufferSensitivity * deviation // Coefficient × Multiplier → Share
      val bufferAdj     = (Share.One - adjustment).clamp(Share.Zero, Share.One)
      val unemployedAdj = status match
        case _: HhStatus.Unemployed => Share.One + p.household.mpcUnemployedBoost
        case _                      => Share.One
      (baseMpc * bufferAdj * unemployedAdj).clamp(MpcFloor, MpcCeiling)

  /** Controlled drawdown from liquid savings buffers.
    *
    * Employed households only draw from savings above their target buffer.
    * Stressed households may also draw from the lower half of the buffer, but
    * still keep a protected floor.
    */
  private[amorfati] def computeSavingsDrawdown(
      hh: State,
      cashOnHand: PLN,
      status: HhStatus,
  )(using p: SimParams): PLN =
    val targetIncome  = cashOnHand.max(p.household.baseReservationWage)
    val targetSavings = targetIncome * Multiplier(p.household.bufferTargetMonths)
    val protectedBuff = targetSavings * p.household.bufferProtectedShare
    status match
      case _: HhStatus.Unemployed | _: HhStatus.Retraining =>
        (hh.savings - protectedBuff).max(PLN.Zero) * p.household.bufferStressDrawdownRate
      case _: HhStatus.Employed | HhStatus.Bankrupt        =>
        (hh.savings - targetSavings).max(PLN.Zero) * p.household.bufferExcessDrawdownRate

  /** Fraction of social neighbors in distress (BitSet, O(k) per HH). */
  private def neighborDistressRatioFast(hh: State, distressedIds: java.util.BitSet): Share =
    if hh.socialNeighbors.isEmpty then Share.Zero
    else
      var count = 0
      var i     = 0
      while i < hh.socialNeighbors.length do
        if distressedIds.get(hh.socialNeighbors(i).toInt) then count += 1
        i += 1
      Share.fraction(count, hh.socialNeighbors.length)

  /** Public entry point for aggregate stats (used by BankingEconomics and
    * tests). Flow totals default to zero — only distribution stats are
    * computed.
    */
  def computeAggregates(
      households: Vector[State],
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Share,
      retrainingAttempts: Int,
      retrainingSuccesses: Int,
  )(using SimParams): Aggregates =
    computeAggregates(
      households,
      marketWage,
      reservationWage,
      importAdj,
      { val t = StepTotals(); t.retrainingAttempts = retrainingAttempts; t.retrainingSuccesses = retrainingSuccesses; t },
    )

  /** Aggregate stats: single-pass accumulation + sorted-array Gini/percentiles.
    * Merges per-HH distribution stats with flow totals from StepTotals in one
    * construction — no intermediate Aggregates + copy overwrite.
    */
  private def computeAggregates(
      households: Vector[State],
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Share,
      t: StepTotals,
  )(using p: SimParams): Aggregates =
    val n = households.length

    var nEmployed    = 0
    var nUnemployed  = 0
    var nRetraining  = 0
    var nBankrupt    = 0
    var sumSkill     = BigInt(0)
    var sumHealth    = BigInt(0)
    val incomes      = new Array[Long](n)
    val consumptions = new Array[Long](n)
    val savingsArr   = new Array[Long](n)

    // Hot path: O(N_hh) single-pass with mutable accumulators + in-place arrays.
    // Intentionally imperative — foldLeft with 9-field accumulator would be slower and less readable.
    var i = 0
    while i < n do
      val hh = households(i)
      hh.status match
        case HhStatus.Employed(_, _, wage) =>
          nEmployed += 1
          incomes(i) = wage.toLong
          sumSkill += BigInt(hh.skill.toLong)
          sumHealth += BigInt(hh.healthPenalty.toLong)
        case HhStatus.Unemployed(months)   =>
          nUnemployed += 1
          incomes(i) = computeBenefit(months).toLong
          sumSkill += BigInt(hh.skill.toLong)
          sumHealth += BigInt(hh.healthPenalty.toLong)
        case HhStatus.Retraining(_, _, _)  =>
          nRetraining += 1
          incomes(i) = 0L
          sumSkill += BigInt(hh.skill.toLong)
          sumHealth += BigInt(hh.healthPenalty.toLong)
        case HhStatus.Bankrupt             =>
          nBankrupt += 1
          incomes(i) = 0L

      val rentRaw       = hh.monthlyRent.toLong
      val debtSvcRaw    = (hh.debt * p.household.debtServiceRate).toLong
      val disposableRaw = math.max(0L, incomes(i) - rentRaw - debtSvcRaw)
      consumptions(i) = bankerRound(BigInt(disposableRaw) * BigInt(hh.mpc.toLong))
      savingsArr(i) = hh.savings.toLong
      i += 1

    val nAlive = n - nBankrupt

    // Sort each array once — reuse for Gini + percentiles + poverty
    java.util.Arrays.sort(incomes)
    java.util.Arrays.sort(savingsArr)
    java.util.Arrays.sort(consumptions)

    // Consumption split: flow totals (from StepTotals) are authoritative
    val totalConsumption = t.goodsConsumption + t.rent
    val importCons       = t.goodsConsumption * importAdj.min(ImportRatioCap)
    val domesticCons     = totalConsumption - importCons

    val medianIncomeRaw = if n > 0 then incomes(n / 2) else 0L

    Aggregates(
      employed = nEmployed,
      unemployed = nUnemployed,
      retraining = nRetraining,
      bankrupt = nBankrupt,
      totalIncome = t.income,
      consumption = totalConsumption,
      domesticConsumption = domesticCons,
      importConsumption = importCons,
      marketWage = marketWage,
      reservationWage = reservationWage,
      giniIndividual = giniSorted(incomes),
      giniWealth = giniSorted(savingsArr),
      meanSavings = if n > 0 then PLN.fromRaw((savingsArr.foldLeft(BigInt(0))((acc, raw) => acc + raw) / n).toLong) else PLN.Zero,
      medianSavings = if n > 0 then PLN.fromRaw(savingsArr(n / 2)) else PLN.Zero,
      povertyRate50 =
        if n > 0 && medianIncomeRaw > 0L then Share.fraction(lowerBound(incomes, (PLN.fromRaw(medianIncomeRaw) * PovertyRate50Pct).toLong), n) else Share.Zero,
      bankruptcyRate = if n > 0 then Share.fraction(nBankrupt, n) else Share.Zero,
      meanSkill = if nAlive > 0 then Share.fromRaw((sumSkill / nAlive).toLong) else Share.Zero,
      meanHealthPenalty = if nAlive > 0 then Share.fromRaw((sumHealth / nAlive).toLong) else Share.Zero,
      retrainingAttempts = t.retrainingAttempts,
      retrainingSuccesses = t.retrainingSuccesses,
      consumptionP10 =
        if n > 0 then PLN.fromRaw(consumptions((n * ConsumptionP10.toLong / com.boombustgroup.amorfati.fp.FixedPointBase.Scale).toInt)) else PLN.Zero,
      consumptionP50 = if n > 0 then PLN.fromRaw(consumptions(n / 2)) else PLN.Zero,
      consumptionP90 =
        if n > 0 then PLN.fromRaw(consumptions(Math.min(n - 1, (n * ConsumptionP90.toLong / com.boombustgroup.amorfati.fp.FixedPointBase.Scale).toInt)))
        else PLN.Zero,
      meanMonthsToRuin = Scalar.Zero,
      povertyRate30 =
        if n > 0 && medianIncomeRaw > 0L then Share.fraction(lowerBound(incomes, (PLN.fromRaw(medianIncomeRaw) * PovertyRate30Pct).toLong), n) else Share.Zero,
      totalRent = t.rent,
      totalDebtService = t.debtService,
      totalUnempBenefits = t.unempBenefits,
      totalDepositInterest = t.depositInterest,
      crossSectorHires = 0,
      voluntaryQuits = t.voluntaryQuits,
      sectorMobilityRate = sectorMobilityRate(households),
      totalRemittances = t.remittances,
      totalPit = t.pit,
      totalSocialTransfers = t.socialTransfers,
      totalConsumerDebtService = t.consumerDebtService,
      totalConsumerOrigination = t.consumerOrigination,
      totalConsumerDefault = t.consumerDefault,
      totalConsumerPrincipal = t.consumerPrincipal,
    )

  /** Gini coefficient for a pre-sorted array (handles negatives by shifting).
    */
  def giniSorted(sorted: Array[Long]): Share =
    val n           = sorted.length
    if n <= 1 then return Share.Zero
    val minVal      = sorted(0)
    val shift       = if minVal < 0L then -minVal else 0L
    var total       = BigInt(0)
    var weightedSum = BigInt(0)
    var i           = 0
    while i < n do
      val v = BigInt(sorted(i) + shift)
      total += v
      weightedSum += BigInt(2 * (i + 1) - n - 1) * v
      i += 1
    if total <= 0 then Share.Zero else Share.fromRaw(scaledDivRaw(weightedSum, BigInt(n) * total))

  /** Binary search: count of elements < threshold in a sorted array. */
  private def lowerBound(sorted: Array[Long], threshold: Long): Int =
    var lo = 0
    var hi = sorted.length
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if sorted(mid) < threshold then lo = mid + 1
      else hi = mid
    lo
