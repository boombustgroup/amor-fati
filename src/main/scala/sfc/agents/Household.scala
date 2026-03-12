package sfc.agents

import sfc.config.*
import sfc.engine.World
import sfc.engine.mechanisms.SectoralMobility
import sfc.networks.Network
import sfc.types.*
import sfc.util.Distributions
import sfc.util.KahanSum.*

import scala.util.Random

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

  // ---- Named constants (extracted from inline magic numbers) ----

  // MPC sampling bounds (Beta distribution, NBP household survey)
  private val MpcFloor   = 0.5
  private val MpcCeiling = 0.98

  // Social network precautionary saving
  private val NeighborDistressThreshold    = 0.30 // fraction of neighbors in distress that triggers adj
  private val NeighborDistressConsAdj      = 0.90 // consumption multiplier when distress exceeds threshold
  private val NeighborDistressRetrainBoost = 0.05 // additional retraining prob when neighbors distressed

  // Labor market
  private val UnemploymentRetrainingThreshold = 6 // months unemployed before eligible for retraining
  private val PostFailedRetrainingMonths      = 7 // months assigned after failed retraining (duration + 1)

  // Consumer credit
  private val DisposableWageThreshold = 0.3   // disposable/wage ratio below which HH may borrow
  private val MinConsumerLoanSize     = 100.0 // minimum loan size (PLN)
  private val ConsumerDebtInitFrac    = 0.3   // init consumer debt as fraction of mortgage draw

  // Init sampling
  private val GpwEquityInitFrac     = 0.05 // fraction of savings allocated to GPW equity at init
  private val SectorSkillBonusCoeff = 0.02 // coefficient for sector-specific skill bonus (log sigma)
  private val SectorSkillBonusMax   = 0.1  // maximum sector-specific skill bonus

  // Aggregates
  private val ImportRatioCap   = 0.65 // cap on import ratio applied to goods consumption
  private val PovertyRate50Pct = 0.50 // poverty line at 50% of median income (EU AROP)
  private val PovertyRate30Pct = 0.30 // poverty line at 30% of median income (deep poverty)
  private val ConsumptionP10   = 0.10 // P10 percentile index
  private val ConsumptionP90   = 0.90 // P90 percentile index

  // ---- Individual household ----

  /** Full state of a single household agent, carried across simulation months.
    */
  case class State(
      id: HhId,                     // unique household identifier
      savings: PLN,                 // liquid savings (bank deposits)
      debt: PLN,                    // outstanding secured (mortgage) debt
      monthlyRent: PLN,             // monthly rent payment (to landlord / housing market)
      skill: Ratio,                 // labor productivity multiplier [0,1], decays during unemployment
      healthPenalty: Ratio,         // cumulative health penalty from long-term unemployment (scarring)
      mpc: Ratio,                   // marginal propensity to consume (Beta-sampled at init)
      status: HhStatus,             // current employment/activity status
      socialNeighbors: Array[HhId], // Watts-Strogatz social network neighbor IDs
      bankId: BankId,               // index into Banking.State.banks (multi-bank)
      equityWealth: PLN,            // value of GPW equity holdings
      lastSectorIdx: SectorIdx,     // last sector employed in (-1 = never)
      isImmigrant: Boolean,         // immigrant status for wage discount + remittances
      numDependentChildren: Int,    // children ≤ 18 for 800+ social transfers
      consumerDebt: PLN,            // outstanding unsecured consumer loan
      education: Int,               // education level: 0=Primary, 1=Vocational, 2=Secondary, 3=Tertiary
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
      giniIndividual: Ratio,         // Gini of income distribution
      giniWealth: Ratio,             // Gini of wealth (savings) distribution
      meanSavings: PLN,              // mean savings across all HH
      medianSavings: PLN,            // median savings across all HH
      povertyRate50: Ratio,          // share with income < 50% median (EU AROP)
      bankruptcyRate: Ratio,         // share of bankrupt HH
      meanSkill: Double,             // mean skill of alive (non-bankrupt) HH
      meanHealthPenalty: Double,     // mean health scarring of alive HH
      retrainingAttempts: Int,       // retraining attempts this month
      retrainingSuccesses: Int,      // successful retraining completions this month
      consumptionP10: PLN,           // 10th percentile of consumption
      consumptionP50: PLN,           // median consumption
      consumptionP90: PLN,           // 90th percentile of consumption
      meanMonthsToRuin: Double,      // mean months until bankruptcy (placeholder)
      povertyRate30: Ratio,          // share with income < 30% median (deep poverty)
      totalRent: PLN,                // aggregate rent payments
      totalDebtService: PLN,         // aggregate secured debt service
      totalUnempBenefits: PLN,       // aggregate unemployment benefits paid
      totalDepositInterest: PLN,     // aggregate deposit interest received
      crossSectorHires: Int,         // cross-sector hires this month
      voluntaryQuits: Int,           // voluntary quits (cross-sector search)
      sectorMobilityRate: Ratio,     // fraction employed in different sector than last
      totalRemittances: PLN,         // aggregate remittances sent abroad
      totalPit: PLN,                 // aggregate PIT paid
      totalSocialTransfers: PLN,     // aggregate 800+ social transfers
      totalConsumerDebtService: PLN, // aggregate consumer debt service
      totalConsumerOrigination: PLN, // aggregate new consumer loans
      totalConsumerDefault: PLN,     // aggregate consumer loan defaults
      totalConsumerPrincipal: PLN,   // aggregate consumer loan principal repaid
  ):
    def unemploymentRate(totalPopulation: Int): Double = 1.0 - employed.toDouble / totalPopulation

  // ---- Init ----

  object Init:

    /** Create individual households with multi-bank assignment. */
    def create(rng: Random, firms: Vector[Firm.State])(using p: SimParams): Vector[State] =
      val hhCount   = firms.map(Firm.workerCount).sum
      val hhNetwork = Network.wattsStrogatz(hhCount, p.household.socialK, p.household.socialP.toDouble, rng)
      val hhs       = initialize(hhCount, firms, hhNetwork, rng)
      // Assign households to same bank as their employer
      hhs.map: h =>
        h.status match
          case HhStatus.Employed(fid, _, _) if fid.toInt < firms.length => h.copy(bankId = firms(fid.toInt).bankId)
          case _                                                        => h

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
    private def sampleHousehold(
        hhId: Int,
        firm: Firm.State,
        sectorIdx: SectorIdx,
        socialNetwork: Array[Array[Int]],
        rng: Random,
    )(using p: SimParams): State =
      val savings: PLN  = PLN(Math.exp(p.household.savingsMu + p.household.savingsSigma * rng.nextGaussian()))
      val debt: PLN     =
        if rng.nextDouble() < p.household.debtFraction.toDouble then PLN(Math.exp(p.household.debtMu + p.household.debtSigma * rng.nextGaussian()))
        else PLN.Zero
      val rent: PLN     = (p.household.rentMean + p.household.rentStd * rng.nextGaussian()).max(p.household.rentFloor)
      val mpc           = Distributions.betaSample(p.household.mpcAlpha, p.household.mpcBeta, rng)
      val (edu, skill)  = sampleEducationAndSkill(sectorIdx, rng)
      val wage: PLN     = p.household.baseWage * (p.sectorDefs(sectorIdx.toInt).wageMultiplier * skill)
      val eqWealth: PLN =
        if p.flags.gpwHhEquity && rng.nextDouble() < p.equity.hhEquityFrac.toDouble then savings * GpwEquityInitFrac
        else PLN.Zero
      val numChildren   = if p.flags.social800 then Distributions.poissonSample(p.fiscal.social800ChildrenPerHh, rng) else 0
      val consDebt: PLN =
        if rng.nextDouble() < p.household.debtFraction.toDouble then
          PLN(Math.exp(p.household.debtMu + p.household.debtSigma * rng.nextGaussian()) * ConsumerDebtInitFrac)
        else PLN.Zero
      State(
        id = HhId(hhId),
        savings = savings,
        debt = debt,
        monthlyRent = rent,
        skill = Ratio(skill),
        healthPenalty = Ratio.Zero,
        mpc = Ratio(mpc).clamp(Ratio(MpcFloor), Ratio(MpcCeiling)),
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
      )

    /** Sample education level and skill for a sector, clamped to edu range. */
    private def sampleEducationAndSkill(sectorIdx: SectorIdx, rng: Random)(using p: SimParams): (Int, Double) =
      val edu                        = p.social.drawEducation(sectorIdx.toInt, rng)
      val (skillFloor, skillCeiling) = p.social.eduSkillRange(edu)
      val sectorSigma                = p.sectorDefs(sectorIdx.toInt).sigma
      val baseSkill                  = skillFloor + (skillCeiling - skillFloor) * rng.nextDouble()
      val sectorBonus                = Math.min(SectorSkillBonusMax, SectorSkillBonusCoeff * Math.log(sectorSigma))
      val skill                      = Math.max(skillFloor, Math.min(skillCeiling, baseSkill + sectorBonus))
      (edu, skill)

  // ---- Step flow totals (immutable, folded from per-HH results) ----

  /** Accumulated flow totals from one step, built via fold over
    * HhMonthlyResult.
    */
  private case class StepTotals(
      income: PLN = PLN.Zero,
      unempBenefits: PLN = PLN.Zero,
      debtService: PLN = PLN.Zero,
      depositInterest: PLN = PLN.Zero,
      goodsConsumption: PLN = PLN.Zero,
      rent: PLN = PLN.Zero,
      remittances: PLN = PLN.Zero,
      pit: PLN = PLN.Zero,
      socialTransfers: PLN = PLN.Zero,
      consumerDebtService: PLN = PLN.Zero,
      consumerOrigination: PLN = PLN.Zero,
      consumerDefault: PLN = PLN.Zero,
      consumerPrincipal: PLN = PLN.Zero,
      retrainingAttempts: Int = 0,
      retrainingSuccesses: Int = 0,
      voluntaryQuits: Int = 0,
  ):
    def add(r: HhMonthlyResult): StepTotals = copy(
      income = income + r.income,
      unempBenefits = unempBenefits + r.benefit,
      debtService = debtService + r.debtService,
      depositInterest = depositInterest + r.depositInterest,
      goodsConsumption = goodsConsumption + r.consumption,
      rent = rent + r.rent,
      remittances = remittances + r.remittance,
      pit = pit + r.pitTax,
      socialTransfers = socialTransfers + r.socialTransfer,
      consumerDebtService = consumerDebtService + r.credit.debtService,
      consumerOrigination = consumerOrigination + r.credit.newLoan,
      consumerDefault = consumerDefault + r.credit.defaultAmt,
      consumerPrincipal = consumerPrincipal + r.credit.principal,
      retrainingAttempts = retrainingAttempts + r.retrainingAttempt,
      retrainingSuccesses = retrainingSuccesses + r.retrainingSuccess,
      voluntaryQuits = voluntaryQuits + r.voluntaryQuit,
    )

  /** Build per-bank flow vector from (BankId, HhMonthlyResult) pairs. */
  private def buildPerBankFlows(flows: Vector[(BankId, HhMonthlyResult)], nBanks: Int): Vector[PerBankFlow] =
    val zero = Vector.fill(nBanks)(PerBankFlow.zero)
    flows.foldLeft(zero) { case (acc, (bankId, r)) =>
      val bId = bankId.toInt
      val cur = acc(bId)
      acc.updated(
        bId,
        cur.copy(
          income = cur.income + r.income,
          consumption = cur.consumption + r.consumption + r.rent,
          debtService = cur.debtService + r.debtService,
          depositInterest = cur.depositInterest + r.depositInterest,
          consumerDebtService = cur.consumerDebtService + r.credit.debtService,
          consumerOrigination = cur.consumerOrigination + r.credit.newLoan,
          consumerDefault = cur.consumerDefault + r.credit.defaultAmt,
          consumerPrincipal = cur.consumerPrincipal + r.credit.principal,
        ),
      )
    }

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

  /** Monthly PIT: progressive Polish brackets (12%/32%), minus kwota wolna. */
  def computeMonthlyPit(monthlyIncome: PLN)(using p: SimParams): PLN =
    if !p.flags.pit || monthlyIncome <= PLN.Zero then PLN.Zero
    else
      val annualized = monthlyIncome * 12.0
      val grossTax   =
        if annualized <= p.fiscal.pitBracket1Annual then annualized * p.fiscal.pitRate1.toDouble
        else
          p.fiscal.pitBracket1Annual * p.fiscal.pitRate1.toDouble +
            (annualized - p.fiscal.pitBracket1Annual) * p.fiscal.pitRate2.toDouble
      (grossTax - p.fiscal.pitTaxCreditAnnual).max(PLN.Zero) / 12.0

  /** Compute 800+ social transfer (PIT-exempt, lump-sum per child ≤ 18). */
  def computeSocialTransfer(numChildren: Int)(using p: SimParams): PLN =
    if !p.flags.social800 || numChildren <= 0 then PLN.Zero
    else PLN(numChildren.toDouble * p.fiscal.social800Rate.toDouble)

  /** Unemployment benefit (zasilek): 1500 PLN m1-3, 1200 PLN m4-6, 0 after. */
  def computeBenefit(monthsUnemployed: Int)(using p: SimParams): PLN =
    if !p.flags.govUnempBenefit then PLN.Zero
    else if monthsUnemployed <= p.fiscal.govBenefitDuration / 2 then p.fiscal.govBenefitM1to3
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
    if !p.flags.sectoralMobility || rng.nextDouble() >= p.labor.voluntarySearchProb.toDouble then return (status, 0)
    val targetSector  =
      SectoralMobility.selectTargetSector(status.sectorIdx.toInt, sectorWages, sectorVacancies, p.labor.frictionMatrix, p.labor.vacancyWeight, rng)
    val targetAvgWage = sectorWages(targetSector)
    if targetAvgWage <= status.wage * (1.0 + p.labor.voluntaryWageThreshold.toDouble) then return (status, 0)
    val friction      = p.labor.frictionMatrix(status.sectorIdx.toInt)(targetSector)
    if friction < p.labor.adjacentFrictionMax.toDouble then (HhStatus.Unemployed(0), 1)
    else
      val rp = SectoralMobility.frictionAdjustedParams(friction, p.labor.frictionDurationMult, p.labor.frictionCostMult.toDouble)
      if hh.savings > rp.cost then (HhStatus.Retraining(rp.duration, SectorIdx(targetSector), rp.cost), 1)
      else (status, 0)

  /** Retraining for unemployed HH → (newStatus, attemptFlag, successFlag). */
  private def tryRetraining(
      hh: State,
      status: HhStatus,
      neighborDistress: Double,
      sectorWages: Option[Vector[PLN]],
      sectorVacancies: Option[Vector[Int]],
      rng: Random,
  )(using p: SimParams): (HhStatus, Int, Int) =
    status match
      case HhStatus.Unemployed(months) if months > UnemploymentRetrainingThreshold && p.household.retrainingEnabled =>
        val retrainProb = p.household.retrainingProb.toDouble +
          (if neighborDistress > NeighborDistressThreshold then NeighborDistressRetrainBoost else 0.0)
        if hh.savings > p.household.retrainingCost && rng.nextDouble() < retrainProb then
          if p.flags.sectoralMobility && sectorWages.isDefined then
            val sw           = sectorWages.get
            val sv           = sectorVacancies.get
            val fromSector   = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
            val targetSector = SectoralMobility.selectTargetSector(fromSector, sw, sv, p.labor.frictionMatrix, p.labor.vacancyWeight, rng)
            val friction     = p.labor.frictionMatrix(fromSector)(targetSector)
            val rp           = SectoralMobility.frictionAdjustedParams(friction, p.labor.frictionDurationMult, p.labor.frictionCostMult.toDouble)
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
          val baseSuccessProb =
            (p.household.retrainingBaseSuccess * afterSkill).toDouble * (1.0 - afterHealth.toDouble) * p.social.eduRetrainMultiplier(hh.education)
          val successProb     = if p.flags.sectoralMobility then
            val fromSector = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
            val friction   = p.labor.frictionMatrix(fromSector)(targetSector.toInt)
            SectoralMobility.frictionAdjustedSuccess(baseSuccessProb, friction)
          else baseSuccessProb
          if rng.nextDouble() < successProb then (HhStatus.Unemployed(0), 0, 1)
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
    val consumerDebtSvc    = hh.consumerDebt * (p.household.ccAmortRate.toDouble + consumerRate.toDouble / 12.0)
    val consumerPrin       = hh.consumerDebt * p.household.ccAmortRate.toDouble

    val newConsumerLoan = hh.status match
      case HhStatus.Employed(_, _, wage)                                             =>
        val stressed = disposable < wage * DisposableWageThreshold
        val eligible = stressed && rng.nextDouble() < p.household.ccEligRate.toDouble
        if !eligible then PLN.Zero
        else
          val existingDti = (debtService + consumerDebtSvc).toDouble / Math.max(1.0, income.toDouble)
          val headroom    = Math.max(0.0, p.household.ccMaxDti.toDouble - existingDti) * income.toDouble
          val desired     = Math.min(headroom, p.household.ccMaxLoan.toDouble)
          if desired > MinConsumerLoanSize then PLN(desired) else PLN.Zero
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
      neighborDistress: Double,
  )

  /** Per-HH monthly pipeline: income → tax → credit → consumption → equity. */
  private def computeMonthlyFlows(
      hh: State,
      world: World,
      rng: Random,
      bankRates: Option[BankRates],
      equityIndexReturn: Double,
      distressedIds: java.util.BitSet,
  )(using p: SimParams): MonthlyFlows =
    val (baseIncome, benefit, newStatus) = computeIncome(hh)

    // Variable-rate debt service (monetary transmission channel 1)
    val debtServiceRate: Double = bankRates match
      case Some(br) => p.household.baseAmortRate.toDouble + br.lendingRates(hh.bankId.toInt).toDouble / 12.0
      case None     => p.household.debtServiceRate.toDouble

    // Deposit interest (monetary transmission channel 2)
    val depInterest: PLN = bankRates match
      case Some(br) => hh.savings * (br.depositRates(hh.bankId.toInt).toDouble / 12.0)
      case None     => PLN.Zero

    val grossIncome     = baseIncome + depInterest.max(PLN.Zero)
    val pitTax          = computeMonthlyPit(grossIncome)
    val socialTransfer  = computeSocialTransfer(hh.numDependentChildren)
    val income          = grossIncome - pitTax + socialTransfer
    val thisDebtService = hh.debt * debtServiceRate

    val remittance =
      if hh.isImmigrant && p.flags.immigration then income * p.immigration.remitRate
      else PLN.Zero

    val obligations         = hh.monthlyRent + thisDebtService + remittance
    val disposablePreCredit = (income - obligations).max(PLN.Zero)
    val credit              = processConsumerCredit(hh, income, disposablePreCredit, thisDebtService, world, bankRates, rng)
    val fullObligations     = obligations + credit.debtService
    val disposable          = (income - fullObligations).max(PLN.Zero)
    val consumption         = (disposable + credit.newLoan) * hh.mpc

    // Social network precautionary effect
    val neighborDistress = neighborDistressRatioFast(hh, distressedIds)
    val consumptionAdj   =
      if neighborDistress > NeighborDistressThreshold then consumption * NeighborDistressConsAdj else consumption

    // GPW equity wealth effect
    val newEquityWealth       = (hh.equityWealth * (1.0 + equityIndexReturn)).max(PLN.Zero)
    val equityGain            = newEquityWealth - hh.equityWealth
    val wealthEffectBoost     =
      if p.flags.gpwHhEquity && equityGain > PLN.Zero then equityGain * p.equity.wealthEffectMpc
      else PLN.Zero
    val consumptionWithWealth = consumptionAdj + wealthEffectBoost

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
      equityIndexReturn: Double,
      sectorWages: Option[Vector[PLN]],
      sectorVacancies: Option[Vector[Int]],
      distressedIds: java.util.BitSet,
  )(using p: SimParams): HhMonthlyResult =
    val f = computeMonthlyFlows(hh, world, rng, bankRates, equityIndexReturn, distressedIds)
    if f.newSavings < hh.monthlyRent * p.household.bankruptcyThreshold then resolveBankruptcy(f)
    else resolveSurvival(f, sectorWages, sectorVacancies, rng)

  /** Bankruptcy branch: write off consumer debt, zero equity. */
  private def resolveBankruptcy(f: MonthlyFlows)(using p: SimParams): HhMonthlyResult =
    val ccDefaultAmt  = f.hh.consumerDebt * (1.0 - p.household.ccAmortRate.toDouble) + f.credit.newLoan
    val creditWithDef = f.credit.copy(defaultAmt = ccDefaultAmt, updatedDebt = PLN.Zero)
    HhMonthlyResult(
      newState = f.hh.copy(
        savings = f.newSavings,
        debt = f.newDebt,
        consumerDebt = PLN.Zero,
        status = HhStatus.Bankrupt,
        equityWealth = PLN.Zero,
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
  )(using p: SimParams): HhMonthlyResult =
    val afterSkill  = applySkillDecay(f.hh, f.newStatus)
    val afterHealth = applyHealthScarring(f.hh, f.newStatus)

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
        mpc = f.hh.mpc,
        status = finalStatus,
        equityWealth = f.newEquityWealth,
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
      importAdj: Double,
      rng: Random,
      nBanks: Int = 1,
      bankRates: Option[BankRates] = None,
      equityIndexReturn: Double = 0.0,
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
    val totals  = flows.foldLeft(StepTotals())((acc, br) => acc.add(br._2))
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
  private def sectorMobilityRate(updated: Vector[State])(using p: SimParams): Double =
    if !p.flags.sectoralMobility then return 0.0
    val employed = updated.flatMap: hh =>
      hh.status match
        case HhStatus.Employed(_, sec, _) => Some((hh.lastSectorIdx, sec))
        case _                            => None
    if employed.isEmpty then return 0.0
    employed.count((last, cur) => last.toInt >= 0 && last != cur).toDouble / employed.length

  /** Base income, benefit, and updated status for one HH. */
  private def computeIncome(hh: State)(using SimParams): (PLN, PLN, HhStatus) =
    hh.status match
      case HhStatus.Employed(firmId, sectorIdx, wage)    =>
        (wage, PLN.Zero, hh.status)
      case HhStatus.Unemployed(months)                   =>
        val benefit = computeBenefit(months)
        (benefit, benefit, HhStatus.Unemployed(months + 1))
      case HhStatus.Retraining(monthsLeft, target, cost) =>
        (PLN.Zero, PLN.Zero, hh.status)
      case HhStatus.Bankrupt                             =>
        (PLN.Zero, PLN.Zero, HhStatus.Bankrupt)

  /** Skill decay for long-term unemployed (onset after scarringOnset months).
    */
  private def applySkillDecay(hh: State, status: HhStatus)(using p: SimParams): Ratio =
    status match
      case HhStatus.Unemployed(months) if months >= p.household.scarringOnset =>
        hh.skill * (Ratio.One - p.household.skillDecayRate)
      case _                                                                  => hh.skill

  /** Apply health scarring for long-term unemployed (cumulative, capped). */
  private def applyHealthScarring(hh: State, status: HhStatus)(using p: SimParams): Ratio =
    status match
      case HhStatus.Unemployed(months) if months >= p.household.scarringOnset =>
        (hh.healthPenalty + p.household.scarringRate).min(p.household.scarringCap)
      case _                                                                  => hh.healthPenalty

  /** Fraction of social neighbors in distress (BitSet, O(k) per HH). */
  private def neighborDistressRatioFast(hh: State, distressedIds: java.util.BitSet): Double =
    if hh.socialNeighbors.isEmpty then 0.0
    else
      var count = 0
      var i     = 0
      while i < hh.socialNeighbors.length do
        if distressedIds.get(hh.socialNeighbors(i).toInt) then count += 1
        i += 1
      count.toDouble / hh.socialNeighbors.length

  /** Public entry point for aggregate stats (used by BankUpdateStep and tests).
    * Flow totals default to zero — only distribution stats are computed.
    */
  def computeAggregates(
      households: Vector[State],
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Double,
      retrainingAttempts: Int,
      retrainingSuccesses: Int,
  )(using SimParams): Aggregates =
    computeAggregates(
      households,
      marketWage,
      reservationWage,
      importAdj,
      StepTotals(retrainingAttempts = retrainingAttempts, retrainingSuccesses = retrainingSuccesses),
    )

  /** Aggregate stats: single-pass accumulation + sorted-array Gini/percentiles.
    * Merges per-HH distribution stats with flow totals from StepTotals in one
    * construction — no intermediate Aggregates + copy overwrite.
    */
  private def computeAggregates(
      households: Vector[State],
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Double,
      t: StepTotals,
  )(using p: SimParams): Aggregates =
    val n = households.length

    var nEmployed    = 0
    var nUnemployed  = 0
    var nRetraining  = 0
    var nBankrupt    = 0
    var sumSkill     = 0.0
    var sumHealth    = 0.0
    val incomes      = new Array[Double](n)
    val consumptions = new Array[Double](n)
    val savingsArr   = new Array[Double](n)

    // Hot path: O(N_hh) single-pass with mutable accumulators + in-place arrays.
    // Intentionally imperative — foldLeft with 9-field accumulator would be slower and less readable.
    var i = 0
    while i < n do
      val hh = households(i)
      hh.status match
        case HhStatus.Employed(_, _, wage) =>
          nEmployed += 1
          incomes(i) = wage.toDouble
          sumSkill += hh.skill.toDouble
          sumHealth += hh.healthPenalty.toDouble
        case HhStatus.Unemployed(months)   =>
          nUnemployed += 1
          incomes(i) = computeBenefit(months).toDouble
          sumSkill += hh.skill.toDouble
          sumHealth += hh.healthPenalty.toDouble
        case HhStatus.Retraining(_, _, _)  =>
          nRetraining += 1
          incomes(i) = 0.0
          sumSkill += hh.skill.toDouble
          sumHealth += hh.healthPenalty.toDouble
        case HhStatus.Bankrupt             =>
          nBankrupt += 1
          incomes(i) = 0.0

      val rent       = hh.monthlyRent.toDouble
      val debtSvc    = hh.debt.toDouble * p.household.debtServiceRate.toDouble
      val disposable = Math.max(0.0, incomes(i) - rent - debtSvc)
      consumptions(i) = disposable * hh.mpc.toDouble
      savingsArr(i) = hh.savings.toDouble
      i += 1

    val nAlive = n - nBankrupt

    // Sort each array once — reuse for Gini + percentiles + poverty
    java.util.Arrays.sort(incomes)
    java.util.Arrays.sort(savingsArr)
    java.util.Arrays.sort(consumptions)

    // Consumption split: flow totals (from StepTotals) are authoritative
    val totalConsumption = (t.goodsConsumption + t.rent).toDouble
    val importCons       = t.goodsConsumption.toDouble * Math.min(ImportRatioCap, importAdj)
    val domesticCons     = totalConsumption - importCons

    val medianIncome = if n > 0 then incomes(n / 2) else 0.0

    Aggregates(
      employed = nEmployed,
      unemployed = nUnemployed,
      retraining = nRetraining,
      bankrupt = nBankrupt,
      totalIncome = t.income,
      consumption = PLN(totalConsumption),
      domesticConsumption = PLN(domesticCons),
      importConsumption = PLN(importCons),
      marketWage = marketWage,
      reservationWage = reservationWage,
      giniIndividual = Ratio(giniSorted(incomes)),
      giniWealth = Ratio(giniSorted(savingsArr)),
      meanSavings = PLN(if n > 0 then savingsArr.kahanSum / n else 0.0),
      medianSavings = PLN(if n > 0 then savingsArr(n / 2) else 0.0),
      povertyRate50 = Ratio(if n > 0 && medianIncome > 0 then lowerBound(incomes, medianIncome * PovertyRate50Pct).toDouble / n else 0.0),
      bankruptcyRate = Ratio(if n > 0 then nBankrupt.toDouble / n else 0.0),
      meanSkill = if nAlive > 0 then sumSkill / nAlive else 0.0,
      meanHealthPenalty = if nAlive > 0 then sumHealth / nAlive else 0.0,
      retrainingAttempts = t.retrainingAttempts,
      retrainingSuccesses = t.retrainingSuccesses,
      consumptionP10 = PLN(if n > 0 then consumptions((n * ConsumptionP10).toInt) else 0.0),
      consumptionP50 = PLN(if n > 0 then consumptions(n / 2) else 0.0),
      consumptionP90 = PLN(if n > 0 then consumptions(Math.min(n - 1, (n * ConsumptionP90).toInt)) else 0.0),
      meanMonthsToRuin = 0.0,
      povertyRate30 = Ratio(if n > 0 && medianIncome > 0 then lowerBound(incomes, medianIncome * PovertyRate30Pct).toDouble / n else 0.0),
      totalRent = t.rent,
      totalDebtService = t.debtService,
      totalUnempBenefits = t.unempBenefits,
      totalDepositInterest = t.depositInterest,
      crossSectorHires = 0,
      voluntaryQuits = t.voluntaryQuits,
      sectorMobilityRate = Ratio(sectorMobilityRate(households)),
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
  def giniSorted(sorted: Array[Double]): Double =
    val n           = sorted.length
    if n <= 1 then return 0.0
    val minVal      = sorted(0)
    val shift       = if minVal < 0 then -minVal else 0.0
    var total       = 0.0
    var weightedSum = 0.0
    var i           = 0
    while i < n do
      val v = sorted(i) + shift
      total += v
      weightedSum += (2.0 * (i + 1) - n - 1) * v
      i += 1
    if total <= 0 then 0.0 else weightedSum / (n * total)

  /** Binary search: count of elements < threshold in a sorted array. */
  private def lowerBound(sorted: Array[Double], threshold: Double): Int =
    var lo = 0
    var hi = sorted.length
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if sorted(mid) < threshold then lo = mid + 1
      else hi = mid
    lo
