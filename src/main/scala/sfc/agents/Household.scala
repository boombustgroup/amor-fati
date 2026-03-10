package sfc.agents

import sfc.config.*
import sfc.engine.World
import sfc.engine.markets.SectoralMobility
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
    lendingRates: Array[Double], // annual lending rate per bank (index = BankId)
    depositRates: Array[Double], // annual deposit rate per bank (index = BankId)
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
      id: HhId,                                 // unique household identifier
      savings: PLN,                             // liquid savings (bank deposits)
      debt: PLN,                                // outstanding secured (mortgage) debt
      monthlyRent: PLN,                         // monthly rent payment (to landlord / housing market)
      skill: Ratio,                             // labor productivity multiplier [0,1], decays during unemployment
      healthPenalty: Ratio,                     // cumulative health penalty from long-term unemployment (scarring)
      mpc: Ratio,                               // marginal propensity to consume (Beta-sampled at init)
      status: HhStatus,                         // current employment/activity status
      socialNeighbors: Array[HhId],             // Watts-Strogatz social network neighbor IDs
      bankId: BankId = BankId(0),               // index into Banking.State.banks (multi-bank)
      equityWealth: PLN = PLN.Zero,             // value of GPW equity holdings
      lastSectorIdx: SectorIdx = SectorIdx(-1), // last sector employed in (-1 = never)
      isImmigrant: Boolean = false,             // immigrant status for wage discount + remittances
      numDependentChildren: Int = 0,            // children ≤ 18 for 800+ social transfers
      consumerDebt: PLN = PLN.Zero,             // outstanding unsecured consumer loan
      education: Int = 2,                       // education level: 0=Primary, 1=Vocational, 2=Secondary, 3=Tertiary
  )

  /** Aggregate statistics computed from individual households (Paper-06). */
  case class Aggregates(
      employed: Int,                            // count of employed HH
      unemployed: Int,                          // count of unemployed HH
      retraining: Int,                          // count of HH in retraining
      bankrupt: Int,                            // count of bankrupt HH
      totalIncome: PLN,                         // aggregate income (wages + benefits + interest + transfers)
      consumption: PLN,                         // aggregate consumption (goods + rent)
      domesticConsumption: PLN,                 // domestic component of consumption
      importConsumption: PLN,                   // import component of consumption
      marketWage: PLN,                          // current market-clearing wage
      reservationWage: PLN,                     // minimum acceptable wage for job search
      giniIndividual: Ratio,                    // Gini of income distribution
      giniWealth: Ratio,                        // Gini of wealth (savings) distribution
      meanSavings: PLN,                         // mean savings across all HH
      medianSavings: PLN,                       // median savings across all HH
      povertyRate50: Ratio,                     // share with income < 50% median (EU AROP)
      bankruptcyRate: Ratio,                    // share of bankrupt HH
      meanSkill: Double,                        // mean skill of alive (non-bankrupt) HH
      meanHealthPenalty: Double,                // mean health scarring of alive HH
      retrainingAttempts: Int,                  // retraining attempts this month
      retrainingSuccesses: Int,                 // successful retraining completions this month
      consumptionP10: PLN,                      // 10th percentile of consumption
      consumptionP50: PLN,                      // median consumption
      consumptionP90: PLN,                      // 90th percentile of consumption
      meanMonthsToRuin: Double,                 // mean months until bankruptcy (placeholder)
      povertyRate30: Ratio,                     // share with income < 30% median (deep poverty)
      totalRent: PLN,                           // aggregate rent payments
      totalDebtService: PLN,                    // aggregate secured debt service
      totalUnempBenefits: PLN,                  // aggregate unemployment benefits paid
      totalDepositInterest: PLN = PLN.Zero,     // aggregate deposit interest received
      crossSectorHires: Int = 0,                // cross-sector hires this month
      voluntaryQuits: Int = 0,                  // voluntary quits (cross-sector search)
      sectorMobilityRate: Ratio = Ratio.Zero,   // fraction employed in different sector than last
      totalRemittances: PLN = PLN.Zero,         // aggregate remittances sent abroad
      totalPit: PLN = PLN.Zero,                 // aggregate PIT paid
      totalSocialTransfers: PLN = PLN.Zero,     // aggregate 800+ social transfers
      totalConsumerDebtService: PLN = PLN.Zero, // aggregate consumer debt service
      totalConsumerOrigination: PLN = PLN.Zero, // aggregate new consumer loans
      totalConsumerDefault: PLN = PLN.Zero,     // aggregate consumer loan defaults
      totalConsumerPrincipal: PLN = PLN.Zero,   // aggregate consumer loan principal repaid
  ):
    def unemploymentRate(totalPopulation: Int): Double = 1.0 - employed.toDouble / totalPopulation

  // ---- Init ----

  object Init:

    /** Create individual households with multi-bank assignment. */
    def create(rng: Random, firms: Vector[Firm.State])(using p: SimParams): Vector[State] =
      val hhCount   = firms.map(Firm.workerCount).sum
      val hhNetwork = Network.wattsStrogatz(hhCount, p.household.socialK, p.household.socialP.toDouble, rng)
      val hhs       = initialize(hhCount, p.pop.firmsCount, firms, hhNetwork, rng)
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
        nFirms: Int,
        firms: Vector[Firm.State],
        socialNetwork: Array[Array[Int]],
        rng: Random,
    )(using p: SimParams): Vector[State] =
      var hhId    = 0
      val builder = Vector.newBuilder[State]

      for f <- firms if Firm.isAlive(f) do
        val nWorkers  = Firm.workerCount(f)
        val sectorIdx = f.sector
        for _ <- 0 until nWorkers do
          if hhId < nHouseholds then
            // Savings: LogNormal(mu, sigma)
            val savings = Math.exp(p.household.savingsMu + p.household.savingsSigma * rng.nextGaussian())

            // Debt: 40% have debt
            val debt =
              if rng.nextDouble() < p.household.debtFraction.toDouble then Math.exp(p.household.debtMu + p.household.debtSigma * rng.nextGaussian())
              else 0.0

            // Rent: Normal(mean, std), floored
            val rent = Math.max(
              p.household.rentFloor.toDouble,
              p.household.rentMean.toDouble + p.household.rentStd.toDouble * rng.nextGaussian(),
            )

            // MPC: Beta(alpha, beta) via gamma transformation
            val mpc = Distributions.betaSample(p.household.mpcAlpha, p.household.mpcBeta, rng)

            // Education draw + skill range
            val edu                        = p.social.drawEducation(sectorIdx.toInt, rng)
            val (skillFloor, skillCeiling) = p.social.eduSkillRange(edu)
            val sectorSigma                = p.sectorDefs(sectorIdx.toInt).sigma
            val baseSkill                  = skillFloor + (skillCeiling - skillFloor) * rng.nextDouble()
            val sectorBonus                = Math.min(SectorSkillBonusMax, SectorSkillBonusCoeff * Math.log(sectorSigma))
            val skill                      = Math.max(skillFloor, Math.min(skillCeiling, baseSkill + sectorBonus))

            val wage = p.household.baseWage.toDouble * p.sectorDefs(sectorIdx.toInt).wageMultiplier * skill

            // GPW equity wealth: GpwHhEquityFrac of HH participate, with wealth ∝ savings
            val eqWealth =
              if p.flags.gpwHhEquity && rng.nextDouble() < p.equity.hhEquityFrac.toDouble then savings * GpwEquityInitFrac
              else 0.0

            // 800+ children: Poisson(λ) per HH
            val numChildren =
              if p.flags.social800 then Distributions.poissonSample(p.fiscal.social800ChildrenPerHh, rng)
              else 0

            // Consumer credit: 40% of HH have small consumer loans (reuse HhDebtFraction)
            val consDebt =
              if rng.nextDouble() < p.household.debtFraction.toDouble then
                Math.exp(p.household.debtMu + p.household.debtSigma * rng.nextGaussian()) * ConsumerDebtInitFrac
              else 0.0

            builder += State(
              id = HhId(hhId),
              savings = PLN(savings),
              debt = PLN(debt),
              monthlyRent = PLN(rent),
              skill = Ratio(skill),
              healthPenalty = Ratio.Zero,
              mpc = Ratio(Math.max(MpcFloor, Math.min(MpcCeiling, mpc))),
              status = HhStatus.Employed(f.id, sectorIdx, PLN(wage)),
              socialNeighbors =
                if hhId < socialNetwork.length then socialNetwork(hhId).map(HhId(_)) else Array.empty[HhId],
              equityWealth = PLN(eqWealth),
              lastSectorIdx = sectorIdx,
              numDependentChildren = numChildren,
              consumerDebt = PLN(consDebt),
              education = edu,
            )
            hhId += 1

      builder.result()

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
      PLN(Math.max(0.0, grossTax.toDouble - p.fiscal.pitTaxCreditAnnual.toDouble) / 12.0)

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
      sectorWages: Array[Double],
      sectorVacancies: Array[Int],
      rng: Random,
  )(using p: SimParams): (HhStatus, Int) =
    if !p.flags.sectoralMobility || rng.nextDouble() >= p.labor.voluntarySearchProb.toDouble then return (status, 0)

    val targetSector  = SectoralMobility.selectTargetSector(
      status.sectorIdx.toInt,
      sectorWages,
      sectorVacancies,
      p.labor.frictionMatrix,
      p.labor.vacancyWeight,
      rng,
    )
    val targetAvgWage = sectorWages(targetSector)
    if targetAvgWage <= status.wage.toDouble * (1.0 + p.labor.voluntaryWageThreshold.toDouble) then return (status, 0)

    val friction = p.labor.frictionMatrix(status.sectorIdx.toInt)(targetSector)
    if friction < p.labor.adjacentFrictionMax.toDouble then (HhStatus.Unemployed(0), 1)
    else
      val (adjDur, adjCost) = SectoralMobility.frictionAdjustedParams(
        friction,
        p.labor.frictionDurationMult,
        p.labor.frictionCostMult.toDouble,
      )
      if hh.savings.toDouble > adjCost then (HhStatus.Retraining(adjDur, SectorIdx(targetSector), PLN(adjCost)), 1)
      else (status, 0)

  /** Retraining for unemployed HH → (newStatus, attemptFlag, successFlag). */
  private def tryRetraining(
      hh: State,
      status: HhStatus,
      neighborDistress: Double,
      sectorWages: Option[Array[Double]],
      sectorVacancies: Option[Array[Int]],
      rng: Random,
  )(using p: SimParams): (HhStatus, Int, Int) =
    status match
      case HhStatus.Unemployed(months) if months > UnemploymentRetrainingThreshold && p.household.retrainingEnabled =>
        val retrainProb = p.household.retrainingProb.toDouble +
          (if neighborDistress > NeighborDistressThreshold then NeighborDistressRetrainBoost else 0.0)
        if hh.savings.toDouble > p.household.retrainingCost.toDouble && rng.nextDouble() < retrainProb then
          if p.flags.sectoralMobility && sectorWages.isDefined then
            val sw                = sectorWages.get
            val sv                = sectorVacancies.get
            val fromSector        = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
            val targetSector      = SectoralMobility.selectTargetSector(
              fromSector,
              sw,
              sv,
              p.labor.frictionMatrix,
              p.labor.vacancyWeight,
              rng,
            )
            val friction          = p.labor.frictionMatrix(fromSector)(targetSector)
            val (adjDur, adjCost) = SectoralMobility.frictionAdjustedParams(
              friction,
              p.labor.frictionDurationMult,
              p.labor.frictionCostMult.toDouble,
            )
            if hh.savings.toDouble > adjCost then (HhStatus.Retraining(adjDur, SectorIdx(targetSector), PLN(adjCost)), 1, 0)
            else (status, 0, 0)
          else
            val targetSector = rng.nextInt(p.sectorDefs.length)
            (
              HhStatus.Retraining(
                p.household.retrainingDuration,
                SectorIdx(targetSector),
                PLN(p.household.retrainingCost.toDouble),
              ),
              1,
              0,
            )
        else (status, 0, 0)

      case HhStatus.Retraining(monthsLeft, targetSector, cost) =>
        if monthsLeft <= 1 then
          val afterSkill      = applySkillDecay(hh, status)
          val afterHealth     = applyHealthScarring(hh, status)
          val baseSuccessProb = p.household.retrainingBaseSuccess.toDouble *
            afterSkill.toDouble * (1.0 - afterHealth.toDouble) * p.social.eduRetrainMultiplier(hh.education)
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
    val consumerRate    = bankRates match
      case Some(br) => br.lendingRates(hh.bankId.toInt) + p.household.ccSpread.toDouble
      case None     => world.nbp.referenceRate.toDouble + p.household.ccSpread.toDouble
    val consumerDebtSvc = hh.consumerDebt * (p.household.ccAmortRate.toDouble + consumerRate / 12.0)
    val consumerPrin    = hh.consumerDebt * p.household.ccAmortRate.toDouble

    val newConsumerLoan = hh.status match
      case HhStatus.Employed(_, _, wage) if disposable < wage * DisposableWageThreshold && rng.nextDouble() < p.household.ccEligRate.toDouble =>
        val existingDti = (debtService + consumerDebtSvc).toDouble / Math.max(1.0, income.toDouble)
        val headroom    = Math.max(0.0, p.household.ccMaxDti.toDouble - existingDti) * income.toDouble
        val desired     = Math.min(headroom, p.household.ccMaxLoan.toDouble)
        if desired > MinConsumerLoanSize then PLN(desired) else PLN.Zero
      case _                                                                                                                                  => PLN.Zero

    val updatedDebt = PLN(Math.max(0.0, (hh.consumerDebt + newConsumerLoan - consumerDebtSvc).toDouble))

    CreditResult(
      debtService = consumerDebtSvc,
      principal = consumerPrin,
      newLoan = newConsumerLoan,
      defaultAmt = PLN.Zero,
      updatedDebt = updatedDebt,
    )

  /** Per-HH monthly pipeline: income → tax → credit → consumption → equity →
    * labor.
    */
  private def processHousehold(
      hh: State,
      world: World,
      rng: Random,
      bankRates: Option[BankRates],
      equityIndexReturn: Double,
      sectorWages: Option[Array[Double]],
      sectorVacancies: Option[Array[Int]],
      distressedIds: java.util.BitSet,
  )(using p: SimParams): HhMonthlyResult =
    val (baseIncome, benefit, newStatus) = computeIncome(hh)

    // Variable-rate debt service (monetary transmission channel 1)
    val debtServiceRate = bankRates match
      case Some(br) => p.household.baseAmortRate.toDouble + br.lendingRates(hh.bankId.toInt) / 12.0
      case None     => p.household.debtServiceRate.toDouble

    // Deposit interest (monetary transmission channel 2)
    val depInterest = bankRates match
      case Some(br) => PLN(br.depositRates(hh.bankId.toInt) / 12.0 * hh.savings.toDouble)
      case None     => PLN.Zero

    val grossIncome     = baseIncome + depInterest.max(PLN.Zero)
    val pitTax          = computeMonthlyPit(grossIncome)
    val socialTransfer  = computeSocialTransfer(hh.numDependentChildren)
    val income          = grossIncome - pitTax + socialTransfer
    val thisDebtService = hh.debt * debtServiceRate

    // Immigrant remittance: fraction of net-of-PIT income sent abroad
    val remittance =
      if hh.isImmigrant && p.flags.immigration then income * p.immigration.remitRate.toDouble
      else PLN.Zero

    val obligations = hh.monthlyRent + thisDebtService + remittance

    // Consumer credit
    val disposablePreCredit = (income - obligations).max(PLN.Zero)
    val credit              = processConsumerCredit(hh, income, disposablePreCredit, thisDebtService, world, bankRates, rng)
    val fullObligations     = obligations + credit.debtService
    val disposable          = (income - fullObligations).max(PLN.Zero)

    val consumption = (disposable + credit.newLoan) * hh.mpc.toDouble

    // Social network precautionary effect
    val neighborDistress = neighborDistressRatioFast(hh, distressedIds)
    val consumptionAdj   =
      if neighborDistress > NeighborDistressThreshold then consumption * NeighborDistressConsAdj else consumption

    // GPW equity wealth effect
    val newEquityWealth       = PLN(Math.max(0.0, hh.equityWealth.toDouble * (1.0 + equityIndexReturn)))
    val equityGain            = newEquityWealth - hh.equityWealth
    val wealthEffectBoost     =
      if p.flags.gpwHhEquity && equityGain > PLN.Zero then equityGain * p.equity.wealthEffectMpc.toDouble
      else PLN.Zero
    val consumptionWithWealth = consumptionAdj + wealthEffectBoost

    val newSavings      = hh.savings + income - fullObligations + credit.newLoan - consumptionWithWealth
    val newDebt         = PLN(Math.max(0.0, (hh.debt - thisDebtService).toDouble))
    val newConsumerDebt = credit.updatedDebt
    val rent            = hh.monthlyRent

    // Bankruptcy test
    if newSavings.toDouble < p.household.bankruptcyThreshold * rent.toDouble then
      val ccDefaultAmt  = hh.consumerDebt * (1.0 - p.household.ccAmortRate.toDouble) + credit.newLoan
      val creditWithDef = credit.copy(defaultAmt = ccDefaultAmt, updatedDebt = PLN.Zero)
      val bankruptState = hh.copy(
        savings = newSavings,
        debt = newDebt,
        consumerDebt = PLN.Zero,
        status = HhStatus.Bankrupt,
        equityWealth = PLN.Zero,
      )
      HhMonthlyResult(
        newState = bankruptState,
        income = income,
        benefit = benefit,
        consumption = consumptionWithWealth,
        debtService = thisDebtService,
        depositInterest = depInterest.max(PLN.Zero),
        remittance = remittance,
        pitTax = pitTax,
        socialTransfer = socialTransfer,
        credit = creditWithDef,
        voluntaryQuit = 0,
        retrainingAttempt = 0,
        retrainingSuccess = 0,
        equityWealth = PLN.Zero,
        rent = rent,
      )
    else
      val afterSkill  = applySkillDecay(hh, newStatus)
      val afterHealth = applyHealthScarring(hh, newStatus)

      // Voluntary cross-sector search (employed workers only)
      val (afterVoluntary, vQuit) = newStatus match
        case emp: HhStatus.Employed if sectorWages.isDefined =>
          tryVoluntarySearch(hh, emp, sectorWages.get, sectorVacancies.get, rng)
        case _                                               => (newStatus, 0)

      // Retraining decision
      val (finalStatus, rAttempt, rSuccess) =
        tryRetraining(hh, afterVoluntary, neighborDistress, sectorWages, sectorVacancies, rng)

      val retrainingCostThisMonth = finalStatus match
        case HhStatus.Retraining(ml, _, cost) if ml == p.household.retrainingDuration - 1 =>
          cost
        case _                                                                            => PLN.Zero

      val updatedState = hh.copy(
        savings = newSavings - retrainingCostThisMonth,
        debt = newDebt,
        consumerDebt = newConsumerDebt,
        skill = afterSkill,
        healthPenalty = afterHealth,
        mpc = hh.mpc,
        status = finalStatus,
        equityWealth = newEquityWealth,
      )

      HhMonthlyResult(
        newState = updatedState,
        income = income,
        benefit = benefit,
        consumption = consumptionWithWealth,
        debtService = thisDebtService,
        depositInterest = depInterest.max(PLN.Zero),
        remittance = remittance,
        pitTax = pitTax,
        socialTransfer = socialTransfer,
        credit = credit,
        voluntaryQuit = vQuit,
        retrainingAttempt = rAttempt,
        retrainingSuccess = rSuccess,
        equityWealth = newEquityWealth,
        rent = rent,
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
      sectorWages: Option[Array[Double]] = None,
      sectorVacancies: Option[Array[Int]] = None,
  )(using p: SimParams): (Vector[State], Aggregates, Option[Vector[PerBankFlow]]) =

    // Pre-compute distressed HH set: O(N_hh) instead of O(N_hh x k) per-HH lookup
    val distressedIds = new java.util.BitSet(households.length)
    var idx           = 0
    while idx < households.length do
      households(idx).status match
        case HhStatus.Bankrupt | HhStatus.Unemployed(_) => distressedIds.set(idx)
        case _                                          =>
      idx += 1

    // Map each HH to (updatedState, Option[(bankId, result)])
    val mapped = households.map: hh =>
      if hh.status == HhStatus.Bankrupt then (hh, None) // absorbing barrier
      else
        val result = processHousehold(
          hh,
          world,
          rng,
          bankRates,
          equityIndexReturn,
          sectorWages,
          sectorVacancies,
          distressedIds,
        )
        (result.newState, Some((hh.bankId, result)))

    val updated = mapped.map(_._1)
    val flows   = mapped.flatMap(_._2)

    // Fold totals (immutable)
    val t = flows.foldLeft(StepTotals())((acc, br) => acc.add(br._2))

    val agg                    =
      computeAggregates(updated, marketWage, reservationWage, importAdj, t.retrainingAttempts, t.retrainingSuccesses)
    val actualTotalConsumption = (t.goodsConsumption + t.rent).toDouble
    val actualImportCons       = t.goodsConsumption.toDouble * Math.min(ImportRatioCap, importAdj)
    val actualDomesticCons     = actualTotalConsumption - actualImportCons
    // Sector mobility rate: fraction of employed in different sector than lastSectorIdx
    val smRate                 = if p.flags.sectoralMobility then
      val employed = updated.filter(_.status.isInstanceOf[HhStatus.Employed])
      if employed.nonEmpty then
        employed.count { hh =>
          val sec = hh.status.asInstanceOf[HhStatus.Employed].sectorIdx
          hh.lastSectorIdx.toInt >= 0 && hh.lastSectorIdx != sec
        }.toDouble / employed.length
      else 0.0
    else 0.0
    val correctedAgg           = agg.copy(
      totalIncome = t.income,
      consumption = PLN(actualTotalConsumption),
      importConsumption = PLN(actualImportCons),
      domesticConsumption = PLN(actualDomesticCons),
      totalUnempBenefits = t.unempBenefits,
      totalDebtService = t.debtService,
      totalDepositInterest = t.depositInterest,
      totalRent = t.rent,
      voluntaryQuits = t.voluntaryQuits,
      sectorMobilityRate = Ratio(smRate),
      totalRemittances = t.remittances,
      totalPit = t.pit,
      totalSocialTransfers = t.socialTransfers,
      totalConsumerDebtService = t.consumerDebtService,
      totalConsumerOrigination = t.consumerOrigination,
      totalConsumerDefault = t.consumerDefault,
      totalConsumerPrincipal = t.consumerPrincipal,
    )
    val pbf                    = if bankRates.isDefined then Some(buildPerBankFlows(flows, nBanks)) else None
    (updated, correctedAgg, pbf)

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
        hh.skill * (1.0 - p.household.skillDecayRate.toDouble)
      case _                                                                  => hh.skill

  /** Apply health scarring for long-term unemployed (cumulative, capped). */
  private def applyHealthScarring(hh: State, status: HhStatus)(using p: SimParams): Ratio =
    status match
      case HhStatus.Unemployed(months) if months >= p.household.scarringOnset =>
        Ratio(Math.min(p.household.scarringCap.toDouble, hh.healthPenalty.toDouble + p.household.scarringRate.toDouble))
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

  /** Aggregate stats: single-pass accumulation + sorted-array Gini/percentiles.
    */
  def computeAggregates(
      households: Vector[State],
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Double,
      retrainingAttempts: Int,
      retrainingSuccesses: Int,
  )(using p: SimParams): Aggregates =
    val n = households.length

    var nEmployed    = 0
    var nUnemployed  = 0
    var nRetraining  = 0
    var nBankrupt    = 0
    var totalIncome  = 0.0
    var sumSkill     = 0.0
    var sumHealth    = 0.0
    val incomes      = new Array[Double](n)
    val consumptions = new Array[Double](n)
    val savingsArr   = new Array[Double](n)

    var totalRent          = 0.0
    var totalDebtService   = 0.0
    var totalUnempBenefits = 0.0

    // Single pass: collect all per-HH stats + accumulate skill/health
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
          val benefit = computeBenefit(months).toDouble
          incomes(i) = benefit
          totalUnempBenefits += benefit
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

      val rent        = hh.monthlyRent.toDouble
      val debtSvc     = hh.debt.toDouble * p.household.debtServiceRate.toDouble
      val obligations = rent + debtSvc
      val disposable  = Math.max(0.0, incomes(i) - obligations)
      consumptions(i) = disposable * hh.mpc.toDouble
      totalIncome += incomes(i)
      savingsArr(i) = hh.savings.toDouble
      if hh.status != HhStatus.Bankrupt then
        totalRent += rent
        totalDebtService += debtSvc
      i += 1

    val nAlive = n - nBankrupt

    // SFC consistency: rent is domestic consumption (landlord income -> spending),
    // debt service flows to bank (captured via BankState in Simulation.scala).
    val goodsConsumption = consumptions.kahanSum
    val totalConsumption = goodsConsumption + totalRent
    val importCons       = goodsConsumption * Math.min(ImportRatioCap, importAdj)
    val domesticCons     = totalConsumption - importCons

    // Sort each array once -- reuse for Gini + percentiles + poverty
    java.util.Arrays.sort(incomes)
    java.util.Arrays.sort(savingsArr)
    java.util.Arrays.sort(consumptions)

    // Gini coefficients (on pre-sorted arrays)
    val giniIncome = giniSorted(incomes)
    val giniWealth = giniSorted(savingsArr)

    // Savings statistics (savingsArr already sorted)
    val meanSavings   = if n > 0 then savingsArr.kahanSum / n else 0.0
    val medianSavings = if n > 0 then savingsArr(n / 2) else 0.0

    // Poverty rates from sorted incomes -- binary search instead of full scan
    val medianIncome  = if n > 0 then incomes(n / 2) else 0.0
    val povertyRate50 = if n > 0 && medianIncome > 0 then lowerBound(incomes, medianIncome * PovertyRate50Pct).toDouble / n else 0.0
    val povertyRate30 = if n > 0 && medianIncome > 0 then lowerBound(incomes, medianIncome * PovertyRate30Pct).toDouble / n else 0.0

    // Consumption percentiles (consumptions already sorted)
    val consP10 = if n > 0 then consumptions((n * ConsumptionP10).toInt) else 0.0
    val consP50 = if n > 0 then consumptions(n / 2) else 0.0
    val consP90 = if n > 0 then consumptions(Math.min(n - 1, (n * ConsumptionP90).toInt)) else 0.0

    // Skill and health (accumulated in main loop)
    val meanSkill  = if nAlive > 0 then sumSkill / nAlive else 0.0
    val meanHealth = if nAlive > 0 then sumHealth / nAlive else 0.0

    // Bankruptcy rate and mean months to ruin
    val bankruptcyRate   = if n > 0 then nBankrupt.toDouble / n else 0.0
    val meanMonthsToRuin = 0.0 // would require tracking entry time

    Aggregates(
      employed = nEmployed,
      unemployed = nUnemployed,
      retraining = nRetraining,
      bankrupt = nBankrupt,
      totalIncome = PLN(totalIncome),
      consumption = PLN(totalConsumption),
      domesticConsumption = PLN(domesticCons),
      importConsumption = PLN(importCons),
      marketWage = marketWage,
      reservationWage = reservationWage,
      giniIndividual = Ratio(giniIncome),
      giniWealth = Ratio(giniWealth),
      meanSavings = PLN(meanSavings),
      medianSavings = PLN(medianSavings),
      povertyRate50 = Ratio(povertyRate50),
      bankruptcyRate = Ratio(bankruptcyRate),
      meanSkill = meanSkill,
      meanHealthPenalty = meanHealth,
      retrainingAttempts = retrainingAttempts,
      retrainingSuccesses = retrainingSuccesses,
      consumptionP10 = PLN(consP10),
      consumptionP50 = PLN(consP50),
      consumptionP90 = PLN(consP90),
      meanMonthsToRuin = meanMonthsToRuin,
      povertyRate30 = Ratio(povertyRate30),
      totalRent = PLN(totalRent),
      totalDebtService = PLN(totalDebtService),
      totalUnempBenefits = PLN(totalUnempBenefits),
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
