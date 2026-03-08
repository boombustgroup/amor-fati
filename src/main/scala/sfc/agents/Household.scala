package sfc.agents

import sfc.config.Config
import sfc.engine.{SectoralMobility, World}
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

// ---- Top-level types (widely referenced, kept flat) ----

enum HhStatus:
  case Employed(firmId: FirmId, sectorIdx: SectorIdx, wage: PLN)
  case Unemployed(monthsUnemployed: Int)
  case Retraining(monthsLeft: Int, targetSector: SectorIdx, cost: PLN)
  case Bankrupt

/** Per-bank lending and deposit rates for individual HH mode. */
case class BankRates(
  lendingRates: Array[Double], // annual lending rate per bank
  depositRates: Array[Double], // annual deposit rate per bank
)

/** Per-bank HH flow accumulators for multi-bank mode. */
case class PerBankHhFlows(
  income: Array[Double], // per-bank total income (incl. deposit interest)
  consumption: Array[Double], // per-bank total consumption (goods + rent)
  debtService: Array[Double], // per-bank total debt service
  depositInterest: Array[Double], // per-bank total deposit interest paid
  consumerDebtService: Array[Double] = Array.empty, // per-bank consumer debt service
  consumerOrigination: Array[Double] = Array.empty, // per-bank consumer loan origination
  consumerDefault: Array[Double] = Array.empty, // per-bank consumer loan defaults
  consumerPrincipal: Array[Double] = Array.empty, // per-bank consumer loan principal repaid
)

object Household:

  // ---- Aggregate household state (backward-compat, used in both modes) ----

  case class SectorState(
    employed: Int,
    marketWage: PLN,
    reservationWage: PLN,
    totalIncome: PLN,
    consumption: PLN,
    domesticConsumption: PLN,
    importConsumption: PLN,
    minWageLevel: PLN = PLN(4666.0),
    minWagePriceLevel: Double = 1.0,
  ):
    def unemploymentRate: Double = 1.0 - employed.toDouble / Config.TotalPopulation

  // ---- Individual household ----

  case class State(
    id: HhId,
    savings: PLN,
    debt: PLN,
    monthlyRent: PLN,
    skill: Ratio,
    healthPenalty: Ratio,
    mpc: Ratio,
    status: HhStatus,
    socialNeighbors: Array[HhId],
    bankId: BankId = BankId(0), // Multi-bank: index into Banking.State.banks
    equityWealth: PLN = PLN.Zero, // GPW: value of equity holdings
    lastSectorIdx: SectorIdx = SectorIdx(-1), // Sectoral mobility: last sector employed in (-1 = never)
    isImmigrant: Boolean = false, // Immigration: tracks immigrant status for wage discount + remittances
    numDependentChildren: Int = 0, // 800+: children ≤ 18 for social transfers
    consumerDebt: PLN = PLN.Zero, // Consumer credit: outstanding unsecured consumer loan
    education: Int = 2, // Education level: 0=Primary, 1=Vocational, 2=Secondary, 3=Tertiary
  )

  /** Aggregate statistics computed from individual households (Paper-06). */
  case class Aggregates(
    employed: Int,
    unemployed: Int,
    retraining: Int,
    bankrupt: Int,
    totalIncome: PLN,
    consumption: PLN,
    domesticConsumption: PLN,
    importConsumption: PLN,
    marketWage: PLN,
    reservationWage: PLN,
    giniIndividual: Ratio,
    giniWealth: Ratio,
    meanSavings: PLN,
    medianSavings: PLN,
    povertyRate50: Ratio,
    bankruptcyRate: Ratio,
    meanSkill: Double,
    meanHealthPenalty: Double,
    retrainingAttempts: Int,
    retrainingSuccesses: Int,
    consumptionP10: PLN,
    consumptionP50: PLN,
    consumptionP90: PLN,
    meanMonthsToRuin: Double,
    povertyRate30: Ratio,
    totalRent: PLN,
    totalDebtService: PLN,
    totalUnempBenefits: PLN,
    totalDepositInterest: PLN = PLN.Zero,
    crossSectorHires: Int = 0,
    voluntaryQuits: Int = 0,
    sectorMobilityRate: Ratio = Ratio.Zero,
    totalRemittances: PLN = PLN.Zero,
    totalPit: PLN = PLN.Zero,
    totalSocialTransfers: PLN = PLN.Zero,
    totalConsumerDebtService: PLN = PLN.Zero,
    totalConsumerOrigination: PLN = PLN.Zero,
    totalConsumerDefault: PLN = PLN.Zero,
    totalConsumerPrincipal: PLN = PLN.Zero,
  )

  // ---- Init ----

  object Init:
    /** Create individual households with multi-bank assignment. */
    def create(rng: Random, firms: Vector[Firm.State]): Vector[State] =
      import sfc.config.*
      import sfc.networks.Network
      val hhCount = Config.TotalPopulation
      val hhNetwork = Network.wattsStrogatz(hhCount, Config.HhSocialK, Config.HhSocialP)
      val hhs = initialize(hhCount, Config.FirmsCount, firms, hhNetwork, rng)
      // Assign households to same bank as their employer
      hhs.map { h =>
        h.status match
          case HhStatus.Employed(fid, _, _) if fid.toInt < firms.length =>
            h.copy(bankId = firms(fid.toInt).bankId)
          case _ => h
      }

    /** Initialize households, all employed, assigned proportionally to firm sizes. */
    def initialize(
      nHouseholds: Int,
      nFirms: Int,
      firms: Vector[Firm.State],
      socialNetwork: Array[Array[Int]],
      rng: Random,
    ): Vector[State] =
      var hhId = 0
      val builder = Vector.newBuilder[State]

      for f <- firms if Firm.isAlive(f) do
        val nWorkers = Firm.workers(f)
        val sectorIdx = f.sector
        for _ <- 0 until nWorkers do
          if hhId < nHouseholds then
            // Savings: LogNormal(mu, sigma)
            val savings = Math.exp(Config.HhSavingsMu + Config.HhSavingsSigma * rng.nextGaussian())

            // Debt: 40% have debt
            val debt =
              if rng.nextDouble() < Config.HhDebtFraction then
                Math.exp(Config.HhDebtMu + Config.HhDebtSigma * rng.nextGaussian())
              else 0.0

            // Rent: Normal(mean, std), floored
            val rent = Math.max(Config.HhRentFloor, Config.HhRentMean + Config.HhRentStd * rng.nextGaussian())

            // MPC: Beta(alpha, beta) via gamma transformation
            val mpc = betaSample(Config.HhMpcAlpha, Config.HhMpcBeta, rng)

            // Education draw + skill range
            val edu = sfc.config.Config.drawEducation(sectorIdx.toInt, rng)
            val (skillFloor, skillCeiling) = sfc.config.Config.eduSkillRange(edu)
            val sectorSigma = sfc.config.SECTORS(sectorIdx.toInt).sigma
            val baseSkill = skillFloor + (skillCeiling - skillFloor) * rng.nextDouble()
            val sectorBonus = Math.min(0.1, 0.02 * Math.log(sectorSigma))
            val skill = Math.max(skillFloor, Math.min(skillCeiling, baseSkill + sectorBonus))

            val wage = Config.BaseWage * sfc.config.SECTORS(sectorIdx.toInt).wageMultiplier * skill

            // GPW equity wealth: GpwHhEquityFrac of HH participate, with wealth ∝ savings
            val eqWealth =
              if sfc.config.Config.GpwHhEquity && rng.nextDouble() < sfc.config.Config.GpwHhEquityFrac then
                savings * 0.05 // ~5% of savings in equity (NBP household wealth survey)
              else 0.0

            // 800+ children: Poisson(λ) per HH
            val numChildren =
              if sfc.config.Config.Social800Enabled then poissonSample(sfc.config.Config.Social800ChildrenPerHh, rng)
              else 0

            // Consumer credit: 40% of HH have small consumer loans (reuse HhDebtFraction)
            val consDebt =
              if rng.nextDouble() < Config.HhDebtFraction then
                Math.exp(Config.HhDebtMu + Config.HhDebtSigma * rng.nextGaussian()) * 0.3
              else 0.0

            builder += State(
              id = HhId(hhId),
              savings = PLN(savings),
              debt = PLN(debt),
              monthlyRent = PLN(rent),
              skill = Ratio(skill),
              healthPenalty = Ratio.Zero,
              mpc = Ratio(Math.max(0.5, Math.min(0.98, mpc))),
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

    /** Sample from Poisson(lambda) using Knuth algorithm (small λ). */
    private[agents] def poissonSample(lambda: Double, rng: Random): Int =
      if lambda <= 0 then 0
      else
        val L = Math.exp(-lambda)
        var k = 0
        var p = rng.nextDouble()
        while p > L do
          k += 1
          p *= rng.nextDouble()
        k

    /** Sample from Beta(alpha, beta) using two Gamma samples. */
    private def betaSample(alpha: Double, beta: Double, rng: Random): Double =
      val x = gammaSample(alpha, rng)
      val y = gammaSample(beta, rng)
      if x + y > 0 then x / (x + y) else 0.5

    /** Sample from Gamma(shape, 1) using Marsaglia-Tsang method. */
    private def gammaSample(shape: Double, rng: Random): Double =
      if shape < 1.0 then gammaSample(shape + 1.0, rng) * Math.pow(rng.nextDouble(), 1.0 / shape)
      else
        val d = shape - 1.0 / 3.0
        val c = 1.0 / Math.sqrt(9.0 * d)
        var result = 0.0
        var done = false
        while !done do
          var x = rng.nextGaussian()
          var v = 1.0 + c * x
          while v <= 0 do
            x = rng.nextGaussian()
            v = 1.0 + c * x
          v = v * v * v
          val u = rng.nextDouble()
          if u < 1.0 - 0.0331 * x * x * x * x then
            result = d * v
            done = true
          else if Math.log(u) < 0.5 * x * x + d * (1.0 - v + Math.log(v)) then
            result = d * v
            done = true
        result

  // ---- Logic ----

  /** Compute monthly PIT using progressive Polish brackets (2022+). 12% on income ≤ 120k PLN/year, 32% above, minus
    * kwota wolna tax credit. Returns 0 when PIT_ENABLED=false or income ≤ 0.
    */
  def computeMonthlyPit(monthlyIncome: Double): Double =
    if !Config.PitEnabled || monthlyIncome <= 0 then 0.0
    else
      val annualized = monthlyIncome * 12.0
      val grossTax =
        if annualized <= Config.PitBracket1Annual then annualized * Config.PitRate1
        else
          Config.PitBracket1Annual * Config.PitRate1 +
            (annualized - Config.PitBracket1Annual) * Config.PitRate2
      Math.max(0.0, grossTax - Config.PitTaxCreditAnnual) / 12.0

  /** Compute 800+ social transfer (PIT-exempt, lump-sum per child ≤ 18). */
  def computeSocialTransfer(numChildren: Int): Double =
    if !Config.Social800Enabled || numChildren <= 0 then 0.0
    else numChildren.toDouble * Config.Social800Rate

  /** Compute unemployment benefit based on months unemployed. Polish zasilek: 1500 PLN months 1-3, 1200 PLN months 4-6,
    * 0 after.
    */
  def computeBenefit(monthsUnemployed: Int): Double =
    if !Config.GovUnempBenefitEnabled then 0.0
    else if monthsUnemployed <= Config.GovBenefitDuration / 2 then Config.GovBenefitM1to3
    else if monthsUnemployed <= Config.GovBenefitDuration then Config.GovBenefitM4to6
    else 0.0

  /** Process one month for all households. Returns updated households + aggregate stats + optional per-bank flows. This
    * is the core individual-mode household step. When bankRates is provided, uses variable-rate debt service and
    * deposit interest.
    * @param equityIndexReturn
    *   monthly return on equity index (for revaluation)
    */
  def step(
    households: Vector[State],
    world: World,
    bdp: Double,
    marketWage: Double,
    reservationWage: Double,
    importAdj: Double,
    rng: Random,
    nBanks: Int = 1,
    bankRates: Option[BankRates] = None,
    equityIndexReturn: Double = 0.0,
    sectorWages: Option[Array[Double]] = None,
    sectorVacancies: Option[Array[Int]] = None,
  ): (Vector[State], Aggregates, Option[PerBankHhFlows]) =

    var retrainingAttempts = 0
    var retrainingSuccesses = 0
    var voluntaryQuits = 0
    var actualTotalIncome = 0.0
    var totalUnempBenefits = 0.0
    var actualTotalDebtService = 0.0
    var actualTotalDepositInterest = 0.0
    var actualGoodsConsumption = 0.0
    var actualTotalRent = 0.0
    var totalEquityWealth = 0.0
    var totalWealthEffect = 0.0
    var totalRemittances = 0.0
    var totalPit = 0.0
    var totalSocialTransfers = 0.0
    var totalConsumerDebtService = 0.0
    var totalConsumerOrigination = 0.0
    var totalConsumerDefault = 0.0

    // Per-bank flow accumulators (only when bankRates provided)
    val perBankArrays = bankRates.map { _ =>
      (
        new Array[Double](nBanks), // inc
        new Array[Double](nBanks), // cons
        new Array[Double](nBanks), // dSvc
        new Array[Double](nBanks), // depInt
        new Array[Double](nBanks), // ccDSvc
        new Array[Double](nBanks), // ccOrig
        new Array[Double](nBanks), // ccDef
        new Array[Double](nBanks), // ccPrin
      )
    }
    var totalConsumerPrincipal = 0.0

    // Pre-compute distressed HH set: O(N_hh) instead of O(N_hh x k) per-HH lookup
    val distressedIds = new java.util.BitSet(households.length)
    var idx = 0
    while idx < households.length do
      households(idx).status match
        case HhStatus.Bankrupt | HhStatus.Unemployed(_) => distressedIds.set(idx)
        case _                                          =>
      idx += 1

    val updated = households.map { hh =>
      hh.status match
        case HhStatus.Bankrupt => hh // absorbing barrier
        case _                 =>
          val (baseIncome, benefit, newStatus) = computeIncome(hh, bdp)

          // Variable-rate debt service (monetary transmission channel 1)
          val debtServiceRate = bankRates match
            case Some(br) => Config.HhBaseAmortRate + br.lendingRates(hh.bankId.toInt) / 12.0
            case None     => Config.HhDebtServiceRate

          // Deposit interest (monetary transmission channel 2)
          val depInterest = bankRates match
            case Some(br) => br.depositRates(hh.bankId.toInt) / 12.0 * hh.savings.toDouble
            case None     => 0.0

          val grossIncome =
            baseIncome + Math.max(0.0, depInterest) // floor at 0 (no negative interest on negative savings)
          val pitTax = computeMonthlyPit(grossIncome)
          totalPit += pitTax
          val socialTransfer = computeSocialTransfer(hh.numDependentChildren) // PIT-exempt
          totalSocialTransfers += socialTransfer
          val income = grossIncome - pitTax + socialTransfer
          actualTotalIncome += income
          totalUnempBenefits += benefit
          val thisDebtService = hh.debt.toDouble * debtServiceRate
          actualTotalDebtService += thisDebtService
          actualTotalDepositInterest += Math.max(0.0, depInterest)

          // Consumer credit debt service
          val consumerRate = bankRates match
            case Some(br) => br.lendingRates(hh.bankId.toInt) + Config.CcSpread
            case None     => world.nbp.referenceRate.toDouble + Config.CcSpread
          val consumerDebtSvc = hh.consumerDebt.toDouble * (Config.CcAmortRate + consumerRate / 12.0)
          totalConsumerDebtService += consumerDebtSvc
          val consumerPrin = hh.consumerDebt.toDouble * Config.CcAmortRate
          totalConsumerPrincipal += consumerPrin

          // Immigrant remittance: fraction of net-of-PIT income sent abroad (deposit outflow)
          val remittance =
            if hh.isImmigrant && Config.ImmigEnabled then income * Config.ImmigRemittanceRate
            else 0.0
          totalRemittances += remittance

          val obligations = hh.monthlyRent.toDouble + thisDebtService + consumerDebtSvc + remittance
          val disposable = Math.max(0.0, income - obligations)

          // Consumer credit origination: employed HH with low disposable income can borrow
          val newConsumerLoan = hh.status match
            case HhStatus.Employed(_, _, wage)
                if disposable < wage.toDouble * 0.3 && rng.nextDouble() < Config.CcEligRate =>
              val existingDti = (thisDebtService + consumerDebtSvc) / Math.max(1.0, income)
              val headroom = Math.max(0.0, Config.CcMaxDti - existingDti) * income
              val desired = Math.min(headroom, Config.CcMaxLoan)
              if desired > 100.0 then desired else 0.0
            case _ => 0.0
          totalConsumerOrigination += newConsumerLoan

          val consumption = (disposable + newConsumerLoan) * hh.mpc.toDouble

          // Social network precautionary effect (uses pre-computed distress set)
          val neighborDistress = neighborDistressRatioFast(hh, distressedIds)
          val consumptionAdj = if neighborDistress > 0.30 then consumption * 0.90 else consumption

          // GPW equity wealth effect
          val newEquityWealth = Math.max(0.0, hh.equityWealth.toDouble * (1.0 + equityIndexReturn))
          val equityGain = newEquityWealth - hh.equityWealth.toDouble
          val wealthEffectBoost =
            if Config.GpwHhEquity && equityGain > 0 then equityGain * Config.GpwWealthEffectMpc
            else 0.0
          val consumptionWithWealth = consumptionAdj + wealthEffectBoost
          totalEquityWealth += newEquityWealth
          totalWealthEffect += wealthEffectBoost

          val newSavings = hh.savings.toDouble + income - obligations + newConsumerLoan - consumptionWithWealth
          val newDebt = Math.max(0.0, hh.debt.toDouble - thisDebtService)
          val newConsumerDebt = Math.max(0.0, hh.consumerDebt.toDouble + newConsumerLoan - consumerDebtSvc)

          // Accumulate actual consumption flows (used for SFC consistency)
          actualGoodsConsumption += consumptionWithWealth
          actualTotalRent += hh.monthlyRent.toDouble

          // Per-bank accumulation
          perBankArrays.foreach { case (pbInc, pbCons, pbDSvc, pbDepInt, pbCcDSvc, pbCcOrig, _, pbCcPrin) =>
            val bId = hh.bankId.toInt
            pbInc(bId) += income
            pbCons(bId) += consumptionWithWealth + hh.monthlyRent.toDouble
            pbDSvc(bId) += thisDebtService
            pbDepInt(bId) += Math.max(0.0, depInterest)
            pbCcDSvc(bId) += consumerDebtSvc
            pbCcOrig(bId) += newConsumerLoan
            pbCcPrin(bId) += consumerPrin
          }

          // Bankruptcy test
          if newSavings < Config.HhBankruptcyThreshold * hh.monthlyRent.toDouble then
            // Default amount: remaining consumer debt after this month's principal repayment + new loan
            val ccDefaultAmt = hh.consumerDebt.toDouble * (1.0 - Config.CcAmortRate) + newConsumerLoan
            totalConsumerDefault += ccDefaultAmt
            perBankArrays.foreach { case (_, _, _, _, _, _, pbCcDef, _) =>
              pbCcDef(hh.bankId.toInt) += ccDefaultAmt
            }
            hh.copy(
              savings = PLN(newSavings),
              debt = PLN(newDebt),
              consumerDebt = PLN.Zero,
              status = HhStatus.Bankrupt,
              equityWealth = PLN.Zero,
            )
          else
            val afterSkill = applySkillDecay(hh, newStatus)
            val afterHealth = applyHealthScarring(hh, newStatus)

            // Voluntary cross-sector search (employed workers only, sectoral mobility ON)
            val (afterVoluntary, vQuit) = newStatus match
              case HhStatus.Employed(firmId, sectorIdx, wage)
                  if Config.LmSectoralMobility && sectorWages.isDefined &&
                    rng.nextDouble() < Config.LmVoluntarySearchProb =>
                val sw = sectorWages.get
                val sv = sectorVacancies.get
                val targetSector = SectoralMobility.selectTargetSector(
                  sectorIdx.toInt,
                  sw,
                  sv,
                  Config.LmFrictionMatrix,
                  Config.LmVacancyWeight,
                  rng,
                )
                val targetAvgWage = sw(targetSector)
                if targetAvgWage > wage.toDouble * (1.0 + Config.LmVoluntaryWageThreshold) then
                  val friction = Config.LmFrictionMatrix(sectorIdx.toInt)(targetSector)
                  if friction < Config.LmAdjacentFrictionMax then
                    // Low friction: direct switch (1-month unemployment gap)
                    (HhStatus.Unemployed(0), 1)
                  else
                    // High friction: enter retraining
                    val (adjDur, adjCost) = SectoralMobility.frictionAdjustedParams(
                      friction,
                      Config.LmFrictionDurationMult,
                      Config.LmFrictionCostMult,
                    )
                    if hh.savings.toDouble > adjCost then
                      (HhStatus.Retraining(adjDur, SectorIdx(targetSector), PLN(adjCost)), 1)
                    else (newStatus, 0) // can't afford retraining
                else (newStatus, 0) // target wage not attractive enough
              case _ => (newStatus, 0)
            voluntaryQuits += vQuit

            // Retraining decision (for unemployed, friction-aware when LM enabled)
            val (finalStatus, rAttempt, rSuccess) = afterVoluntary match
              case HhStatus.Unemployed(months) if months > 6 && Config.HhRetrainingEnabled =>
                val retrainProb = Config.HhRetrainingProb +
                  (if neighborDistress > 0.30 then 0.05 else 0.0)
                if hh.savings.toDouble > Config.HhRetrainingCost && rng.nextDouble() < retrainProb then
                  if Config.LmSectoralMobility && sectorWages.isDefined then
                    // Friction-aware target selection
                    val sw = sectorWages.get
                    val sv = sectorVacancies.get
                    val fromSector = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
                    val targetSector = SectoralMobility.selectTargetSector(
                      fromSector,
                      sw,
                      sv,
                      Config.LmFrictionMatrix,
                      Config.LmVacancyWeight,
                      rng,
                    )
                    val friction = Config.LmFrictionMatrix(fromSector)(targetSector)
                    val (adjDur, adjCost) = SectoralMobility.frictionAdjustedParams(
                      friction,
                      Config.LmFrictionDurationMult,
                      Config.LmFrictionCostMult,
                    )
                    if hh.savings.toDouble > adjCost then
                      (HhStatus.Retraining(adjDur, SectorIdx(targetSector), PLN(adjCost)), 1, 0)
                    else (afterVoluntary, 0, 0)
                  else
                    val targetSector = rng.nextInt(sfc.config.SECTORS.length)
                    (
                      HhStatus.Retraining(
                        Config.HhRetrainingDuration,
                        SectorIdx(targetSector),
                        PLN(Config.HhRetrainingCost),
                      ),
                      1,
                      0,
                    )
                else (afterVoluntary, 0, 0)
              case HhStatus.Retraining(monthsLeft, targetSector, cost) =>
                if monthsLeft <= 1 then
                  // Retraining complete -- check success (friction-adjusted)
                  val baseSuccessProb = Config.HhRetrainingBaseSuccess *
                    afterSkill * (1.0 - afterHealth) * Config.eduRetrainMultiplier(hh.education)
                  val successProb = if Config.LmSectoralMobility then
                    val fromSector = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt else 0
                    val friction = Config.LmFrictionMatrix(fromSector)(targetSector.toInt)
                    SectoralMobility.frictionAdjustedSuccess(baseSuccessProb, friction)
                  else baseSuccessProb
                  if rng.nextDouble() < successProb then
                    // Success: reset skill, become unemployed (ready for job search)
                    (HhStatus.Unemployed(0), 0, 1)
                  else
                    // Failure: still unemployed, skill not reset
                    (HhStatus.Unemployed(7), 0, 0) // 7 = 6 months training + 1
                else (HhStatus.Retraining(monthsLeft - 1, targetSector, cost), 0, 0)
              case _ => (afterVoluntary, 0, 0)

            retrainingAttempts += rAttempt
            retrainingSuccesses += rSuccess

            val retrainingCostThisMonth = finalStatus match
              case HhStatus.Retraining(ml, _, cost) if ml == Config.HhRetrainingDuration - 1 =>
                cost.toDouble // pay upfront in first month
              case _ => 0.0

            hh.copy(
              savings = PLN(newSavings - retrainingCostThisMonth),
              debt = PLN(newDebt),
              consumerDebt = PLN(newConsumerDebt),
              skill = Ratio(afterSkill),
              healthPenalty = Ratio(afterHealth),
              mpc = hh.mpc,
              status = finalStatus,
              equityWealth = PLN(newEquityWealth),
            )
    }

    val agg =
      computeAggregates(updated, marketWage, reservationWage, importAdj, retrainingAttempts, retrainingSuccesses)
    val actualTotalConsumption = actualGoodsConsumption + actualTotalRent
    val actualImportCons = actualGoodsConsumption * Math.min(0.65, importAdj)
    val actualDomesticCons = actualTotalConsumption - actualImportCons
    // Sector mobility rate: fraction of employed in different sector than lastSectorIdx
    val smRate = if Config.LmSectoralMobility then
      val employed = updated.filter(_.status.isInstanceOf[HhStatus.Employed])
      if employed.nonEmpty then
        employed.count { hh =>
          val sec = hh.status.asInstanceOf[HhStatus.Employed].sectorIdx
          hh.lastSectorIdx.toInt >= 0 && hh.lastSectorIdx != sec
        }.toDouble / employed.length
      else 0.0
    else 0.0
    val correctedAgg = agg.copy(
      totalIncome = PLN(actualTotalIncome),
      consumption = PLN(actualTotalConsumption),
      importConsumption = PLN(actualImportCons),
      domesticConsumption = PLN(actualDomesticCons),
      totalUnempBenefits = PLN(totalUnempBenefits),
      totalDebtService = PLN(actualTotalDebtService),
      totalDepositInterest = PLN(actualTotalDepositInterest),
      totalRent = PLN(actualTotalRent),
      voluntaryQuits = voluntaryQuits,
      sectorMobilityRate = Ratio(smRate),
      totalRemittances = PLN(totalRemittances),
      totalPit = PLN(totalPit),
      totalSocialTransfers = PLN(totalSocialTransfers),
      totalConsumerDebtService = PLN(totalConsumerDebtService),
      totalConsumerOrigination = PLN(totalConsumerOrigination),
      totalConsumerDefault = PLN(totalConsumerDefault),
      totalConsumerPrincipal = PLN(totalConsumerPrincipal),
    )
    val pbf = perBankArrays.map { case (pbInc, pbCons, pbDSvc, pbDepInt, pbCcDSvc, pbCcOrig, pbCcDef, pbCcPrin) =>
      PerBankHhFlows(pbInc, pbCons, pbDSvc, pbDepInt, pbCcDSvc, pbCcOrig, pbCcDef, pbCcPrin)
    }
    (updated, correctedAgg, pbf)

  private def computeIncome(hh: State, bdp: Double): (Double, Double, HhStatus) =
    hh.status match
      case HhStatus.Employed(firmId, sectorIdx, wage) =>
        (wage.toDouble + bdp, 0.0, hh.status) // UBI is universal: employed also receive BDP
      case HhStatus.Unemployed(months) =>
        val benefit = computeBenefit(months)
        (bdp + benefit, benefit, HhStatus.Unemployed(months + 1))
      case HhStatus.Retraining(monthsLeft, target, cost) =>
        (bdp * 0.7, 0.0, hh.status) // reduced availability during training
      case HhStatus.Bankrupt =>
        (0.0, 0.0, HhStatus.Bankrupt)

  private def applySkillDecay(hh: State, status: HhStatus): Double =
    status match
      case HhStatus.Unemployed(months) if months >= Config.HhScarringOnset =>
        hh.skill.toDouble * (1.0 - Config.HhSkillDecayRate)
      case _ => hh.skill.toDouble

  private def applyHealthScarring(hh: State, status: HhStatus): Double =
    status match
      case HhStatus.Unemployed(months) if months >= Config.HhScarringOnset =>
        Math.min(Config.HhScarringCap, hh.healthPenalty.toDouble + Config.HhScarringRate)
      case _ => hh.healthPenalty.toDouble

  private def neighborDistressRatioFast(hh: State, distressedIds: java.util.BitSet): Double =
    if hh.socialNeighbors.isEmpty then 0.0
    else
      var count = 0
      var i = 0
      while i < hh.socialNeighbors.length do
        if distressedIds.get(hh.socialNeighbors(i).toInt) then count += 1
        i += 1
      count.toDouble / hh.socialNeighbors.length

  /** Compute aggregate statistics from individual household states.
    * @param bdp
    *   current per-capita BDP (for income reconstruction)
    */
  def computeAggregates(
    households: Vector[State],
    marketWage: Double,
    reservationWage: Double,
    importAdj: Double,
    retrainingAttempts: Int,
    retrainingSuccesses: Int,
    bdp: Double = 0.0,
  ): Aggregates =
    val n = households.length

    var nEmployed = 0
    var nUnemployed = 0
    var nRetraining = 0
    var nBankrupt = 0
    var totalIncome = 0.0
    var sumSkill = 0.0
    var sumHealth = 0.0
    val incomes = new Array[Double](n)
    val consumptions = new Array[Double](n)
    val savingsArr = new Array[Double](n)

    var totalRent = 0.0
    var totalDebtService = 0.0
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
        case HhStatus.Unemployed(months) =>
          nUnemployed += 1
          val benefit = computeBenefit(months)
          incomes(i) = bdp + benefit
          totalUnempBenefits += benefit
          sumSkill += hh.skill.toDouble
          sumHealth += hh.healthPenalty.toDouble
        case HhStatus.Retraining(_, _, _) =>
          nRetraining += 1
          incomes(i) = bdp * 0.7
          sumSkill += hh.skill.toDouble
          sumHealth += hh.healthPenalty.toDouble
        case HhStatus.Bankrupt =>
          nBankrupt += 1
          incomes(i) = 0.0

      val rent = hh.monthlyRent.toDouble
      val debtSvc = hh.debt.toDouble * Config.HhDebtServiceRate
      val obligations = rent + debtSvc
      val disposable = Math.max(0.0, incomes(i) - obligations)
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
    val importCons = goodsConsumption * Math.min(0.65, importAdj)
    val domesticCons = totalConsumption - importCons

    // Sort each array once -- reuse for Gini + percentiles + poverty
    java.util.Arrays.sort(incomes)
    java.util.Arrays.sort(savingsArr)
    java.util.Arrays.sort(consumptions)

    // Gini coefficients (on pre-sorted arrays)
    val giniIncome = giniSorted(incomes)
    val giniWealth = giniSorted(savingsArr)

    // Savings statistics (savingsArr already sorted)
    val meanSavings = if n > 0 then savingsArr.kahanSum / n else 0.0
    val medianSavings = if n > 0 then savingsArr(n / 2) else 0.0

    // Poverty rates from sorted incomes -- binary search instead of full scan
    val medianIncome = if n > 0 then incomes(n / 2) else 0.0
    val povertyRate50 = if n > 0 && medianIncome > 0 then lowerBound(incomes, medianIncome * 0.5).toDouble / n else 0.0
    val povertyRate30 = if n > 0 && medianIncome > 0 then lowerBound(incomes, medianIncome * 0.3).toDouble / n else 0.0

    // Consumption percentiles (consumptions already sorted)
    val consP10 = if n > 0 then consumptions((n * 0.10).toInt) else 0.0
    val consP50 = if n > 0 then consumptions(n / 2) else 0.0
    val consP90 = if n > 0 then consumptions(Math.min(n - 1, (n * 0.90).toInt)) else 0.0

    // Skill and health (accumulated in main loop)
    val meanSkill = if nAlive > 0 then sumSkill / nAlive else 0.0
    val meanHealth = if nAlive > 0 then sumHealth / nAlive else 0.0

    // Bankruptcy rate and mean months to ruin
    val bankruptcyRate = if n > 0 then nBankrupt.toDouble / n else 0.0
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
      marketWage = PLN(marketWage),
      reservationWage = PLN(reservationWage),
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

  /** Gini coefficient for a pre-sorted array (handles negatives by shifting). */
  def giniSorted(sorted: Array[Double]): Double =
    val n = sorted.length
    if n <= 1 then return 0.0
    val minVal = sorted(0)
    val shift = if minVal < 0 then -minVal else 0.0
    var total = 0.0
    var weightedSum = 0.0
    var i = 0
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
