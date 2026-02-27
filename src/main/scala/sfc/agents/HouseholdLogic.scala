package sfc.agents

import sfc.config.Config
import sfc.engine.World

import scala.util.Random

/** Per-bank lending and deposit rates for individual HH mode. */
case class BankRates(
  lendingRates: Array[Double],  // annual lending rate per bank
  depositRates: Array[Double]   // annual deposit rate per bank
)

/** Per-bank HH flow accumulators for multi-bank mode. */
case class PerBankHhFlows(
  income: Array[Double],          // per-bank total income (incl. deposit interest)
  consumption: Array[Double],     // per-bank total consumption (goods + rent)
  debtService: Array[Double],     // per-bank total debt service
  depositInterest: Array[Double]  // per-bank total deposit interest paid
)

object HouseholdLogic:

  /** Compute unemployment benefit based on months unemployed.
    * Polish zasilek: 1500 PLN months 1-3, 1200 PLN months 4-6, 0 after. */
  def computeBenefit(monthsUnemployed: Int): Double =
    if !Config.GovUnempBenefitEnabled then 0.0
    else if monthsUnemployed <= Config.GovBenefitDuration / 2 then Config.GovBenefitM1to3
    else if monthsUnemployed <= Config.GovBenefitDuration then Config.GovBenefitM4to6
    else 0.0

  /** Process one month for all households. Returns updated households + aggregate stats + optional per-bank flows.
    * This is the core individual-mode household step.
    * When bankRates is provided, uses variable-rate debt service and deposit interest. */
  def step(households: Vector[Household], world: World, bdp: Double,
           marketWage: Double, reservationWage: Double,
           importAdj: Double, rng: Random,
           nBanks: Int = 1,
           bankRates: Option[BankRates] = None
  ): (Vector[Household], HhAggregates, Option[PerBankHhFlows]) =

    var retrainingAttempts = 0
    var retrainingSuccesses = 0
    var actualTotalIncome = 0.0
    var totalUnempBenefits = 0.0
    var actualTotalDebtService = 0.0
    var actualTotalDepositInterest = 0.0

    // Per-bank flow accumulators (only when bankRates provided)
    val br = bankRates.orNull
    val perBankInc    = if br != null then new Array[Double](nBanks) else null
    val perBankCons   = if br != null then new Array[Double](nBanks) else null
    val perBankDSvc   = if br != null then new Array[Double](nBanks) else null
    val perBankDepInt = if br != null then new Array[Double](nBanks) else null

    // Pre-compute distressed HH set: O(N_hh) instead of O(N_hh x k) per-HH lookup
    val distressedIds = new java.util.BitSet(households.length)
    var idx = 0
    while idx < households.length do
      households(idx).status match
        case HhStatus.Bankrupt | HhStatus.Unemployed(_) => distressedIds.set(idx)
        case _ =>
      idx += 1

    val updated = households.map { hh =>
      hh.status match
        case HhStatus.Bankrupt => hh  // absorbing barrier
        case _ =>
          val (baseIncome, benefit, newStatus) = computeIncome(hh, bdp, marketWage, world)

          // Variable-rate debt service (monetary transmission channel 1)
          val debtServiceRate = if br != null then
            Config.HhBaseAmortRate + br.lendingRates(hh.bankId) / 12.0
          else Config.HhDebtServiceRate

          // Deposit interest (monetary transmission channel 2)
          val depInterest = if br != null then
            br.depositRates(hh.bankId) / 12.0 * hh.savings
          else 0.0

          val income = baseIncome + Math.max(0.0, depInterest)  // floor at 0 (no negative interest on negative savings)
          actualTotalIncome += income
          totalUnempBenefits += benefit
          val thisDebtService = hh.debt * debtServiceRate
          actualTotalDebtService += thisDebtService
          actualTotalDepositInterest += Math.max(0.0, depInterest)

          val obligations = hh.monthlyRent + thisDebtService
          val disposable = Math.max(0.0, income - obligations)
          val consumption = disposable * hh.mpc

          // Social network precautionary effect (uses pre-computed distress set)
          val neighborDistress = neighborDistressRatioFast(hh, distressedIds)
          val consumptionAdj = if neighborDistress > 0.30 then consumption * 0.90 else consumption

          val newSavings = hh.savings + income - obligations - consumptionAdj
          val newDebt = Math.max(0.0, hh.debt - thisDebtService)

          // Per-bank accumulation
          if perBankInc != null then
            val bId = hh.bankId
            perBankInc(bId) += income
            perBankCons(bId) += consumptionAdj + hh.monthlyRent
            perBankDSvc(bId) += thisDebtService
            perBankDepInt(bId) += Math.max(0.0, depInterest)

          // Bankruptcy test
          if newSavings < Config.HhBankruptcyThreshold * hh.monthlyRent then
            hh.copy(savings = newSavings, debt = newDebt, status = HhStatus.Bankrupt)
          else
            val afterSkill = applySkillDecay(hh, newStatus)
            val afterHealth = applyHealthScarring(hh, newStatus)

            // Retraining decision
            val (finalStatus, rAttempt, rSuccess) = newStatus match
              case HhStatus.Unemployed(months) if months > 6 && Config.HhRetrainingEnabled =>
                val retrainProb = Config.HhRetrainingProb +
                  (if neighborDistress > 0.30 then 0.05 else 0.0)
                if hh.savings > Config.HhRetrainingCost && rng.nextDouble() < retrainProb then
                  val targetSector = rng.nextInt(sfc.config.SECTORS.length)
                  (HhStatus.Retraining(Config.HhRetrainingDuration, targetSector,
                    Config.HhRetrainingCost), 1, 0)
                else (newStatus, 0, 0)
              case HhStatus.Retraining(monthsLeft, targetSector, cost) =>
                if monthsLeft <= 1 then
                  // Retraining complete -- check success
                  val successProb = Config.HhRetrainingBaseSuccess *
                    afterSkill * (1.0 - afterHealth)
                  if rng.nextDouble() < successProb then
                    // Success: reset skill, become unemployed (ready for job search)
                    (HhStatus.Unemployed(0), 0, 1)
                  else
                    // Failure: still unemployed, skill not reset
                    (HhStatus.Unemployed(7), 0, 0)  // 7 = 6 months training + 1
                else
                  (HhStatus.Retraining(monthsLeft - 1, targetSector, cost), 0, 0)
              case _ => (newStatus, 0, 0)

            retrainingAttempts += rAttempt
            retrainingSuccesses += rSuccess

            val retrainingCostThisMonth = finalStatus match
              case HhStatus.Retraining(ml, _, cost) if ml == Config.HhRetrainingDuration - 1 =>
                cost  // pay upfront in first month
              case _ => 0.0

            hh.copy(
              savings = newSavings - retrainingCostThisMonth,
              debt = newDebt,
              skill = afterSkill,
              healthPenalty = afterHealth,
              mpc = hh.mpc,
              status = finalStatus
            )
    }

    val agg = computeAggregates(updated, marketWage, reservationWage, importAdj,
      retrainingAttempts, retrainingSuccesses)
    val correctedAgg = agg.copy(
      totalIncome = actualTotalIncome,
      totalUnempBenefits = totalUnempBenefits,
      totalDebtService = actualTotalDebtService,
      totalDepositInterest = actualTotalDepositInterest
    )
    val pbf = if perBankInc != null then
      Some(PerBankHhFlows(perBankInc, perBankCons, perBankDSvc, perBankDepInt))
    else None
    (updated, correctedAgg, pbf)

  private def computeIncome(hh: Household, bdp: Double, marketWage: Double,
                            world: World): (Double, Double, HhStatus) =
    hh.status match
      case HhStatus.Employed(firmId, sectorIdx, wage) =>
        (wage + bdp, 0.0, hh.status)  // UBI is universal: employed also receive BDP
      case HhStatus.Unemployed(months) =>
        val benefit = computeBenefit(months)
        (bdp + benefit, benefit, HhStatus.Unemployed(months + 1))
      case HhStatus.Retraining(monthsLeft, target, cost) =>
        (bdp * 0.7, 0.0, hh.status)  // reduced availability during training
      case HhStatus.Bankrupt =>
        (0.0, 0.0, HhStatus.Bankrupt)

  private def applySkillDecay(hh: Household, status: HhStatus): Double =
    status match
      case HhStatus.Unemployed(months) if months >= Config.HhScarringOnset =>
        hh.skill * (1.0 - Config.HhSkillDecayRate)
      case _ => hh.skill

  private def applyHealthScarring(hh: Household, status: HhStatus): Double =
    status match
      case HhStatus.Unemployed(months) if months >= Config.HhScarringOnset =>
        Math.min(Config.HhScarringCap, hh.healthPenalty + Config.HhScarringRate)
      case _ => hh.healthPenalty

  private def neighborDistressRatioFast(hh: Household, distressedIds: java.util.BitSet): Double =
    if hh.socialNeighbors.isEmpty then 0.0
    else
      var count = 0
      var i = 0
      while i < hh.socialNeighbors.length do
        if distressedIds.get(hh.socialNeighbors(i)) then count += 1
        i += 1
      count.toDouble / hh.socialNeighbors.length

  /** Compute aggregate statistics from individual household states.
    * @param bdp current per-capita BDP (for income reconstruction) */
  def computeAggregates(households: Vector[Household], marketWage: Double,
                        reservationWage: Double, importAdj: Double,
                        retrainingAttempts: Int, retrainingSuccesses: Int,
                        bdp: Double = 0.0): HhAggregates =
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
          incomes(i) = wage
          sumSkill += hh.skill
          sumHealth += hh.healthPenalty
        case HhStatus.Unemployed(months) =>
          nUnemployed += 1
          val benefit = computeBenefit(months)
          incomes(i) = bdp + benefit
          totalUnempBenefits += benefit
          sumSkill += hh.skill
          sumHealth += hh.healthPenalty
        case HhStatus.Retraining(_, _, _) =>
          nRetraining += 1
          incomes(i) = bdp * 0.7
          sumSkill += hh.skill
          sumHealth += hh.healthPenalty
        case HhStatus.Bankrupt =>
          nBankrupt += 1
          incomes(i) = 0.0

      val rent = hh.monthlyRent
      val debtSvc = hh.debt * Config.HhDebtServiceRate
      val obligations = rent + debtSvc
      val disposable = Math.max(0.0, incomes(i) - obligations)
      consumptions(i) = disposable * hh.mpc
      totalIncome += incomes(i)
      savingsArr(i) = hh.savings
      if hh.status != HhStatus.Bankrupt then
        totalRent += rent
        totalDebtService += debtSvc
      i += 1

    val nAlive = n - nBankrupt

    // SFC consistency: rent is domestic consumption (landlord income -> spending),
    // debt service flows to bank (captured via BankState in Simulation.scala).
    val goodsConsumption = consumptions.sum
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
    val meanSavings = if n > 0 then savingsArr.sum / n else 0.0
    val medianSavings = if n > 0 then savingsArr(n / 2) else 0.0

    // Poverty rates from sorted incomes -- binary search instead of full scan
    val medianIncome = if n > 0 then incomes(n / 2) else 0.0
    val povertyRate50 = if n > 0 && medianIncome > 0 then
      lowerBound(incomes, medianIncome * 0.5).toDouble / n else 0.0
    val povertyRate30 = if n > 0 && medianIncome > 0 then
      lowerBound(incomes, medianIncome * 0.3).toDouble / n else 0.0

    // Consumption percentiles (consumptions already sorted)
    val consP10 = if n > 0 then consumptions((n * 0.10).toInt) else 0.0
    val consP50 = if n > 0 then consumptions(n / 2) else 0.0
    val consP90 = if n > 0 then consumptions(Math.min(n - 1, (n * 0.90).toInt)) else 0.0

    // Skill and health (accumulated in main loop)
    val meanSkill = if nAlive > 0 then sumSkill / nAlive else 0.0
    val meanHealth = if nAlive > 0 then sumHealth / nAlive else 0.0

    // Bankruptcy rate and mean months to ruin
    val bankruptcyRate = if n > 0 then nBankrupt.toDouble / n else 0.0
    val meanMonthsToRuin = 0.0  // would require tracking entry time

    HhAggregates(
      employed = nEmployed,
      unemployed = nUnemployed,
      retraining = nRetraining,
      bankrupt = nBankrupt,
      totalIncome = totalIncome,
      consumption = totalConsumption,
      domesticConsumption = domesticCons,
      importConsumption = importCons,
      marketWage = marketWage,
      reservationWage = reservationWage,
      giniIndividual = giniIncome,
      giniWealth = giniWealth,
      meanSavings = meanSavings,
      medianSavings = medianSavings,
      povertyRate50 = povertyRate50,
      bankruptcyRate = bankruptcyRate,
      meanSkill = meanSkill,
      meanHealthPenalty = meanHealth,
      retrainingAttempts = retrainingAttempts,
      retrainingSuccesses = retrainingSuccesses,
      consumptionP10 = consP10,
      consumptionP50 = consP50,
      consumptionP90 = consP90,
      meanMonthsToRuin = meanMonthsToRuin,
      povertyRate30 = povertyRate30,
      totalRent = totalRent,
      totalDebtService = totalDebtService,
      totalUnempBenefits = totalUnempBenefits
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
