package sfc.agents

import sfc.config.Config
import sfc.engine.World

import scala.util.Random

object HouseholdLogic:

  /** Process one month for all households. Returns updated households + aggregate stats.
    * This is the core individual-mode household step. */
  def step(households: Vector[Household], world: World, bdp: Double,
           marketWage: Double, reservationWage: Double,
           importAdj: Double, rng: Random): (Vector[Household], HhAggregates) =

    var retrainingAttempts = 0
    var retrainingSuccesses = 0

    val updated = households.map { hh =>
      hh.status match
        case HhStatus.Bankrupt => hh  // absorbing barrier
        case _ =>
          val (income, newStatus) = computeIncome(hh, bdp, marketWage, world)
          val obligations = hh.monthlyRent + hh.debt * Config.HhDebtServiceRate
          val disposable = Math.max(0.0, income - obligations)
          val consumption = disposable * hh.mpc

          // Social network precautionary effect
          val neighborDistress = neighborDistressRatio(hh, households)
          val consumptionAdj = if neighborDistress > 0.30 then consumption * 0.90 else consumption

          val newSavings = hh.savings + income - obligations - consumptionAdj
          val newDebt = Math.max(0.0, hh.debt - hh.debt * Config.HhDebtServiceRate)

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
                  retrainingAttempts += 1
                  val targetSector = rng.nextInt(sfc.config.SECTORS.length)
                  (HhStatus.Retraining(Config.HhRetrainingDuration, targetSector,
                    Config.HhRetrainingCost), 1, 0)
                else (newStatus, 0, 0)
              case HhStatus.Retraining(monthsLeft, targetSector, cost) =>
                if monthsLeft <= 1 then
                  // Retraining complete — check success
                  val successProb = Config.HhRetrainingBaseSuccess *
                    afterSkill * (1.0 - afterHealth)
                  if rng.nextDouble() < successProb then
                    retrainingSuccesses += 1
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
    (updated, agg)

  private def computeIncome(hh: Household, bdp: Double, marketWage: Double,
                            world: World): (Double, HhStatus) =
    hh.status match
      case HhStatus.Employed(firmId, sectorIdx, wage) =>
        (wage + bdp, hh.status)  // UBI is universal: employed also receive BDP
      case HhStatus.Unemployed(months) =>
        (bdp, HhStatus.Unemployed(months + 1))
      case HhStatus.Retraining(monthsLeft, target, cost) =>
        (bdp * 0.7, hh.status)  // reduced availability during training
      case HhStatus.Bankrupt =>
        (0.0, HhStatus.Bankrupt)

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

  private def neighborDistressRatio(hh: Household, households: Vector[Household]): Double =
    if hh.socialNeighbors.isEmpty then 0.0
    else
      val distressCount = hh.socialNeighbors.count { nid =>
        if nid >= 0 && nid < households.length then
          households(nid).status match
            case HhStatus.Bankrupt | HhStatus.Unemployed(_) => true
            case _ => false
        else false
      }
      distressCount.toDouble / hh.socialNeighbors.length

  /** Compute aggregate statistics from individual household states.
    * @param bdp current per-capita BDP (for income reconstruction) */
  def computeAggregates(households: Vector[Household], marketWage: Double,
                        reservationWage: Double, importAdj: Double,
                        retrainingAttempts: Int, retrainingSuccesses: Int,
                        bdp: Double = 0.0): HhAggregates =
    val alive = households.filter(_.status != HhStatus.Bankrupt)
    val n = households.length
    val nAlive = alive.length

    var nEmployed = 0
    var nUnemployed = 0
    var nRetraining = 0
    var nBankrupt = 0
    var totalIncome = 0.0
    val incomes = new Array[Double](n)
    val consumptions = new Array[Double](n)
    val savingsArr = new Array[Double](n)
    var totalMonthsToRuin = 0.0
    var bankruptWithMonths = 0

    var totalRent = 0.0
    var totalDebtService = 0.0

    households.zipWithIndex.foreach { (hh, i) =>
      hh.status match
        case HhStatus.Employed(_, _, wage) =>
          nEmployed += 1
          incomes(i) = wage  // wage already includes BDP from computeIncome
        case HhStatus.Unemployed(months) =>
          nUnemployed += 1
          incomes(i) = bdp
        case HhStatus.Retraining(_, _, _) =>
          nRetraining += 1
          incomes(i) = bdp * 0.7
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
      // SFC: track obligations — these are income to other sectors
      if hh.status != HhStatus.Bankrupt then
        totalRent += rent
        totalDebtService += debtSvc
    }

    // SFC consistency: rent is domestic consumption (landlord income → spending),
    // debt service flows to bank (captured via BankState in Simulation.scala).
    // Household consumption on goods + rent recycled as domestic demand.
    val goodsConsumption = consumptions.sum
    val totalConsumption = goodsConsumption + totalRent  // rent recycles as domestic demand
    val importCons = goodsConsumption * Math.min(0.65, importAdj)  // only goods are importable
    val domesticCons = totalConsumption - importCons

    // Gini coefficients
    val giniIncome = gini(incomes.filter(_ >= 0))
    val giniWealth = gini(savingsArr)

    // Savings statistics
    val sortedSavings = savingsArr.sorted
    val meanSavings = if n > 0 then sortedSavings.sum / n else 0.0
    val medianSavings = if n > 0 then sortedSavings(n / 2) else 0.0

    // Poverty rates (relative to median income)
    val sortedIncomes = incomes.sorted
    val medianIncome = if n > 0 then sortedIncomes(n / 2) else 0.0
    val povertyRate50 = if n > 0 && medianIncome > 0 then
      incomes.count(_ < medianIncome * 0.5).toDouble / n else 0.0
    val povertyRate30 = if n > 0 && medianIncome > 0 then
      incomes.count(_ < medianIncome * 0.3).toDouble / n else 0.0

    // Consumption percentiles
    val sortedCons = consumptions.sorted
    val consP10 = if n > 0 then sortedCons((n * 0.10).toInt) else 0.0
    val consP50 = if n > 0 then sortedCons(n / 2) else 0.0
    val consP90 = if n > 0 then sortedCons(Math.min(n - 1, (n * 0.90).toInt)) else 0.0

    // Skill and health
    val meanSkill = if nAlive > 0 then alive.map(_.skill).sum / nAlive else 0.0
    val meanHealth = if nAlive > 0 then alive.map(_.healthPenalty).sum / nAlive else 0.0

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
      totalDebtService = totalDebtService
    )

  /** Gini coefficient for an array of values (handles negatives by shifting). */
  def gini(values: Array[Double]): Double =
    val n = values.length
    if n <= 1 then return 0.0
    val sorted = values.sorted
    // Shift to non-negative if needed
    val minVal = sorted.head
    val shifted = if minVal < 0 then sorted.map(_ - minVal) else sorted
    val total = shifted.sum
    if total <= 0 then return 0.0
    var cumSum = 0.0
    var weightedSum = 0.0
    for i <- 0 until n do
      cumSum += shifted(i)
      weightedSum += (2.0 * (i + 1) - n - 1) * shifted(i)
    weightedSum / (n * total)
